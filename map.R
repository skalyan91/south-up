library(groundhog)

date <- "2023-07-17"

groundhog_dir <- paste0("groundhog_libraries_", date)

if(!dir.exists(groundhog_dir)){
  dir.create(groundhog_dir)
  }

set.groundhog.folder(groundhog_dir)

groundhog_pkgs <- c("elevatr", 
                    "ggnewscale", 
                    "mapdata", 
                    "maptools", 
                    "raster", 
                    "rgeos", 
                    "rmapshaper", 
                    "sf", 
                    "tidyverse")

groundhog.library(groundhog_pkgs, "2023-07-17")

world <- map_data("world", wrap = c(-30,330))
lakes <- map_data("lakes", wrap = c(-30,330))

elevation <- get_elev_raster(tibble(long = c(-180, 180, -180, 180),
                                    lat = c(-85, -85, 85, 85)),
                             z = 3, prj = "+proj=longlat",
                             verbose = F)
slope <- terrain(elevation, opt = "slope")
aspect <- terrain(elevation, opt = "aspect")
hill <- hillShade(slope, aspect, angle = 45, direction = -30)
elev_df <- rasterToPoints(elevation) %>% 
  as_tibble() %>% 
  `colnames<-`(c("long", "lat", "elevation")) %>% 
  mutate(long = if_else(long < -30, long + 360, long))
slope_df <- rasterToPoints(slope) %>% 
  as_tibble() %>% 
  `colnames<-`(c("long", "lat", "slope")) %>% 
  mutate(long = if_else(long < -30, long + 360, long),
         slope = if_else(slope > 0.4, 0, slope))
hill_df <- rasterToPoints(hill) %>% 
  as_tibble() %>% 
  `colnames<-`(c("long", "lat", "shade")) %>% 
  mutate(long = if_else(long < -30, long + 360, long),
         shade = if_else(shade < 0.5, sqrt(0.5), shade))
shading_df <- full_join(slope_df, hill_df) %>% 
  left_join(elev_df)
shading_df_edge <- shading_df %>% 
  filter(long == min(long[long > 180]))
edge_long <- min(shading_df_edge$long)
spacing <- shading_df$long %>% 
  unique() %>% sort() %>% diff() %>% 
  table() %>% which.max() %>% names() %>% 
  as.numeric()
shading_df <- shading_df %>% 
  full_join(shading_df_edge %>% 
              mutate(long = edge_long - spacing)) %>% 
  full_join(shading_df_edge %>% 
              mutate(long = edge_long - 2 * spacing))

ggplot(shading_df %>% 
         filter(elevation < 0 & lat <= 5 & long >= 300 & long <= 329), aes(x = long, y = lat)) +
  geom_raster(mapping = aes(alpha = desc(elevation)), fill = "lightblue4") +
  scale_alpha_continuous(range = c(0,1)) +
  new_scale("alpha") +
  geom_raster(mapping = aes(fill = shade^2, alpha = slope)) +
  scale_alpha_continuous(range = c(0,1)) +
  scale_fill_gradient2(low = "white", high = "black",
                       mid = "gray50", midpoint = 0.5) +
  coord_fixed()

if (!file.exists("EEZs_simple.RData")){
  EEZ_shp <- readOGR("World_EEZ_v11_20191118_LR/eez_v11_lowres.shp", stringsAsFactors = F)
  EEZ_shp_simple <- ms_simplify(EEZ_shp, keep = 0.01) # takes a while to run!
  save(EEZ_shp_simple, file = "EEZs_simple.RData")
} else {
  load("EEZs_simple.RData")
}
EEZ_shp_df <- EEZ_shp_simple %>% 
  bind(nowrapRecenter(EEZ_shp_simple)) %>% 
  aggregate(list(rep.int(EEZ_shp_simple$GEONAME,2)), FUN = identity) %>% 
  broom::tidy()

ggplot(world, aes(x = long, y = lat, group = group)) +
  geom_raster(data = shading_df %>% 
                filter(elevation < 0), 
              mapping = aes(alpha = desc(elevation), group = NULL), fill = "lightblue4") +
  scale_alpha_continuous(range = c(0,1)) +
  new_scale("alpha") +
  geom_raster(data = shading_df %>% 
                filter(elevation < 0), 
              mapping = aes(fill = shade^2, alpha = slope, group = NULL)) +
  scale_alpha_continuous(range = c(0,1)) +
  geom_polygon(data = EEZ_shp_df, col = "lightblue1", fill = alpha("lightblue", 1/3), size = 0.25) +
  geom_polygon(col = "honeydew2", fill = "honeydew3", size = 0.25) +
  scale_fill_gradient2(low = "lightblue1", high = "black", midpoint = 0.5) +
  new_scale_fill() +
  new_scale("alpha") +
  geom_raster(data = shading_df %>% 
                filter(elevation >= 0),
              mapping = aes(alpha = elevation, group = NULL), fill = "snow") +
  scale_alpha_continuous(range = c(0,1)) +
  new_scale("alpha") +
  geom_raster(data = shading_df %>% 
                filter(elevation >= 0),
              mapping = aes(fill = shade^2, alpha = slope, group = NULL)) +
  scale_alpha_continuous(range = c(0,1)) +
  geom_polygon(data = lakes, col = "honeydew2", fill = "lightblue", size = 0.25) +
  scale_fill_gradient2(low = "white", high = "black",
                       mid = "gray50", midpoint = 0.5) +
  coord_equal(xlim = c(330,-30), ylim = c(85,-85)) +
  scale_x_reverse(expand = c(0,0)) +
  scale_y_reverse(expand = c(0,0)) +
  theme_minimal() +
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title=element_blank(),
        axis.line = element_blank(),  
        panel.background = element_rect(colour = NA, fill = "lightblue"),
        axis.text = element_blank(),  
        axis.ticks = element_blank(),
        plot.margin=grid::unit(c(0,0,-1,-1), "mm"))

ggsave("south_up_shaded.png", width = 20, height = 20*(170/360))

show_borders <- "no"

if (show_borders == "no"){
  padding <- 0.1
} else {
  padding <- 0
}

ggplot(world, aes(x = long, y = lat, group = group)) +
  geom_polygon(data = EEZ_shp_df, col = "black", fill = "lightgray", 
               linewidth = 0.25 + 2 * padding) +
  geom_polygon(data = EEZ_shp_df, col = "lightgray", fill = "lightgray", 
               linewidth = 0 + padding) +
  geom_polygon(col = "black", fill = "white", 
               linewidth = 0.25 + 2 * padding) +
  geom_polygon(col = "white", fill = "white", 
               linewidth = 0 + padding) +
  geom_polygon(data = lakes, col = "black", fill = "lightgray", 
               linewidth = 0.25 + 2 * padding) +
  geom_polygon(data = lakes, col = "lightgray", fill = "lightgray", 
               linewidth = 0 + padding) +
  coord_equal(xlim = c(330,-30), ylim = c(90,-90)) +
  scale_x_reverse(expand = c(0,0)) +
  scale_y_reverse(expand = c(0,0)) +
  theme_minimal() +
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title=element_blank(),
        axis.line = element_blank(),  
        panel.background = element_rect(colour = NA, fill = "gray"),
        axis.text = element_blank(),  
        axis.ticks = element_blank(),
        plot.margin=grid::unit(c(0,0,-1,-1), "mm")) +  
  annotation_custom(grid::grid.text("Siva Kalyan, 2023\n https://github.com/skalyan91/south-up", x=0.9,  y=0.95, gp=grid::gpar(col = "darkgrey", fontsize=10, fontface="italic"))) 

ggsave("south_up_flat.png", width = 20, height = 10)

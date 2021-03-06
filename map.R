library(tidyverse)
library(ggnewscale)
library(mapdata)
library(rgdal)
library(rmapshaper)
library(raster)
library(elevatr)

world <- map_data("world", wrap = c(-30,330))
lakes <- map_data("lakes", wrap = c(-30,330))

elevation <- get_elev_raster(map_data("world") %>% 
                               dplyr::select(long, lat) %>% 
                               filter(long > -180 & long <= 135 &
                                        lat > -79 & lat <= 90), 
                             z = 3, prj = "+proj=longlat +lon_0=150",
                             verbose = F)
slope <- terrain(elevation, opt = "slope")
aspect <- terrain(elevation, opt = "aspect")
hill <- hillShade(slope, aspect, angle = 45, direction = -30)
elev_df <- rasterToPoints(elevation) %>% 
  as_tibble() %>% 
  `colnames<-`(c("long", "lat", "elevation")) %>% 
  mutate(long = long + 150)
slope_df <- rasterToPoints(slope) %>% 
  as_tibble() %>% 
  `colnames<-`(c("long", "lat", "slope")) %>% 
  mutate(long = long + 150,
         slope = if_else(slope > 0.4, 0, slope))
hill_df <- rasterToPoints(hill) %>% 
  as_tibble() %>% 
  `colnames<-`(c("long", "lat", "shade")) %>% 
  mutate(long = long + 150,
         shade = if_else(shade < 0.5, sqrt(0.5), shade))
shading_df <- full_join(slope_df, hill_df) %>% 
  full_join(elev_df)

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

EEZ_shp <- readOGR("World_EEZ_v11_20191118_LR/eez_v11_lowres.shp", stringsAsFactors = F)
EEZ_shp_simple <- ms_simplify(EEZ_shp, keep = 0.01) # takes a while to run!
save(EEZ_shp_simple, file = "EEZs_simple.RData")
EEZ_shp_df <- EEZ_shp_simple %>% 
  bind(maptools::nowrapRecenter(EEZ_shp_simple)) %>% 
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

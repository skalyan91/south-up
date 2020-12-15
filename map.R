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
elev_df <- rasterToPoints(elevation) %>% 
  as_tibble() %>% 
  `colnames<-`(c("long", "lat", "elevation")) %>% 
  mutate(long = long + 150)

EEZ_shp <- readOGR("World_EEZ_v11_20191118_LR/eez_v11_lowres.shp", stringsAsFactors = F)
EEZ_shp_simple <- ms_simplify(EEZ_shp, keep = 0.01) # takes a while to run!
save(EEZ_shp_simple, file = "EEZs_simple.RData")
EEZ_shp_df <- EEZ_shp_simple %>% 
  bind(maptools::nowrapRecenter(EEZ_shp_simple)) %>% 
  aggregate(list(rep.int(EEZ_shp_simple$GEONAME,2)), FUN = identity) %>% 
  broom::tidy()

ggplot(world, aes(x = long, y = lat, group = group)) +
  geom_tile(data = elev_df %>% filter(elevation <= 0), mapping = aes(fill = elevation, group = NULL)) +
  geom_polygon(data = EEZ_shp_df, col = "lightblue1", fill = alpha("lightblue", 1/3), size = 0.25) +
  scale_fill_gradient(low = "lightblue4", high = "lightblue") +
  geom_polygon(col = "honeydew2", fill = "honeydew3", size = 0.25) +
  geom_tile(data = elev_df %>% filter(elevation > 0), fill = "white", mapping = aes(alpha = elevation, group = NULL)) +
  geom_polygon(data = lakes, col = "honeydew2", fill = "lightblue", size = 0.25) +
  # geom_hline(yintercept = seq(-90,90,10), colour = "lightblue3", alpha = 0.25) +
  # geom_vline(xintercept = seq(-30,330,10), colour = "lightblue3", alpha = 0.25) +
  coord_equal(xlim = c(330,-30), ylim = c(85,-85)) +
  scale_x_reverse(expand = c(0,0)) +
  scale_y_reverse(expand = c(0,0)) +
  scale_alpha_continuous(range = c(0,1)) +
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

ggsave("south_up.png", width = 20, height = 20*(170/360))

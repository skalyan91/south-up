source("01_requirements.R")

#WORLMAP DATA
world <- map_data("world", wrap = c(-30,330))
lakes <- map_data("lakes", wrap = c(-30,330))

elevation <- get_elev_raster(tibble(long = c(-180, 180, -180, 180),
                                    lat = c(-85, -85, 85, 85)),
                             z = 3, prj = "+proj=longlat",
                             verbose = F)
slope <- terrain(elevation, opt = "slope")
aspect <- terrain(elevation, opt = "aspect")
hill_se <- hillShade(slope, aspect, angle = 45, direction = -30)
hill_nw <- hillShade(slope, aspect, angle = 45, direction = 150)
elev_df <- rasterToPoints(elevation) %>% 
  as_tibble() %>% 
  `colnames<-`(c("long", "lat", "elevation")) %>% 
  mutate(long = if_else(long < -30, long + 360, long))
slope_df <- rasterToPoints(slope) %>% 
  as_tibble() %>% 
  `colnames<-`(c("long", "lat", "slope")) %>% 
  mutate(long = if_else(long < -30, long + 360, long),
         slope = if_else(slope > 0.4, 0, slope))
hill_se_df <- rasterToPoints(hill_se) %>% 
  as_tibble() %>% 
  `colnames<-`(c("long", "lat", "shade_se")) %>% 
  mutate(long = if_else(long < -30, long + 360, long),
         shade_se = if_else(shade_se < 0.5, sqrt(0.5), shade_se))
hill_nw_df <- rasterToPoints(hill_nw) %>% 
  as_tibble() %>% 
  `colnames<-`(c("long", "lat", "shade_nw")) %>% 
  mutate(long = if_else(long < -30, long + 360, long),
         shade_nw = if_else(shade_nw < 0.5, sqrt(0.5), shade_nw))

shading_df <- full_join(slope_df, hill_se_df, by = c("long", "lat")) %>% 
  full_join(hill_nw_df, by = c("long", "lat")) %>% 
  left_join(elev_df, by = c("long", "lat"))

shading_df_edge <- shading_df %>% 
  filter(long == min(long[long > 180]))
edge_long <- min(shading_df_edge$long)
spacing <- shading_df$long %>% 
  unique() %>% sort() %>% diff() %>% 
  table() %>% which.max() %>% names() %>% 
  as.numeric()

shading_df <- shading_df %>% 
  full_join(shading_df_edge %>% 
              mutate(long = edge_long - spacing), by = join_by(long, lat, slope, shade_se, shade_nw, elevation)) %>% 
  full_join(shading_df_edge %>% 
              mutate(long = edge_long - 2 * spacing), by = join_by(long, lat, slope, shade_se, shade_nw, elevation))

if (!file.exists("EEZs_simple.RData")){
  EEZ_shp <- readOGR("World_EEZ_v11_20191118_LR/eez_v11_lowres.shp", stringsAsFactors = F)
  EEZ_shp_simple <- ms_simplify(EEZ_shp, keep = 0.01) # takes a while to run!
  save(EEZ_shp_simple, file = "EEZs_simple.RData")
} else {
  load("EEZs_simple.RData")
}

EEZ_shp_df <- EEZ_shp_simple %>% 
  bind(nowrapRecenter(EEZ_shp_simple, avoidGEOS = F)) %>% 
  aggregate(list(rep.int(EEZ_shp_simple$GEONAME,2)), FUN = identity) %>% 
  broom::tidy()

#saving the objects for later use. Using qs to compress and splitting up so each file is less than 100 MB

shading_df_greater <- shading_df %>%
  filter(elevation > 0) 

nrow <- nrow(shading_df_greater)
half <- floor(nrow*0.5)

shading_df_greater %>% 
  .[1:half,] %>% 
  qs::qsave(file = "south_up_shading_df_greater_pt1.qs")

shading_df_greater %>% 
  .[half:nrow,] %>% 
  qs::qsave(file = "south_up_shading_df_greater_pt2.qs")

shading_df_less <- shading_df %>%
  filter(elevation < 0) 

nrow <- nrow(shading_df_less)
half <- floor(nrow*0.5)

shading_df_less %>% 
.[1:half,] %>% 
  qs::qsave(file = "south_up_shading_df_less_pt1.qs")
  
shading_df_less %>% 
  .[half:nrow,] %>% 
  qs::qsave(file = "south_up_shading_df_less_pt2.qs")

qs::qsave(EEZ_shp_df, file = "south_up_EEZ_shp_df.qs")


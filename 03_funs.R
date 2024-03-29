#colour_land = "white" 
#colour_water = "gray"
#colour_water_EEZ = "lightgray" 
#colour_outline_land = "black" 
#colour_outline_eez = "#949494"
#colour_lakes = "lightgray"


plot_eez_map_kalyan <- function(elevation_shading = F, 
                                xlim = c(-30, 330),
                                ylim = c(-90, 90),
                                south = "up", 
                                colour_land = "white", 
                                colour_water = "gray",
                                colour_water_EEZ = "lightgray", 
                                colour_outline_land = "black", 
                                colour_border_land = "darkgray",
                                colour_outline_eez = "#949494",
                                colour_lakes = "lightgray"
){

  #loading pkgs
  source("fun_def_h_load.R")
  h_load(pkg = c("tidyverse", "ggnewscale", "qs"))
  
  #loading data that is necessary for both kinds of plots
  world <- map_data("world", wrap = c(-30,330))
  lakes <- map_data("lakes", wrap = c(-30,330))
  EEZ_shp_df  <- qs::qread(file = "south_up_EEZ_shp_df.qs")
  
  
##ELEVATION PLOT  
  if(elevation_shading == T){
    
    #loading data that is only necessary for elevation shaing plot
    shading_df <- qs::qread(file = "south_up_shading_df_greater_pt1.qs")  %>% 
      full_join(qs::qread(file = "south_up_shading_df_greater_pt2.qs"), 
                by = join_by(long, lat, slope, shade_se, shade_nw, elevation)) %>%   
      full_join(qs::qread(file = "south_up_shading_df_less_pt1.qs"), 
                by = join_by(long, lat, slope, shade_se, shade_nw, elevation)) %>% 
      full_join(qs::qread(file = "south_up_shading_df_less_pt2.qs"), 
                by = join_by(long, lat, slope, shade_se, shade_nw, elevation))
    
    #make sure shading is the right way round
    if(south == "up"){
      shading_df <- shading_df %>% 
        rename(shade = shade_se)
    } else {
      shading_df <- shading_df %>% 
        rename(shade = shade_nw)
    }
    
    #plot
    
    p <- ggplot(world, aes(x = long, y = lat, group = group)) +
      geom_tile(data = shading_df %>% 
                    filter(elevation < 0), 
                  mapping = aes(alpha = desc(elevation), group = NULL), fill = "lightblue4") +
      scale_alpha_continuous(range = c(0,1)) +
      ggnewscale::new_scale("alpha") +
      geom_tile(data = shading_df %>% 
                    filter(elevation < 0), 
                  mapping = aes(fill = shade^2, alpha = slope, group = NULL)) +
      scale_alpha_continuous(range = c(0,1)) +
      geom_polygon(data = EEZ_shp_df, col = "lightblue1", fill = alpha("lightblue", 1/3), size = 0.25) +
      geom_polygon(col = "honeydew2", fill = "honeydew3", size = 0.25) +
      scale_fill_gradient2(low = "lightblue1", high = "black", midpoint = 0.5) +
      new_scale_fill() +
      new_scale("alpha") +
      geom_tile(data = shading_df %>% 
                    filter(elevation >= 0),
                  mapping = aes(alpha = elevation, group = NULL), fill = "snow") +
      scale_alpha_continuous(range = c(0,1)) +
      new_scale("alpha") +
      geom_tile(data = shading_df %>% 
                    filter(elevation >= 0),
                  mapping = aes(fill = shade^2, alpha = slope, group = NULL)) +
      scale_alpha_continuous(range = c(0,1)) +
      geom_polygon(data = lakes, 
                   col = "honeydew2", fill = "lightblue", size = 0.25) +
      scale_fill_gradient2(low = "white", high = "black",
                           mid = "gray50", midpoint = 0.5) +
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
  }
  
if(elevation_shading == F){
    padding  <- 0.2
    p <- ggplot(world, aes(x = long, y = lat, group = group)) +
      geom_polygon(data = EEZ_shp_df, 
                   color = colour_outline_eez, 
                   fill = colour_water_EEZ, 
                   linewidth = 0.25 +  padding) +
      geom_polygon(col = colour_border_land, fill = colour_land, 
                   linewidth = 0.25 + 2 * padding) +
      geom_polygon(col = colour_border_land, fill = colour_land, 
                   linewidth = 0 + padding) +
      geom_polygon(data = lakes, col = colour_border_land, fill = colour_lakes, 
                   linewidth = 0.25 + 2 * padding) +
      geom_polygon(data = lakes, col = colour_lakes, fill = colour_lakes, 
                   linewidth = 0 + padding) +  theme_minimal() +
      theme(legend.position="none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title=element_blank(),
            axis.line = element_blank(),  
            panel.background = element_rect(colour = NA, fill = colour_water),
            axis.text = element_blank(),  
            axis.ticks = element_blank(),
            plot.margin=grid::unit(c(0,0,-1,-1), "mm")
      ) 
    
  }
  if(south == "up"){
    p <- p  +
      coord_equal(xlim = rev(xlim), ylim = rev(ylim)) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) 
    
  }else{
    p <- p  +
      coord_equal(xlim = xlim, ylim = ylim) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) 
    
  }
  
  
p
  } 
  
  
  
  




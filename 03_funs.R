colour_land = "white" 
colour_water = "gray"
colour_water_EEZ = "lightgray" 
colour_outline_land = "black" 
colour_outline_eez = "#949494"
colour_lakes = "lightgray"


plot_eez_map_kalyan <- function(elevation_shading = F, 
                                xlim = NULL, 
                                ylim = NULL, 
                                south = "up", 
                                colour_land = "white", 
                                colour_water = "gray",
                                colour_water_EEZ = "lightgray", 
                                colour_outline_land = "black", 
                                colour_outline_eez = "#949494",
                                colour_lakes = "lightgray"
){
#loading data and packages
  load("south_up_fun_needed_data.RData")
  
  source("fun_def_h_load.R")
  h_load(pkg = c("tidyverse", "ggnewscale"))
  
  if(elevation_shading == T){
    p <- ggplot(needed_data$world, aes(x = long, y = lat, group = group)) +
      geom_tile(data = needed_data$shading_df %>% 
                    filter(elevation < 0), 
                  mapping = aes(alpha = desc(elevation), group = NULL), fill = "lightblue4") +
      scale_alpha_continuous(range = c(0,1)) +
      ggnewscale::new_scale("alpha") +
      geom_tile(data = needed_data$shading_df %>% 
                    filter(elevation < 0), 
                  mapping = aes(fill = shade^2, alpha = slope, group = NULL)) +
      scale_alpha_continuous(range = c(0,1)) +
      geom_polygon(data = needed_data$EEZ_shp_df, col = "lightblue1", fill = alpha("lightblue", 1/3), size = 0.25) +
      geom_polygon(col = "honeydew2", fill = "honeydew3", size = 0.25) +
      scale_fill_gradient2(low = "lightblue1", high = "black", midpoint = 0.5) +
      new_scale_fill() +
      new_scale("alpha") +
      geom_tile(data = needed_data$shading_df %>% 
                    filter(elevation >= 0),
                  mapping = aes(alpha = elevation, group = NULL), fill = "snow") +
      scale_alpha_continuous(range = c(0,1)) +
      new_scale("alpha") +
      geom_tile(data = needed_data$shading_df %>% 
                    filter(elevation >= 0),
                  mapping = aes(fill = shade^2, alpha = slope, group = NULL)) +
      scale_alpha_continuous(range = c(0,1)) +
      geom_polygon(data = needed_data$lakes, 
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
    p <- ggplot(needed_data$world, aes(x = long, y = lat, group = group)) +
      geom_polygon(data = needed_data$EEZ_shp_df, 
                   color = colour_outline_eez, 
                   fill = colour_water_EEZ, 
                   linewidth = 0.25 +  padding) +
      geom_polygon(col = colour_outline_land, fill = colour_land, 
                   linewidth = 0.25 + 2 * padding) +
      geom_polygon(col = colour_land, fill = colour_land, 
                   linewidth = 0 + padding) +
      geom_polygon(data = needed_data$lakes, col = "black", fill = colour_lakes, 
                   linewidth = 0.25 + 2 * padding) +
      geom_polygon(data = needed_data$lakes, col = colour_lakes, fill = colour_lakes, 
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
  if(south == "up" & is.null(xlim) &
     is.null(ylim)){
    p <- p  +
      xlim(c(330,-30)) +
      ylim(c(85,-85))
  }
  
  if(south != "up" & is.null(xlim) &
     is.null(ylim)){
    p <- p  +
      xlim(c(-30,330)) +
      ylim(c(-85,85))
  }
  
  
#ZOOMING IN
  if(is.null(xlim) & !is.null(ylim)|
    !is.null(xlim) & is.null(ylim)){
    
    warning("Please specify both xlim and ylm.\n")
    stop()
  }
  
  
if(!is.null(xlim) & !is.null(ylim)){
  
  
  if(south == "up"){
    ylim = rev(ylim)
    xlim = rev(xlim)
  }
  
  
#  xlim = c(78, 255)
  #    ylim = c(-56,27)
  p <- p +
    coord_equal(xlim = xlim, ylim = ylim) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) 
    }

p
  } 
  
  
  
  
  
plot_eez_map_kalyan(elevation_shading = F)




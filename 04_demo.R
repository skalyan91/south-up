source("03_funs.R")

png(filename = "test.png", width = 20, height = 15, units = "cm", res = 400)
plot_eez_map_kalyan(elevation_shading = F, south = "down", 
                    colour_water_EEZ = "azure1",
                    colour_water = "white", 
                    colour_land = "aliceblue", 
                    colour_outline_eez = "lightgrey", 
                    xlim = c(20, 280),
                    ylim = c(-50, 50)
)

x <- dev.off()

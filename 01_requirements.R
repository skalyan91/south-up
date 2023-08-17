#installing packages, using groundhogr so that it's particular package versions.

#install.packages("groundhog")
library(groundhog)

date <- "2023-07-17"

groundhog_dir <- paste0("groundhog_libraries_", date)

if(!dir.exists(groundhog_dir)){
  dir.create(groundhog_dir)
}

set.groundhog.folder(groundhog_dir)

groundhog_pkgs <- c("elevatr",
                    "maptools", 
                    "raster", 
                    "rgeos", 
                    "ggnewscale",
                    "rmapshaper", 
                    "sf", 
                    "tidyverse")

groundhog.library(groundhog_pkgs, "2023-07-17")

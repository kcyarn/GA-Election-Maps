library(tidyverse)
library(plyr)
library(dplyr)
library(sf)
library(spatstat)
library(maptools) #Needed for as.owin
library(purrr)

# Set the working directory
setwd("C:/Users/kcyar/OneDrive - The Index Writer, LLC/Medium/better_ga_map")

all_map_file <- "ga_2020_general/ga_2020_general.shp"
all_map_data <- st_read(all_map_file)

all_map_data$STATE <- "Georgia"

# Get Georgia map with counties
ga_only <- all_map_data %>%
  group_by(CTYNAME) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

save(ga_only, file="ga_cty_state.Rdata")
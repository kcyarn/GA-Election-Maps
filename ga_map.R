library(tidyverse)
library(dplyr)
library(sf)
library(maps)

# Set the working directory
setwd("C:/Users/kcyar/OneDrive - The Index Writer, LLC/Medium/better_ga_map")

ga_file <- "ga_2020_general/ga_2020_general.shp"
ga_data <- st_read(ga_file)
# The shapefile uses GCS_GRS_1980. It must be set manually.
ga_data <- ga_data %>% st_set_crs(4019)
# Now transform crs to 4326, which is the same as the crs for the base GA map.
ga_data <- ga_data %>% st_transform(crs=4326)

ga_map <- st_union(ga_data, by_feature = FALSE, is_coverage = FALSE)

package_ga <- st_as_sf(maps::map("state", "georgia", plot = FALSE, fill = TRUE))

ga_clean <- st_union(ga_map, package_ga, by_feature = FALSE, is_coverage = FALSE)

# County map

ga_county_group <- ga_data %>%
  group_by(CTYNAME) %>%
  select(CTYNAME, CTYSOSID, geometry) %>%
  summarise_()

names(ga_county_group)[1] <- "ID"
names(ga_county_group)[2] <- "geom"
st_geometry(ga_county_group) <- "geom"

ga_county_group <- ga_county_group %>%
  filter(!is.na(ID)) %>%
  mutate(ID = paste("georgia", str_to_lower(ID), sep=","))

# Remove counties with small polygon issues. These will be replaced with the map package. The map only really needs the Savannah River and the Atlantic Ocean areas from the Harvard set.
ga_county_group <- ga_county_group[!(ga_county_group$ID == "georgia,fulton"),]
ga_county_group <- ga_county_group[!(ga_county_group$ID == "georgia,forsyth"),]
ga_county_group <- ga_county_group[!(ga_county_group$ID == "georgia,paulding"),]
ga_county_group <- ga_county_group[!(ga_county_group$ID == "georgia,dekalb"),]
ga_county_group <- ga_county_group[!(ga_county_group$ID == "georgia,jeff davis"),]


# remove the na values. Then merge with the package_ga_county dataset. Then run st_union.

package_ga_county <- st_as_sf(maps::map("county", "georgia", plot=FALSE, fill=TRUE))

merged_counties <- rbind(ga_county_group, package_ga_county)

ga_county_clean <- merged_counties %>%
  group_by(ID) %>%
  select(ID, geom) %>%
  summarise_()
  

save(ga_clean, ga_county_clean, file="ga_clean.Rdata")

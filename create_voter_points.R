library(tidyverse)
library(plyr)
library(dplyr)
library(sf)
library(spatstat)

# Set the working directory
setwd("C:/Users/kcyar/OneDrive - The Index Writer, LLC/Medium/better_ga_map")

load("ga_map.Rdata")

# Generate points within precinct to represent voter density. Note, this uses Ballots_Cast, not Total_Votes due to the previous issues mentioned with Hall County. In truth,100% voter turnout is practically unheard. On election day, the results are all about who shows up. Who stays home makes a difference, but we're visualizing who showed up because that is the election result.

#ga_map$Ballots_Cast_Log[ga_map$Ballots_Cast_Log == "-Inf"] <- NA

ga_map_filter <- ga_map %>%
  select(geometry, Ballots_Cast, R_Percentage_BC, D_Percentage_BC, R_MOV_BC, R_MOV_BC_COLOR, R_Percentage_RDL, D_Percentage_RDL, R_MOV_RDL, R_MOV_RDL_COLOR) %>%
  filter(!is.na(Ballots_Cast)) %>%
  st_as_sf()

vote_1 <- 1

vote_10 <- 10

vote_25 <- 25

vote_50 <- 50

vote_100 <- 100

ga_map_dots <- function(scale) {
  filter_map <- ga_map_filter %>%
    select(geometry, Ballots_Cast) %>%
    mutate(Ballots_Cast = round(Ballots_Cast/scale),0) %>%
    st_as_sf()
  
  correct_map <- filter_map %>%
    filter(Ballots_Cast >= 1)
  
  voter_dots <- st_sample(filter_map$geometry, filter_map$Ballots_Cast, type="random", exact=TRUE)
  voter_dots <- st_sf(voter_dots)
  voter_dots_merge <- st_join(voter_dots, ga_map_filter)
  
}

#voter_dots_1 <- ldply(vote_1, ga_map_dots)
voter_dots_10 <- ldply(vote_10, ga_map_dots)
#voter_dots_25 <- ldply(vote_25, ga_map_dots)
#voter_dots_50 <- ldply(vote_50, ga_map_dots)
#voter_dots_100 <- ldply(vote_100, ga_map_dots)


save(ga_map_filter, voter_dots_10, file="voter_points.Rdata")

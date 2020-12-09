library(maps)
library(tidyverse)
library(readxl)
library(ggplot2)
library(plyr)
library(dplyr)
library(sf)
library(spatstat)
library(maptools) #Needed for as.owin
library(kableExtra)
library(scales)
#library(purrr)
#library(gganimate)
#library(gifski)
library(xml2)
library(extrafont)

# Set the working directory
setwd("C:/Users/kcyar/OneDrive - The Index Writer, LLC/Medium/better_ga_map")

# Load all XML files fetched by the python script (1 per county)

xml_dir <- "data/xml_data"
xml_list <- list.files(path = xml_dir)

## Turnout function

turnout_data <- function (x) {
  this_path <- paste(xml_dir,x, sep="/")
  this_xml <- read_xml(this_path)
  
  # Find the County Name
  this_county <- xml_find_all(this_xml, ".//Region") %>% xml_text()
  this_county <- str_to_lower(this_county)
  
  # Get Voter Turnout Stats
  this_turnout <- xml_find_all(this_xml, ".//VoterTurnout")
  this_turnout_pre <- xml_attr(xml_find_all(this_turnout, ".//Precinct"), "name")
  this_turnout_tv <- xml_attr(xml_find_all(this_turnout, ".//Precinct"), "totalVoters")
  this_turnout_bc <- xml_attr(xml_find_all(this_turnout, ".//Precinct"), "ballotsCast")
  
  this_turnout_pre <- str_to_lower(this_turnout_pre)
  # Add "Path" = this_path if you need to quick way to find county xml files.
  turnout <- data.frame("County" = this_county, "Precinct" = this_turnout_pre, "Total_Voters" = this_turnout_tv, "Ballots_Cast" = this_turnout_bc)
  
}

## Create turnout data.frame using the turnout function to loop through all xml files.
## The ldply function from the plyr package is the simplest way to do this. Yes, it's old. There are other ways to do this, but they're not nearly as quick to type.


turnout <- ldply(xml_list, turnout_data)
turnout$id <- paste(turnout$Precinct, turnout$County, sep=".")
turnout[, 3:4] <- sapply(turnout[, 3:4], as.numeric)


# Use the state's active voter stats to fix the counties that somehow forgot to report how many total voters are in each precinct. https://sos.ga.gov/admin/uploads/Active_Voters_by_Race_and_Gender_By_Congressional_as_of_November_1_2020.xlsx

fix_turnout_totals <- read.csv("data/missing_precinct_total_voters.csv")

for(i in 1:nrow(fix_turnout_totals)) {
  this_id <- fix_turnout_totals[i, 1]
  this_total <- fix_turnout_totals[i, 2]
  turnout$Total_Voters[turnout$id == this_id] <- this_total
}


# A useful bit to list out the problem precincts for cleaning/correction purposes. This is already done.
#zero_turnout <- turnout %>%
# filter(Total_Voters == "0")
#write.csv(zero_turnout, "data/zero_turnout_errors.csv")

# Empty Choice dataframe
choice <- NULL

# The election_data function creates 1 data.frame for a specified contest. Will need to rework this to use it for additional contests.

election_data <- function(x) {
  
  # Always check the contest key! These change by election.
  ## Nov 3 - president = 1, senate = 2, senate special = 3
  
  this_path <- paste(xml_dir, x, sep="/")
  this_xml <- read_xml(this_path)
  
  # Find the County Name
  this_county <- xml_find_all(this_xml, ".//Region") %>% xml_text()
  this_county <- str_to_lower(this_county, locale = "en")
  
  # Get Contest Stats
  # //title[@lang='en']
  this_contest <- xml_find_all(this_xml, "//Contest[@key=1]")
  # Undervotes
  this_uv <- xml_find_all(this_contest, ".//VoteType[@name='Undervotes']")
  this_uv_pre <- xml_attr(xml_find_all(this_uv, ".//Precinct"), "name")
  this_uv_pre <- str_to_lower(this_uv_pre)
  
  this_uv_count <- xml_attr(xml_find_all(this_uv, ".//Precinct"), "votes")
  uv <- data.frame("County" = this_county, "Precinct" = this_uv_pre, "Undervotes" = this_uv_count)
  
  # Overvotes
  this_ov <- xml_find_all(this_contest, ".//VoteType[@name='Overvotes']")
  this_ov_pre <- xml_attr(xml_find_all(this_ov, ".//Precinct"), "name")
  this_ov_pre <- str_to_lower(this_ov_pre)
  
  this_ov_count <- xml_attr(xml_find_all(this_ov, ".//Precinct"), "votes")
  ov <- data.frame("Precinct" = this_ov_pre, "Overvotes" = this_ov_count)
  
  # Merge Undervotes and Overvotes
  uv_ov = uv %>% left_join(ov, "Precinct", copy=FALSE)
  uv_ov$id <- paste(uv_ov$Precinct, uv_ov$County, sep=".")
  
  # Candidates
  this_choice_list <- xml_attr(xml_find_all(this_contest, ".//Choice"), "key")
  
  for (i in this_choice_list) {
    this_key <- paste("//Choice[@key=",i,"]", sep="")
    this_choice <- xml_find_all(this_contest, this_key)
    this_name <- xml_attr(this_choice, "text")
    
    this_ed <- xml_find_all(this_choice, "VoteType[1]")
    this_ed_pre <- xml_attr(xml_find_all(this_ed, ".//Precinct"), "name")
    this_ed_pre <- str_to_lower(this_ed_pre)
    this_ed_votes <- xml_attr(xml_find_all(this_ed, ".//Precinct"), "votes")
    
    this_ed_df <- data.frame("Precinct" = this_ed_pre, "County" = this_county, this_ed_votes)
    
    colnames(this_ed_df)[3] <- paste(this_name, "Election_Day_Votes", sep=".")
    
    this_av <- xml_find_all(this_choice, "VoteType[2]")
    this_av_pre <- xml_attr(xml_find_all(this_av, ".//Precinct"), "name")
    this_av_pre <- str_to_lower(this_av_pre)
    this_av_votes <- xml_attr(xml_find_all(this_av, ".//Precinct"), "votes")
    
    this_av_df <- data.frame("Precinct" = this_av_pre, "Advanced_Voting_Votes" = this_av_votes)
    colnames(this_av_df)[2] <- paste(this_name, "Advanced_Voting_Votes", sep=".")
    
    this_ab <- xml_find_all(this_choice, "VoteType[3]")
    this_ab_pre <- xml_attr(xml_find_all(this_ab, ".//Precinct"), "name")
    this_ab_pre <- str_to_lower(this_ab_pre)
    this_ab_votes <- xml_attr(xml_find_all(this_ab, ".//Precinct"), "votes")  
    
    this_ab_df <- data.frame("Precinct" = this_ab_pre, "Absentee_by_Mail_Votes" = this_ab_votes)
    colnames(this_ab_df)[2] <- paste(this_name, "Absentee_by_Mail_Votes", sep=".")
    
    this_pv <- xml_find_all(this_choice, "VoteType[4]")
    this_pv_pre <- xml_attr(xml_find_all(this_pv, ".//Precinct"), "name")
    this_pv_pre <- str_to_lower(this_pv_pre)
    this_pv_votes <- xml_attr(xml_find_all(this_pv, ".//Precinct"), "votes")  
    
    this_pv_df <- data.frame("Precinct" = this_pv_pre, "Provisional_Votes" = this_pv_votes)
    
    colnames(this_pv_df)[2] <- paste(this_name, "Provisional_Votes", sep=".")
    this_df <- merge(this_ed_df, this_av_df, all=TRUE)
    
    this_df <- merge(this_df, this_ab_df, all=TRUE)
    
    this_df <- merge(this_df, this_pv_df, all=TRUE)
    
    this_df$id <- paste(this_df$Precinct, this_df$County, sep=".")
    
    this_df$Precinct <- NULL
    this_df$County <- NULL
    
    this_df[, 1:4] <- sapply(this_df[, 1:4], as.numeric)
    
    this_df$Total <- this_df[,1] + this_df[,2] + this_df[,3] + this_df[,4]
    colnames(this_df)[6] <- paste(this_name, "Total", sep=".")
    
    
    if (is.null(choice)) {
      choice <- this_df
    } else {
      choice <- merge(choice, this_df, by="id")
    }
    
  }
  
  results <- merge(uv_ov, choice, by="id", all=TRUE)
  
}

## Create the election data frame

election <- ldply(xml_list, election_data)


election$County <- NULL
election$Precinct <-NULL
election[, 2:3] <- sapply(election[, 2:3], as.numeric)

# Create election_merge. This is the primary data.frame used for precinct visualizations.

election_merge <- merge(turnout, election, by="id", all=TRUE)

## Calculate Margin of Victory (Republican) in points. It doesn't matter which one you choose for this as long as you stay consistent. Note, this formula is not appropriate for comparing elections. For comparison, this needs to be a normalized MOV and the equation itself may vary depending on how victory is defined. Races cannot be comared with R_MOV_RDL. They must be compared with R_MOV_BC, which uses the total ballots cast in the election.

# Aggregated County Data.
# county_merge is the primary data.frame used for county visualizations.

county_merge <- as.data.frame(election_merge) %>%
  group_by(County) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  ungroup() %>%
  mutate("Total_RDL" = .[[10]] + .[[15]] + .[[20]]) %>%
  mutate("R_Percentage_BC" = .[[10]] / .[[3]], "D_Percentage_BC" = .[[15]]/.[[3]], "R_MOV_BC" = (R_Percentage_BC - D_Percentage_BC)*100, "R_Percentage_RDL" = .[[10]] / Total_RDL, "D_Percentage_RDL" = .[[15]]/Total_RDL, "R_MOV_RDL" = (R_Percentage_RDL - D_Percentage_RDL)*100, "ID" = paste("georgia", str_to_lower(County), sep=","))

election_merge <- election_merge %>%
  mutate("Total_RDL" = .[[12]] + .[[17]] + .[[22]]) %>%
  mutate("R_Percentage_BC" = .[[12]] / .[[5]], "D_Percentage_BC" = .[[17]]/.[[5]], "R_MOV_BC" = (R_Percentage_BC - D_Percentage_BC)*100, "R_Percentage_RDL" = .[[12]] / Total_RDL, "D_Percentage_RDL" = .[[17]]/Total_RDL, "R_MOV_RDL" = (R_Percentage_RDL - D_Percentage_RDL)*100)


# Assign color volues to R_MOV_BC. Basically, rescale this to 0-1 where 0 -.5 = Democrat and .5 - 1 = Republican.

election_merge <- election_merge %>%
  mutate(R_MOV_BC_COLOR = (R_MOV_BC + 100) / 200, R_MOV_RDL_COLOR = (R_MOV_RDL + 100)/200)

county_merge <- county_merge %>%
  mutate(R_MOV_BC_COLOR = (R_MOV_BC + 100) / 200,  R_MOV_RDL_COLOR = (R_MOV_RDL + 100)/200)

# NOW BEGIN COMBINING THE ABOVE WITH MAP DATA. YOU MUST HAVE ga_clean.Rdata generated for this!

# Load Ga Map as sf
load("ga_clean.Rdata")

# Load Base Map File

map_file <- "ga_2020_general/ga_2020_general.shp"
map_data <- st_read(map_file)
# The shapefile uses GCS_GRS_1980. It must be set manually.
map_data <- map_data %>% st_set_crs(4019)
# Now transform crs to 4326, which is the same as the crs for the base GA map.
map_data <- map_data %>% st_transform(crs=4326)

map <- as.data.frame(map_data)

map$id <- paste(str_to_lower(map$PRECINCT_N), str_to_lower(map$CTYNAME), sep=".")

# Fix Chatham County

map$prec_id <- paste(str_to_lower(map$PRECINCT_I), str_to_lower(map$PRECINCT_N), sep=" ")
map$prec_county_id <- paste(map$prec_id, str_to_lower(map$CTYNAME), sep=".")

chatham <- map %>%
  filter(CTYNAME == "CHATHAM") %>%
  mutate(id = prec_county_id)

# Delete Chatham County rows from map

map <- map[!(map$CTYNAME == "CHATHAM"),]

# Add fixed Chatham IDS back to the map

map <- rbind(map, chatham)

#Cleanup chatham
rm(chatham)
map$prec_id <- NULL
map$prec_county_id <- NULL

# Fix Ware County

map$prec_id <- paste(str_to_lower(map$PRECINCT_I), str_to_lower(map$CTYNAME), sep=".")
ware <- map %>%
  filter(CTYNAME=="WARE") %>%
  mutate(id = prec_id)

map <- map[!(map$CTYNAME == "WARE"),]

map <- rbind(map, ware)

#Cleanup Ware
rm(ware)
map$prec_id <- NULL

# Individual ID Fixes (Spelling and punctuation issues)
map$id[map$id == "tignall sch lunch rm.wilkes"] <- "tignal sch lunch rm.wilkes"
map$id[map$id == "04 covenant life sanctuary.barrow"] <- "04 covenant life sanctuary .barrow"
map$id[map$id == "chappell mill vfd.lamar"] <- "chappell mill v fd.lamar"
map$id[map$id == "03 hmong new hope alliance church.barrow"] <- "03 hmong new hope alliance church .barrow"
map$id[map$id == "activity center (includes ftben 1-3).chattahoochee"] <- "activity center.chattahoochee"
map$id[map$id == "little ochlocknee.thomas"] <- "little ochlocknee baptist church.thomas"
map$id[map$id == "powder springs 1a.cobb"] <- "powders springs 1a.cobb"
map$id[map$id == "powder springs 2a.cobb"] <- "powders springs 2a.cobb"
map$id[map$id == "powder springs 3a.cobb"] <- "powders springs 3a.cobb"
map$id[map$id == "5-07c elks lodge.chatham"] <- "5-07c station 1.chatham"
map$id[map$id == "6-10c georgetown elementary.chatham"] <- "6-10c station 3.chatham"
map$id[map$id == "7-16c pooler rec center gym.chatham"] <- "7-16c pooler recreation center gymnasium.chatham"
map$id[map$id == "8-08c resurrection of our lord church.chatham"] <- "8-08c resur of our lord church.chatham"


ga_map <- map %>%
  left_join(election_merge, by="id")

# Fulton County Issues
## There are 20 precincts in the Harvard dataset that are not in Fulton county's final election results.
#fulton_file <- "fulton_county/Voting_Precincts.shp"
#fulton_data <- st_read(fulton_file)
#fulton <- as.data.frame(fulton_data)

#fulton_election <- election_merge %>%
# filter(County == "fulton")

# A quick check with the above confirms that Fulton county's official xml file only reported data for 384 precincts, not the 404 listed in both the Harvard shapefile and the Fulton County provided shapefile. Fulton County experienced a great deal of pandemic-related disruption this cycle. This issue is not unexpected. We may still need to deal with the 20 missing precincts. Keep in mind that missing in this context means their voters voted elsewhere. It does not mean their votes went unreported.


# Get the NA list and fix the ids.

#ga_na_list <- ga_map %>%
# select("PRECINCT_I", "PRECINCT_N", "CTYNAME", "id", "County") %>%
#filter(is.na(County))
#write.csv(ga_na_list, "data/ga_na_list.csv")


# Fix De Kalb County id
county_merge$ID[county_merge$ID == "georgia,dekalb"] <- "georgia,de kalb"


# Merge County data with map. Problem with list. Needs to unlist?
ga_county_map <- as.data.frame(county_merge) %>%
  left_join(ga_county_clean, by="ID")

st_geometry(ga_county_map) <- ga_county_map$geom

## SAVE WHAT YOU NEED

# Save turnout, election_merge, and county_merge to Rdata.

save(turnout, election_merge, county_merge, file="turnout.Rdata")

# Keep the datasets seperate by saving different categories into seperate Rdata files. This makes it the dot density map slightly easier on old laptop processor. (It's a relative thing. It will never enjoy creating that!)
# Save ga_map.
save(ga_map, ga_county_map, file="ga_map.Rdata")
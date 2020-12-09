library(maps)
library(tidyverse)
library(readxl)
library(ggplot2)
library(plyr)
library(dplyr)
library(sf)
library(spatstat)
library(maptools)

# Set the working directory
setwd("C:/Users/kcyar/OneDrive - The Index Writer, LLC/Medium/better_ga_map")

xml_dir <- "data/xml_data"
xml_list <- list.files(path = xml_dir)

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

# The ldply function from the plyr package is the simplest way to do this.


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


# A useful bit to list out the problem precincts.
#zero_turnout <- turnout %>%
# filter(Total_Voters == "0")
#write.csv(zero_turnout, "data/zero_turnout_errors.csv")

# Empty Choice dataframe
choice <- NULL

senate1_data <- function(x) {
  
  # Always check the contest key! These change by senate1.
  ## Nov 3 - president = 1, senate = 2, senate special = 3
  
  this_path <- paste(xml_dir, x, sep="/")
  this_xml <- read_xml(this_path)
  
  # Find the County Name
  this_county <- xml_find_all(this_xml, ".//Region") %>% xml_text()
  this_county <- str_to_lower(this_county, locale = "en")
  
  # Get Contest Stats
  # //title[@lang='en']
  this_contest <- xml_find_all(this_xml, "//Contest[@key=2]")
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
    
    colnames(this_ed_df)[3] <- paste(this_name, "senate1_Day_Votes", sep=".")
    
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

senate1 <- ldply(xml_list, senate1_data)


senate1$County <- NULL
senate1$Precinct <-NULL
senate1[, 2:3] <- sapply(senate1[, 2:3], as.numeric)

senate1_merge <- merge(turnout, senate1, by="id", all=TRUE)

senate1_merge$Sen1_Total_votes <- senate1_merge[[10]] + senate1_merge[[15]] + senate1_merge[[20]])
=
# Aggregated County Data
county_sen1_merge <- as.data.frame(senate1_merge) %>%
  group_by(County) %>%
  summarise_if(is.numeric, funs(sum)) %>%
  ungroup() %>%
  mutate("R_Percentage" = .[[10]] / .[[3]], "D_Percentage" = .[[15]]/.[[3]], "R_MOV" = (R_Percentage - D_Percentage)*100, "ID" = paste("georgia", str_to_lower(County), sep=","))

#senate1_merge$Ballots_Cast_Log <- log(senate1_merge$Ballots_Cast)

# Chloropleth
senate1_merge$R_Percentage <-  senate1_merge[,12]/senate1_merge[,5]
senate1_merge$D_Percentage <- senate1_merge[,17] / senate1_merge[,5]


# Calculate Margin of Victory (Republican) in points. It doesn't matter which one you choose for this as long as you stay consistent. Note, this formula is not appropriate for comparing senate1s. For comparison, this needs to be a normalized MOV and the equation itself may vary depending on how victory is defined.

senate1_merge$R_MOV <- (senate1_merge$R_Percentage - senate1_merge$D_Percentage)*100

# Assign color volues to R_MOV

senate1_merge <- senate1_merge %>%
  mutate(R_MOV_COLOR = (R_MOV + 100) / 200)

county_sen1_merge <- county_sen1_merge %>%
  mutate(R_MOV_COLOR = (R_MOV + 100) / 200)

# Check the Undervotes

sen1_total_undervote <- sum(senate1_merge$Undervotes)
sen1_total_undervote

r_senate1_merge <- senate1_merge %>%
  filter(R_Percentage > D_Percentage)

rsen1_undervote <- sum(r_senate1_merge$Undervotes)
rsen1_undervote

d_senate1_merge <- senate1_merge %>%
  filter(D_Percentage > R_Percentage)

dsen1_undervote<- sum(d_senate1_merge$Undervotes)
dsen1_undervote

diff_r_dsen1_undervote <- rsen1_undervote - dsen1_undervote
diff_r_dsen1_undervote

neutral_senate1_merge <- senate1_merge %>%
  filter(D_Percentage == R_Percentage)

test_rmov <- function(x, y) {
  this_senate1_filter <- senate1_merge %>%
    filter(x <= R_MOV_COLOR & R_MOV_COLOR < y)
  totalsen1_undervote <- sum(this_senate1_filter$Undervotes)/sum(this_senate1_filter$Ballots_Cast)
}

d65sen1_under <- test_rmov(0, 0.175)
d65sen1_under

d40sen1_under <- test_rmov(0.175, 0.3)
d40sen1_under

d15sen1_under <- test_rmov(0.3, 0.425)
d15sen1_under

d_0sen1_under <- test_rmov(0.425, 0.5)
d_0sen1_under

r15sen1_under <- test_rmov(0.5001, 0.575)
r15sen1_under

r40sen1_under <- test_rmov(0.575, 0.7)
r40sen1_under

r65sen1_under <- test_rmov(0.7, 1)
r65sen1_under

save(senate1_merge, county_sen1_merge, file="sen1_data.Rdata")

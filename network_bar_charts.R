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

# These commands do not need to be run every time.
#font_import()
#loadfonts(device = "win")

# Load workspace with fonts
load("fonts.Rdata")

stacked_bar <- function(df, colors, this_title) {
  stacked_bar <- ggplot(df, aes(y=Chart, x=Race)) +
    geom_col(aes(fill=Category), width=0.6) +
    geom_bar(position="fill", stat="identity") +
    scale_fill_manual(values=colors) +
    coord_flip() +
    theme_void() +
    geom_text(aes(label = Labels, group=Category), position=position_stack(vjust=0.5), size=5, family="Noto Sans",  fontface="bold", color ="white") +
    labs(title = "Georgia General Election 2020", subtitle=this_title) +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5, size = 20, family="Noto Sans"),
          plot.subtitle = element_text(hjust = 0.5, size = 15, color="gray30", vjust = -1, face="italic", family="Noto Sans")
    )
}

# Load election_overviow and pres_totals

election_overview <- read.csv("output/election_overview.csv")

total_registered_voters <- as.numeric(election_overview$Total_Registered_Voters[1])

pres_totals <- read.csv("output/pres_totals.csv")

actual_total_votes <- as.numeric(pres_totals[6] + pres_totals[8] + pres_totals[10])


calc_est_voters <- function(network, percent_reporting, r_votes, r_percent, d_votes, d_percent) {
  r_d_total <- r_votes + d_votes
  r_d_percent <- r_percent + d_percent
  third_party_percent <- 1 - r_d_percent
  
  current_tally <- round(r_d_total / r_d_percent)
  base_network_est <- round(current_tally/percent_reporting)
  
  third_party_votes <- current_tally - r_d_total
  
  # subtract 3rd party from the base_network_est for the network's estimated of the total remaining votes
  est_votes_remaining <- base_network_est - third_party_votes - r_votes - d_votes
  
  # Registered voters remaining
  reg_votes_remaining <- total_registered_voters - third_party_votes - r_votes - d_votes
  
  # Actual Votes Remainings (uses certified results!)
  
  actual_votes_remaining <- actual_total_votes - third_party_votes - r_votes - d_votes
  
  this_est_votes <- data.frame("Network" = network, "Percent In" = percent_reporting, "Network Est Voter Turnout" = base_network_est, "Network Est No Third" = base_network_est - third_party_votes, "Network Est Votes Remaining" = est_votes_remaining, "Registered Voters" = total_registered_voters, "Registered Voters No Third" = total_registered_voters - third_party_votes, "Registered Voters Remaining" = reg_votes_remaining, "Actual Votes Cast" = actual_total_votes, "Actual Votes No Third" = actual_total_votes - third_party_votes, "Actual Votes Remaining" = actual_votes_remaining, "Republican Votes" = r_votes, "Republican Percent" = r_percent, "Democratic Votes" = d_votes, "Democratic Percent" = d_percent, "Third Party Votes" = third_party_votes, "Third Party Percent" = third_party_percent)
}


fake_real_news <- function(est_voter_label, est_voters, r_votes, d_votes, this_chart_subtitle, est_vote_type, file_prefix) {
    
    nov_3 <- data.frame("Category" = c(est_voter_label, "Republican Votes", "Democratic Votes"), "Total" = c(est_voters,  r_votes, d_votes), Race = "Presidential")
    
    nov_3 <- nov_3 %>%
      mutate(Category = fct_relevel(Category, est_voter_label, "Republican Votes", "Democratic Votes"))
    
    
    nov_3_possible_votes <- nov_3$Total[1] - (nov_3$Total[2] + nov_3$Total[3])
    
    nov_3$Chart <- c(nov_3_possible_votes, nov_3$Total[2], nov_3$Total[3])
    
    nov_3_no_reg <- nov_3
    nov_3_no_reg <- nov_3_no_reg[-c(1),]
    
    # Nov_3 labels
    
    nov_3_reg_voters_remain <- nov_3_possible_votes/nov_3$Total[1]
    
    nov_3_reg_label <- paste(est_voter_label, " Outstanding\n",(percent(round(nov_3_reg_voters_remain, 4), accuracy=0.01)), "\n", format(nov_3_possible_votes, big.mark=","), sep="")
    
    nov_3_label_r <- paste("Trump (R)\n", percent(round((nov_3$`Total`[2]/nov_3$`Total`[1]), 4), accuracy=0.01), "\n", format(nov_3$`Total`[2],big.mark=","), sep="")
    nov_3_label_d <- paste("Biden (D)\n", percent(round((nov_3$`Total`[3]/nov_3$`Total`[1]), 4), accuracy=0.01), "\n", format(nov_3$`Total`[3], big.mark=","), sep="")
    
    nov_3$Labels <- c(nov_3_reg_label, nov_3_label_r, nov_3_label_d)
    
    
    # Nov_3_no_reg labels
    total_nov_3_no_reg_votes <- nov_3_no_reg$Total[1] + nov_3_no_reg$Total[2]
    nov_3_no_reg_label_r <- paste("Trump (R)\n", percent(round((nov_3_no_reg$`Total`[1]/total_nov_3_no_reg_votes), 4), accuracy=0.01), "\n", format(nov_3_no_reg$`Total`[1], big.mark=","), sep="")
    nov_3_no_reg_label_d <- paste("Biden (D)\n", percent(round((nov_3_no_reg$`Total`[2]/total_nov_3_no_reg_votes), 4), accuracy=0.01), "\n", format(nov_3_no_reg$`Total`[2],big.mark=","), sep="")
    
    nov_3_no_reg$Labels <- c(nov_3_no_reg_label_r, nov_3_no_reg_label_d)
    
    no_reg_chart_subtitle <- paste("As Reported by", this_chart_subtitle)
    
    standard_chart_subtitle <- paste("As Reported by", this_chart_subtitle, est_vote_type)
    
    nov_3_chart_filename <- paste("output/", file_prefix, "ga_est.png", sep="")
    
    nov_3_no_reg_filename <- paste("output/", file_prefix, "ga_no_est.png", sep="")
    
    # Nov 3 Stacked Bar Graphs
    
    nov_3_colors <- c("#666666", "red3", "blue3")
    
    nov_3_chart <- stacked_bar(nov_3, nov_3_colors, standard_chart_subtitle)
    
    ggsave(nov_3_chart_filename, nov_3_chart, width = 14, height=2.5, units="in")
    
    # Nov 3 No Reg Stacked Bar Graphs
    
    nov_3_no_reg_colors <- c("red3", "blue3")
    
    nov_3_no_reg_chart <- stacked_bar(nov_3_no_reg, nov_3_no_reg_colors, no_reg_chart_subtitle)
    
    ggsave(nov_3_no_reg_filename, nov_3_no_reg_chart, width = 14, height=2.5, units="in")
}


# 11Alive out of Atlanta \Source https://www.youtube.com/watch?v=M-7izWMoG-s For both est and final, calculate the total voter turnout used by either the model or the final resuls. Then subtract all votes cast for a 3rd party.

alive11_nov_4_0105_est <- calc_est_voters("11Alive", 0.85, 2256527, 0.53, 1982826, 0.46)


alive11_nov_4_0105_reg_chart <- fake_real_news("Registered Voters", alive11_nov_4_0105_est$Registered.Voters.No.Third[1], alive11_nov_4_0105_est$Republican.Votes[1], alive11_nov_4_0105_est$Democratic.Votes[1], "11Alive on Nov 4 (1:05 AM)", "with Registered Voters", "alive11_nov_4_0105_reg_")


alive11_nov_4_0105_est_chart <- fake_real_news("Est",alive11_nov_4_0105_est$Network.Est.No.Third[1], alive11_nov_4_0105_est$Republican.Votes[1], alive11_nov_4_0105_est$Democratic.Votes[1], "11Alive on Nov 4 (1:05 AM)", "with 85% of AP Estimated Vote In", "alive11_nov_4_0105_est_")


alive11_nov_4_0105_final <- fake_real_news("", alive11_nov_4_0105_est$Actual.Votes.No.Third[1], alive11_nov_4_0105_est$Republican.Votes[1], alive11_nov_4_0105_est$Democratic.Votes[1], "11Alive on Nov 4 (1:05 AM)", "with Official Turnout", "alive11_nov_4_0105_final_")

# Source: Fox News Election night coverage https://www.youtube.com/watch?v=u5vQPo31lYA . For both est and final, calculate the total voter turnout used by either the model or the final resuls. Then subtract all votes cast for a 3rd party.

fox_nov_4_1235_est <- calc_est_voters("Fox News", 0.83, 2210672, 0.531, 1899647, 0.456)


fox_nov_4_1235_reg_chart <- fake_real_news("Registered Voters", fox_nov_4_1235_est$Registered.Voters.No.Third[1], fox_nov_4_1235_est$Republican.Votes[1], fox_nov_4_1235_est$Democratic.Votes[1], "Fox News on Nov 4 (12:35 AM)", "with Registered Voters", "fox_nov_1235_reg_")

# Adjust turnout number for Jorgenson

fox_nov_4_1235_est_chart <- fake_real_news("Est", fox_nov_4_1235_est$Network.Est.No.Third[1], fox_nov_4_1235_est$Republican.Votes[1], fox_nov_4_1235_est$Democratic.Votes[1], "Fox News on Nov 4 (12:35 AM) - 83% In", "", "fox_nov_4_1235_est_")

# Adjust turnout number for jorgenson

fox_nov_4_1235_final_chart <- fake_real_news("", fox_nov_4_1235_est$Actual.Votes.No.Third, fox_nov_4_1235_est$Republican.Votes[1], fox_nov_4_1235_est$Democratic.Votes[1], "Fox News on Nov 4 (12:35 AM)", "with Official Turnout", "fox_nov_4_1235_final_")

diff_r_d <- fox_nov_4_1235_est$Republican.Votes - fox_nov_4_1235_est$Democratic.Votes

diff_final_fox <- as.numeric(actual_total_votes) - fox_nov_4_1235_est$Republican.Votes - fox_nov_4_1235_est$Democratic.Votes - fox_nov_4_1235_est$Third.Party.Votes

per_diff_final_fox <- diff_final_fox / actual_total_votes

fox_nov_4_1235_total <- fox_nov_4_1235_est$Republican.Votes + fox_nov_4_1235_est$Democratic.Votes + fox_nov_4_1235_est$Third.Party.Votes

# 2016 Comparison

## Source: https://results.enr.clarityelections.com/GA/63991/184321/en/summary.html

pres_total_2016 <- 4092373

diff_pres_2020_2016 <- actual_total_votes - pres_total_2016

pres_2016_fox_1104_1235 <- - pres_total_2016 + fox_nov_4_1235_total

# Early vote total Nov 2
# Source: https://web.archive.org/web/20201103163532/https://electproject.github.io/Early-Vote-2020G/GA.html
nov_2_2020_early_total <- 3912819

percent_2020_early_2016_total <- nov_2_2020_early_total / pres_total_2016

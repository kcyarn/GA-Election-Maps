library(extrafont)

# Set the working directory
setwd("C:/Users/kcyar/OneDrive - The Index Writer, LLC/Medium/better_ga_map")

# These commands do not need to be run every time.
font_import()
loadfonts(device = "win")

save.image(file="fonts.Rdata")

##### GROUPER DIET PROJECT: SIZE DISTRIBUTION OF GROUPER ######

# The following script visually examines the size distribution of the 6 grouper
# species analyzed in this study.


# Set-Up =======================================================================

# Packages
library(dplyr)
library(ggplot2)

# Data 
grouper_data <- read.csv("/Users/irisgeorge/Documents/Local-Documents/Grouper_Diet_Study/grouper-diet-study/Dataframes/grouper_collection_data.csv")


# Summary Size Stats ===========================================================

# Calculating the sample size, mean, minimum, and maximum sizes of each grouper 
# species as well as the standard deviation. 

# species list
unique(grouper_data$species)

# nassau grouper 
length(which(grouper_data$species == "nassau.grouper")) # n = 121
mean(grouper_data[grouper_data$species == "nassau.grouper", "TL.mm"]) # 481mm
min(grouper_data[grouper_data$species == "nassau.grouper", "TL.mm"]) # 240mm
max(grouper_data[grouper_data$species == "nassau.grouper", "TL.mm"]) # 760mm
sd(grouper_data[grouper_data$species == "nassau.grouper", "TL.mm"]) # 118mm

# yellowfin grouper
length(which(grouper_data$species == "yellowfin.grouper")) # n = 36
mean(grouper_data[grouper_data$species == "yellowfin.grouper", "TL.mm"]) # 568mm
min(grouper_data[grouper_data$species == "yellowfin.grouper", "TL.mm"]) # 285mm
max(grouper_data[grouper_data$species == "yellowfin.grouper", "TL.mm"]) # 930mm
sd(grouper_data[grouper_data$species == "yellowfin.grouper", "TL.mm"]) # 119mm

# black grouper
length(which(grouper_data$species == "black.grouper")) # n = 36
mean(grouper_data[grouper_data$species == "black.grouper", "TL.mm"]) # 522mm
min(grouper_data[grouper_data$species == "black.grouper", "TL.mm"]) # 310mm
max(grouper_data[grouper_data$species == "black.grouper", "TL.mm"]) # 1140mm
sd(grouper_data[grouper_data$species == "black.grouper", "TL.mm"]) # 200mm

# redhind
length(which(grouper_data$species == "redhind")) # n = 54
mean(grouper_data[grouper_data$species == "redhind", "TL.mm"]) # 361mm
min(grouper_data[grouper_data$species == "redhind", "TL.mm"]) # 270mm
max(grouper_data[grouper_data$species == "redhind", "TL.mm"]) # 710mm
sd(grouper_data[grouper_data$species == "redhind", "TL.mm"]) # 103mm

# tiger grouper
length(which(grouper_data$species == "tiger.grouper")) # n = 2
mean(grouper_data[grouper_data$species == "tiger.grouper", "TL.mm"]) # 405mm
min(grouper_data[grouper_data$species == "tiger.grouper", "TL.mm"]) # 400mm
max(grouper_data[grouper_data$species == "tiger.grouper", "TL.mm"]) # 410mm
sd(grouper_data[grouper_data$species == "tiger.grouper", "TL.mm"]) # 7mm

# graysby 
length(which(grouper_data$species == "graysby")) # n = 1, length = 260mm


# Size Boxplot =================================================================

# Creating a boxplot displaying the size distribution of each grouper species. 

ggplot(grouper_data, aes(x = species, y = TL.mm)) +
  geom_boxplot(aes(fill = as.factor(species)), show.legend = FALSE) + 
  theme_classic() + 
  xlab("Species") + 
  ylab("Total Length (mm)") +
  scale_x_discrete(labels = c("Black Grouper", "Graysby", "Nassau Grouper", "Redhind", "Tiger Grouper", "Yellowfin Grouper")) +
  theme(axis.title = element_text(size = 22)) + 
  theme(axis.text= element_text(size = 12)) + 
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "Set3")










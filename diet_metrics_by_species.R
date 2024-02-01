##### GROUPER DIET PROJECT: CALCULATING DIET METRICS BY SPECIES ######

# The following script calculates diet metrics and indices for the contents of 
# grouper stomachs collected from the Bahamas. The four metrics calculated are:
# 1) %N (percent composition by number), 2) %V (percent composition by volume),
# 3) %F (percent frequency of occurrence), and 4) IRI (index of relative 
# importance). Metrics are calculated for diet items seen in each of 6 grouper
# species


# Set-Up =======================================================================

# Packages
library(tidyverse)
library(dplyr)

# Data 
stomach_contents <- read.csv("/Users/irisgeorge/Documents/Local-Documents/Grouper_Diet_Study/grouper-diet-study/Dataframes/grouper_stomach_data.csv")
grouper_data <-  read.csv("/Users/irisgeorge/Documents/Local-Documents/Grouper_Diet_Study/grouper-diet-study/Dataframes/grouper_collection_data.csv")


# Creating Full Dataframe ======================================================

# Combining stomach_contents and grouper_data into one dataframe with diet items
# and grouper species.

# select relevant columns (grouper ID and species)
grouper_species <- grouper_data[,c("grouper.ID", "species")]

# merge dataframes
stomach_contents <- left_join(stomach_contents, grouper_species, by = "grouper.ID")

# remove non-food items 
stomach_contents <- subset(stomach_contents, !(stomach_contents$broad.taxa %in% 
                                                 c("empty", "mush", "chlorophyta", "coral", "algae", "rock")))

  
# Frequency Calculation ========================================================

# Calculate frequency of diet contents: the number of stomachs containing that 
# food item regardless of quantity; can be described as a percentage of stomachs 
# with that item (%F). Calculating for broad taxa by species. 

# assign new dataframe
frequency_data <- stomach_contents[,c("grouper.ID", "species", "broad.taxa")]

# subset for only distinct rows
frequency_data <- distinct(frequency_data, .keep_all = FALSE)

# calculate the number of stomachs containing each diet item
frequency_data <- frequency_data %>% 
  group_by(species, broad.taxa) %>% 
  add_count(name = "taxa.frequency")

# calculate taxa frequency 
taxa_frequency <- frequency_data[,c("species", "broad.taxa", "taxa.frequency")]
taxa_frequency <- distinct(taxa_frequency)

# split into dataframe/species and add average frequency 
nassau_frequency <- subset(taxa_frequency, species == "nassau.grouper")
nassau_frequency$mean.frequency <- nassau_frequency$taxa.frequency/sum(nassau_frequency$taxa.frequency) 

# split into dataframes for rest of the species, then merge back together 


  
  
  
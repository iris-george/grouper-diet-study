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

# calculate the number of stomachs for each species 
species_n <- frequency_data[,c("grouper.ID", "species")]
species_n <- distinct(species_n)

length(which(species_n$species == "nassau.grouper")) # 55 nassau stomachs
length(which(species_n$species == "yellowfin.grouper")) # 12 yellowfin stomachs
length(which(species_n$species == "black.grouper")) # 8 black stomachs 
length(which(species_n$species == "redhind")) # 19 redhind stomachs 
length(which(species_n$species == "tiger.grouper")) # 1 tiger stomach

# remove tiger grouper: only 1 stomach with food content 
frequency_data <- frequency_data[!(frequency_data$species %in% "tiger.grouper"),]

# calculate the number of stomachs containing each diet item
frequency_data <- frequency_data %>% 
  group_by(species, broad.taxa) %>% 
  add_count(name = "taxa.frequency")

# calculate taxa frequency 
taxa_frequency <- frequency_data[,c("species", "broad.taxa", "taxa.frequency")]
taxa_frequency <- distinct(taxa_frequency)

# split into dataframe/species and add average frequency 
nassau_frequency <- subset(taxa_frequency, species == "nassau.grouper")
nassau_frequency$mean.frequency <- nassau_frequency$taxa.frequency/55

yellowfin_frequency <- subset(taxa_frequency, species == "yellowfin.grouper")
yellowfin_frequency$mean.frequency <- yellowfin_frequency$taxa.frequency/12

black_frequency <- subset(taxa_frequency, species == "black.grouper")
black_frequency$mean.frequency <- black_frequency$taxa.frequency/8

redhind_frequency <- subset(taxa_frequency, species == "redhind")
redhind_frequency$mean.frequency <- redhind_frequency$taxa.frequency/19

tiger_frequency <- subset(taxa_frequency, species == "tiger.grouper")
tiger_frequency$mean.frequency <- tiger_frequency$taxa.frequency/1 

# merge species dataframes back together 
avg_taxa_frequency <- rbind(nassau_frequency, yellowfin_frequency, black_frequency, redhind_frequency)

# add in frequency = 0 rows 
yellowfin_echinodermata <- data.frame("yellowfin.grouper", "echinodermata", 0, 0)
names(yellowfin_echinodermata) <- c("species", "broad.taxa", "taxa.frequency", "mean.frequency")

black_crustacea <- data.frame("black.grouper", "crustacea", 0, 0)
names(black_crustacea) <- c("species", "broad.taxa", "taxa.frequency", "mean.frequency")

black_echinodermata <- data.frame("black.grouper", "echinodermata", 0, 0)
names(black_echinodermata) <- c("species", "broad.taxa", "taxa.frequency", "mean.frequency")

redhind_echinodermata <- data.frame("redhind", "echinodermata", 0, 0)
names(redhind_echinodermata) <- c("species", "broad.taxa", "taxa.frequency", "mean.frequency")

avg_taxa_frequency <- rbind(avg_taxa_frequency, yellowfin_echinodermata, black_crustacea, black_echinodermata, redhind_echinodermata)

# export frequency dataframe 
write.csv(avg_taxa_frequency, file = "taxa_frequency.csv", row.names = FALSE)


# Abundance Calculation ========================================================

# Calculate abundance of diet contents: the number of individuals in each 
# stomach recorded and expressed as a proportion of the total individuals 
# present in the stomach (%N); the average %N can be calculated across all 
# stomachs for each grouper species. Calculating for broad taxa by species. 

# In each stomach that contains food, what is the average proportion of those
# items that belong to a certain taxa? 

# assign new dataframe
abundance_data <- stomach_contents[,c("grouper.ID", "species", "broad.taxa")]

# calculate total number of species in each stomach
abundance_data <- abundance_data %>% 
  group_by(grouper.ID) %>% 
  add_count(name = "total.n")

# calculate total number of individuals of each taxa in each stomach 
abundance_data <- abundance_data %>% 
  group_by(grouper.ID, broad.taxa) %>% 
  add_count(name = "n.taxa")

# collapse by unique grouper.ID/stomach.item combo
abundance_data <- distinct(abundance_data)

# calculate proportion of total individuals 
abundance_data$proportion.n <- abundance_data$n.taxa/abundance_data$total.n

# calculate average abundance of each diet item
average_abundance <- abundance_data[,c("species", "broad.taxa", "proportion.n")]
average_abundance <- average_abundance %>% 
  group_by(species) %>% 
  summarise(mean.abundance = mean(proportion.n))












  
  
  
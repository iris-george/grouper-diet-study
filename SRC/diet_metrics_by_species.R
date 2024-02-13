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

# assign new dataframe
abundance_data <- stomach_contents[,c("grouper.ID", "species", "broad.taxa")]

# calculate total number of taxa in each stomach
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

# remove tiger grouper: only 1 stomach with food content 
abundance_data <- abundance_data[!(abundance_data$species %in% "tiger.grouper"),]

# split into dataframe for each grouper species
black_abun <- abundance_data %>% filter(species == "black.grouper")
yellowfin_abun <- abundance_data %>% filter(species == "yellowfin.grouper")
nassau_abun <- abundance_data %>% filter(species == "nassau.grouper")
redhind_abun <- abundance_data %>% filter(species == "redhind")

# add total # of stomachs for each species 
black_abun$n.stomachs <- n_distinct(black_abun$grouper.ID)
yellowfin_abun$n.stomachs <- n_distinct(yellowfin_abun$grouper.ID)
nassau_abun$n.stomachs <- n_distinct(nassau_abun$grouper.ID)
redhind_abun$n.stomachs <- n_distinct(redhind_abun$grouper.ID)

# calculate average abundance per row 
black_abun$avg.abun <- black_abun$proportion.n / black_abun$n.stomachs
yellowfin_abun$avg.abun <- yellowfin_abun$proportion.n / yellowfin_abun$n.stomachs
nassau_abun$avg.abun <- nassau_abun$proportion.n / nassau_abun$n.stomachs
redhind_abun$avg.abun <- redhind_abun$proportion.n / redhind_abun$n.stomachs

# collapse by taxa 
black_abun <- black_abun %>% group_by(broad.taxa) %>% summarise(avg.prop.abun = sum(avg.abun))
black_abun$species <- "black.grouper"

yellowfin_abun <- yellowfin_abun %>% group_by(broad.taxa) %>% summarise(avg.prop.abun = sum(avg.abun))
yellowfin_abun$species <- "yellowfin.grouper"

nassau_abun <- nassau_abun %>% group_by(broad.taxa) %>% summarise(avg.prop.abun = sum(avg.abun))
nassau_abun$species <- "nassau.grouper"

redhind_abun <- redhind_abun %>% group_by(broad.taxa) %>% summarise(avg.prop.abun = sum(avg.abun))
redhind_abun$species <- "redhind"

# merge back together
average_abundance <- rbind(black_abun, yellowfin_abun, nassau_abun, redhind_abun)

# add missing rows 
yellowfin_echinodermata <- data.frame("echinodermata", 0, "yellowfin.grouper")
names(yellowfin_echinodermata) <- c("broad.taxa", "avg.prop.abun", "species")

black_crustacea <- data.frame("crustacea", 0, "black.grouper")
names(black_crustacea) <- c("broad.taxa", "avg.prop.abun", "species")

black_echinodermata <- data.frame("echinodermata", 0, "black.grouper")
names(black_echinodermata) <- c("broad.taxa", "avg.prop.abun", "species")

redhind_echinodermata <- data.frame("echinodermata", 0, "redhind")
names(redhind_echinodermata) <- c("broad.taxa", "avg.prop.abun", "species")

average_taxa_abundance <- rbind(average_abundance, yellowfin_echinodermata, black_crustacea, black_echinodermata, redhind_echinodermata)

# export abundance dataframe 
write.csv(average_taxa_abundance, file = "taxa_abundance.csv", row.names = FALSE)


# Volume Calculation ===========================================================

# Calculate volume of diet contents: the volume of the stomach filled by 
# individuals of each taxa, expressed as percentage of the total volume occupied 
# (%V). Calculating for broad taxa by species. 

# assign new dataframe
volume_data <- stomach_contents[,c("grouper.ID", "species", "broad.taxa", "volume.ml")] 

# remove <1ml volumes and NA values 
volume_data <- volume_data[!(volume_data$volume.ml %in% "<1"),]
volume_data <- volume_data[!(volume_data$volume.ml %in% "<0.01"),]
volume_data <- volume_data[!(volume_data$volume.ml %in% "na"),]
volume_data <- transform(volume_data, volume.ml = as.numeric(volume.ml))

# calculate volume of each taxa per stomach 
volume_data <- volume_data %>% 
  group_by(grouper.ID, species, broad.taxa) %>% 
  summarise(taxa.vol = sum(volume.ml))

# calculate total volume occupied in each stomach
stomach_volume <- volume_data %>% 
  group_by(grouper.ID, species) %>% 
  summarise(total.vol = sum(taxa.vol))

# merge total stomach volume to volume dataframe 
volume_data <- merge(volume_data, stomach_volume, by = c("grouper.ID", "species"), all = TRUE)

# calculate proportion of total volume occupied by each taxa per stomach
volume_data$prop.vol <- volume_data$taxa.vol / volume_data$total.vol

# split into dataframe for each grouper species
black_vol <- volume_data %>% filter(species == "black.grouper")
yellowfin_vol <- volume_data %>% filter(species == "yellowfin.grouper")
nassau_vol <- volume_data %>% filter(species == "nassau.grouper")
redhind_vol <- volume_data %>% filter(species == "redhind")

# add total # of stomachs for each species 
black_vol$n.stomachs <- n_distinct(black_vol$grouper.ID)
yellowfin_vol$n.stomachs <- n_distinct(yellowfin_vol$grouper.ID)
nassau_vol$n.stomachs <- n_distinct(nassau_vol$grouper.ID)
redhind_vol$n.stomachs <- n_distinct(redhind_vol$grouper.ID)

# calculate average volume per row 
black_vol$avg.vol <- black_vol$prop.vol / black_vol$n.stomachs
yellowfin_vol$avg.vol <- yellowfin_vol$prop.vol / yellowfin_vol$n.stomachs
nassau_vol$avg.vol <- nassau_vol$prop.vol / nassau_vol$n.stomachs
redhind_vol$avg.vol <- redhind_vol$prop.vol / redhind_vol$n.stomachs

# collapse by taxa 
black_vol <- black_vol %>% group_by(broad.taxa) %>% summarise(avg.prop.vol = sum(avg.vol))
black_vol$species <- "black.grouper"

yellowfin_vol <- yellowfin_vol %>% group_by(broad.taxa) %>% summarise(avg.prop.vol = sum(avg.vol))
yellowfin_vol$species <- "yellowfin.grouper"

nassau_vol <- nassau_vol %>% group_by(broad.taxa) %>% summarise(avg.prop.vol = sum(avg.vol))
nassau_vol$species <- "nassau.grouper"

redhind_vol <- redhind_vol %>% group_by(broad.taxa) %>% summarise(avg.prop.vol = sum(avg.vol))
redhind_vol$species <- "redhind"

# merge back together
average_volume <- rbind(black_vol, yellowfin_vol, nassau_vol, redhind_vol)

# add missing rows 
yellowfin_echinodermata <- data.frame("echinodermata", 0, "yellowfin.grouper")
names(yellowfin_echinodermata) <- c("broad.taxa", "avg.prop.vol", "species")

yellowfin_crustacea <- data.frame("crustacea", 0, "yellowfin.grouper")
names(yellowfin_crustacea) <- c("broad.taxa", "avg.prop.vol", "species")

black_crustacea <- data.frame("crustacea", 0, "black.grouper")
names(black_crustacea) <- c("broad.taxa", "avg.prop.vol", "species")

black_echinodermata <- data.frame("echinodermata", 0, "black.grouper")
names(black_echinodermata) <- c("broad.taxa", "avg.prop.vol", "species")

redhind_echinodermata <- data.frame("echinodermata", 0, "redhind")
names(redhind_echinodermata) <- c("broad.taxa", "avg.prop.vol", "species")

average_taxa_volume <- rbind(average_volume, yellowfin_echinodermata, yellowfin_crustacea, black_crustacea, black_echinodermata, redhind_echinodermata)

# export volume dataframe 
write.csv(average_taxa_volume, file = "taxa_volume.csv", row.names = FALSE)  


# IRI Calculation ==============================================================

# Calculate the index of relative important (IRI) for each taxa: combines 
# the amount of each item and its bulk in the stomach (IRI = (%N + %V)*%F)

# merge %N (abundance), %V (volume), and %F (frequency) dataframes 
iri_data <- avg_taxa_frequency %>% 
  left_join(average_taxa_abundance, by = c("species", "broad.taxa")) %>% 
  left_join(average_taxa_volume, by = c("species", "broad.taxa"))

# calculate IRI
iri_data$iri <- (iri_data$avg.prop.abun + iri_data$avg.prop.vol)*iri_data$mean.frequency 

# remove taxa.frequency column
iri_data <- subset(iri_data, select = -c(taxa.frequency))

# calculate total iri per species 
total_iri <- iri_data %>% 
  group_by(species) %>% 
  summarise(total.iri = sum(iri))

# join total iri to main dataframe
iri_data <- merge(iri_data, total_iri, by = "species", all = TRUE)

# calculate proportion iri 
iri_data$prop.iri <- iri_data$iri / iri_data$total.iri

# select relevant columns
iri_data <- iri_data[,c("species", "broad.taxa", "iri", "total.iri", "prop.iri")]

# export iri dataframe 
write.csv(iri_data, file = "taxa_iri.csv", row.names = FALSE)  


# Create Full Dataframe ========================================================

# The following creates a dataframe of all diet metrices for each grouper 
# species across all broad taxa groups. 

# join all dataframes
diet_metrics <- avg_taxa_frequency %>%
  left_join(average_taxa_abundance, by = c("species", "broad.taxa")) %>% 
  left_join(average_taxa_volume, by = c("species", "broad.taxa")) %>%
  left_join(iri_data, by = c("species", "broad.taxa"))

# remove unwanted columns
diet_metrics <- subset(diet_metrics, select = -c(taxa.frequency, iri, total.iri))

# export diet metrics dataframe
write.csv(diet_metrics, file = "taxa_diet_metrics.csv", row.names = FALSE)  





##### GROUPER DIET PROJECT: DIET METRICS BY SPECIES BARPLOTS ######

# The following script creates barplots for calculated diet metrices across
# four grouper species collected in the Bahamas: black grouper, yellowfin 
# grouper, nassau grouper, and redhind. The four metrics utilized are:
# 1) %N (percent composition by number), 2) %V (percent composition by volume),
# 3) %F (percent frequency of occurrence), and 4) IRI (index of relative 
# importance). 


# Set-Up =======================================================================

# Packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(wesanderson)

# Data 
diet_metrics <- read.csv("/Users/irisgeorge/Documents/Local-Documents/Grouper_Diet_Study/grouper-diet-study/Dataframes/taxa_diet_metrics.csv")


# Create Individual Bar Plots ==================================================

# The following creates individual barplots for each diet metric for each 
# grouper species.

# create dataframes for each species
nas <- subset(diet_metrics, species == "nassau.grouper")
red <- subset(diet_metrics, species == "redhind")
blk <- subset(diet_metrics, species == "black.grouper")
yel <- subset(diet_metrics, species == "yellowfin.grouper")

# get colour palettes
names(wes_palettes)
wes_palette("Zissou1")
wes_palette("Royal1")
wes_palette("Royal2")
wes_palette("FantasticFox1")

# average % frequency of occurrence plot
frequency_plot <- ggplot(diet_metrics, aes(x = species, y = mean.frequency, fill = broad.taxa)) +
  geom_bar(stat = "identity", width = 0.5) +
  xlab("Species") +
  ylab("% Occurrence") +
  theme_classic() +
  coord_flip() +
  scale_fill_manual(values=wes_palette("Zissou1"))

# average % abundance plot
abundance_plot <- ggplot(diet_metrics, aes(x = species, y = avg.prop.abun, fill = broad.taxa)) +
  geom_bar(stat = "identity", width = 0.5) +
  xlab("Species") +
  ylab("% Number") +
  theme_classic() +
  coord_flip() +
  scale_fill_manual(values=wes_palette("Zissou1"))

# average % volume plot
volume_plot <- ggplot(diet_metrics, aes(x = species, y = avg.prop.vol, fill = broad.taxa)) +
  geom_bar(stat = "identity", width = 0.5) + 
  xlab("Species") +
  ylab("% Volume") +
  theme_classic() +
  coord_flip() +
  scale_fill_manual(values=wes_palette("Zissou1"))

# average % IRI plot
iri.plot <- ggplot(diet_metrics, aes(x = species, y = prop.iri, fill = broad.taxa)) +
  geom_bar(stat = "identity", width = 0.5) +
  xlab("Species") +
  ylab("% IRI") +
  theme_classic() +
  coord_flip() +
  scale_fill_manual(values=wes_palette("Zissou1"))


# Create Bar Plot of All Metrics ===============================================

# The following creates a stacked barplot of all diet metrics for each of the 
# four grouper species. 

# transform data from wide to long 
diet_metrics_long <- diet_metrics %>% 
  pivot_longer(cols = mean.frequency:prop.iri, 
                                  names_to = "metric",
                                  values_to = "avg.percent")

# rename metrics 
diet_metrics_long <- diet_metrics_long %>%
  mutate(metric = recode(metric, mean.frequency = "Occurrence", avg.prop.abun = "Abundance", avg.prop.vol = "Volume", prop.iri = "Relative Importance"))

# plot 
full_plot <- ggplot(diet_metrics_long, aes(x = species, y = avg.percent, fill = broad.taxa)) +
  geom_bar(stat = "identity", width = 0.35) +
  xlab("Species") +
  ylab("Percent") +
  theme_bw() +
  facet_grid(.~metric, scales = "free") +
  coord_flip() +
  scale_fill_manual(values=wes_palette("Zissou1"))
fig
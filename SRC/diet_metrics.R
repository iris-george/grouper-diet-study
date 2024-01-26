##### GROUPER DIET PROJECT: CALCULATING DIET METRICS ######

# The following script calculates diet metrics and indices for the contents of 
# grouper stomachs collected from the Bahamas. The four metrices calculated are:
# 1) %N (percent composition by number), 2) %V (percent composition by volume),
# 3) %F (percent frequency of occurrence), and 4) IRI (index of relative 
# importance). 


# Set-Up =======================================================================

# Packages
library(dplyr)

# Data 
stomach_contents <- read.csv("/Users/irisgeorge/Documents/Documents/Grouper-Diet-Study/grouper-diet-study/Dataframes/grouper_stomach_data.csv")


# Frequency Calculation ========================================================

# Calculate frequency of diet contents: the number of stomachs containing that 
# food item regardless of quantity; can be described as a percentage of stomachs 
# with that item (%F) 

# find unique diet items
unique(stomach_contents$stomach.item)

# assign new dataframe
frequency_data <- stomach_contents[,c("grouper.ID", "broad.taxa", 
                                      "stomach.item")]

# subset for only distinct rows
frequency_data <- distinct(frequency_data, .keep_all = FALSE)

# frequency of empty stomachs 
frequency_data$empty <- if_else(frequency_data$stomach.item == "empty", 1, 0)
n_distinct(frequency_data$grouper.ID) # 231 
sum(frequency_data$empty)/231 # 0.4112554 

# frequency of mush 
frequency_data$mush <- if_else(frequency_data$stomach.item == "mush", 1, 0) 
sum(frequency_data$mush)/231 # 0.2554113

# frequency of crab
frequency_data$crab <- if_else(frequency_data$stomach.item == "crab", 1, 0) 
sum(frequency_data$crab)/231 # 0.0952381

# frequency of fish
frequency_data$fish <- if_else(frequency_data$stomach.item == "fish", 1, 0) 
sum(frequency_data$fish)/231 # 0.1861472

# frequency of shrimp
frequency_data$shrimp <- if_else(frequency_data$stomach.item == "shrimp", 1, 0) 
sum(frequency_data$shrimp)/231 # 0.04761905

# frequency of spiny lobster
frequency_data$spiny.lobster <- if_else(frequency_data$stomach.item == "spiny.lobster", 1, 0) 
sum(frequency_data$spiny.lobster)/231 # 0.008658009

# frequency of red ridged clinging crab 
frequency_data$red.ridged.clinging.crab <- if_else(frequency_data$stomach.item == "red.ridged.clinging.crab", 1, 0) 
sum(frequency_data$red.ridged.clinging.crab)/231 # 0.02597403

# frequency of rough box crab 
frequency_data$rough.box.crab <- if_else(frequency_data$stomach.item == "rough.box.crab", 1, 0) 
sum(frequency_data$rough.box.crab)/231 # 0.01298701

# frequency of snapper
frequency_data$snapper <- if_else(frequency_data$stomach.item == "snapper", 1, 0) 
sum(frequency_data$snapper)/231 # 0.004329004

# frequency of mermaids tea cup
frequency_data$mermaids.tea.cup <- if_else(frequency_data$stomach.item == "mermaids.tea.cup", 1, 0) 
sum(frequency_data$mermaids.tea.cup)/231 # 0.004329004

# frequency of coral
frequency_data$coral <- if_else(frequency_data$stomach.item == "coral", 1, 0) 
sum(frequency_data$coral)/231 # 0.004329004

# frequency of creole wrasse 
frequency_data$creole.wrasse <- if_else(frequency_data$stomach.item == "creole.wrasse", 1, 0) 
sum(frequency_data$creole.wrasse)/231 # 0.004329004

# frequency of mantis shrimp
frequency_data$mantis.shrimp <- if_else(frequency_data$stomach.item == "mantis.shrimp", 1, 0) 
sum(frequency_data$mantis.shrimp)/231 # 0.01298701

# frequency of conch foot
frequency_data$conch.foot <- if_else(frequency_data$stomach.item == "conch.foot", 1, 0) 
sum(frequency_data$conch.foot)/231 # 0.008658009

# frequency of parrotfish
frequency_data$parrotfish <- if_else(frequency_data$stomach.item == "parrotfish", 1, 0) 
sum(frequency_data$parrotfish)/231 # 0.02164502

# frequency of surgeon fish
frequency_data$surgeon.fish <- if_else(frequency_data$stomach.item == "surgeon.fish", 1, 0) 
sum(frequency_data$surgeon.fish)/231 # 0.004329004

# frequency of seaweed
frequency_data$seaweed <- if_else(frequency_data$stomach.item == "seaweed", 1, 0) 
sum(frequency_data$seaweed)/231 # 0.004329004

# frequency of lobster
frequency_data$lobster <- if_else(frequency_data$stomach.item == "lobster", 1, 0) 
sum(frequency_data$lobster)/231 # 0.004329004

# frequency of grunt
frequency_data$grunt <- if_else(frequency_data$stomach.item == "grunt", 1, 0) 
sum(frequency_data$grunt)/231 # 0.004329004

# frequency of sea cucumber
frequency_data$sea.cucumber <- if_else(frequency_data$stomach.item == "sea.cucumber", 1, 0) 
sum(frequency_data$sea.cucumber)/231 # 0.004329004

# frequency of white speckled hermit crab
frequency_data$white.speckled.hermit.crab <- if_else(frequency_data$stomach.item == "white.speckled.hermit.crab", 1, 0) 
sum(frequency_data$white.speckled.hermit.crab)/231 # 0.004329004

# frequency of stoplight parrotfish
frequency_data$stoplight.parrotfish <- if_else(frequency_data$stomach.item == "stoplight.parrotfish", 1, 0) 
sum(frequency_data$stoplight.parrotfish)/231 # 0.004329004

# frequency of octopus
frequency_data$octopus <- if_else(frequency_data$stomach.item == "octopus", 1, 0) 
sum(frequency_data$octopus)/231 # 0.008658009

# frequency of crustacean
frequency_data$crustacean <- if_else(frequency_data$stomach.item == "crustacean", 1, 0) 
sum(frequency_data$crustacean)/231 # 0.01298701

# frequency of rock
frequency_data$rock <- if_else(frequency_data$stomach.item == "rock", 1, 0) 
sum(frequency_data$rock)/231 # 0.004329004

# frequency of isopod
frequency_data$isopod <- if_else(frequency_data$stomach.item == "isopod", 1, 0) 
sum(frequency_data$isopod)/231 # 0.008658009

# frequency of shell 
frequency_data$shell <- if_else(frequency_data$stomach.item == "shell", 1, 0) 
sum(frequency_data$shell)/231 # 0.004329004












# Script to pull species with lens data out of full trait database

#import full trait database 
trait.db <- data.frame(read.csv("Data/trait_database.csv", 
                                header=TRUE, na.strings=c("", "NA"))) 
#add column for genus_species
trait.db$genus_species <- as.factor(paste(trait.db$Genus, 
                                           trait.db$Species, 
                                           sep="_"))
#import our species data
species <- data.frame(read.csv("Data/species_data.csv", header=TRUE, na.strings=c("", "NA"))) 

#add genus_species column and pull out
species <- species %>%
  mutate(genus_species = gsub('\\s+', '_', species)) %>%
  select(genus_species)

#import published species data
yov_species <- data.frame(read.csv("Data/yovanovich_species.csv", header=TRUE, na.strings=c("", "NA"))) 

#add genus_species column and pull out
yov_species <- yov_species %>%
  mutate(genus_species = species) %>%
  select(genus_species)

#merge dataframes
sp_list <- full_join(species, yov_species, by = "genus_species")

#remove duplicates
sp_list <- unique(sp_list)

#subset trait database to sampled species only and desired traits only
trait.lens <- left_join(sp_list, trait.db, by = "genus_species")
  
#check for missed taxa
setdiff(sp_list$genus_species, trait.lens$genus_species)

#tidy up traits (keep only desired columns)
trait.export <- trait.lens %>%
  select(Order, Family, genus_species, 
         Adult_habitat, Activity_period, Sex_dichromatism, AH_source, AP_source, SD_source)
  
#export subsetted, tidied traits matching lens transmission data
write.csv(trait.export, file = "Data/lens_traits.csv")

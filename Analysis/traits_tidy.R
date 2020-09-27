# Script to pull species with lens data out of full trait database

#import full trait database 
trait.db <- data.frame(read.csv("../Data/trait_database.csv", 
                                header=TRUE, na.strings=c("", "NA"))) 
#add column for genus_species
trait.db$genus_species <- as.factor(paste(trait.db$Genus, 
                                           trait.db$Species, 
                                           sep="_"))
#import list of sampled species
sampling <- data.frame(read.csv("../Data/lens_species.csv", header=TRUE, na.strings=c("", "NA"))) 

#add column for genus_species
sampling$genus_species <- as.factor(paste(sampling$Genus, 
                                          sampling$Species, 
                                          sep="_"))

#pull out list of desired species
samps <- sampling$genus_species

#subset trait database to sampled species only and desired traits only
trait.lens <- trait.db[trait.db$genus_species %in% samps, ]
  
#check for missed taxa
setdiff(samps, trait.lens$genus_species)

#tidy up traits (keep only desired columns)
trait.export <- trait.lens %>%
  select(Order, Family, Subfamily, Genus, Species, genus_species, 
         Adult_habitat, Activity_period, Mating_habitat, 
         Life_history, Larval_habitat, Sex_dichromatism) %>%
  replace_na(list(Adult_habitat = "Unknown", Activity_period = "Unknown", Mating_habitat = "Unknown", Life_history = "Unknown", Larval_habitat = "Unknown",
                  Sex_dichromatism = "Unknown"))
  
#export subsetted, tidied traits matching lens transmission data
write.csv(trait.export, file = "../Data/lens_traits.csv")

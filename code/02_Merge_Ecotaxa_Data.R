#Merging all samples within a single month together and adding merged file to .Rproj 
##Set working dir to documents folder where all ecotaxa data is stored
setwd("C:/Users/schirmer_n/Documents/Data/Ecotaxa/Classified_data/2024_Jan")

##Using read_tsv to open all files within dir above then applying rbind function to generate single merged dataframe from all files at dir
merge <- do.call(rbind, lapply(list.files(), read_tsv))

##Writing merge object as a .csv into the Ecotaxa_Cell_Conc.Rproj and Github repository
write.csv(merge, "C:/Users/schirmer_n/Documents/Ecotaxa_Cell_Conc/data/01_2024_merged.csv")

##Checking that the file was added to the proper directory
list.files(here::here("data"))




 
  
  
  
  
  

##Loading in data
merge <- read.csv(
  
  file = here::here("data", "01_2024_merged.csv")
  
  )

#Changing vol_img column from character to numerical, removing "ml"
#Removing undesired object annotation categories (see details below)
merge_vol <- merge %>%
  
  mutate(
    
    acq_vol_img = as.numeric(substr(acq_vol_img, 1, nchar(acq_vol_img) - 3))
    
  ) %>% 
  
  filter(
    
    !object_annotation_category %in% rmv
    
  )

##Removing irrelevant columns
#when annotating image data it is helpful to apply specific classifiers to images to improve the application of machine learning tools, 
#as well as keeping track of trends in non-living objects; however, these classifiers serve no purpose to the study of living plankton
#assemblages. Additionally to increase the power of the data taxonomic classifications will be rolled back into broad group categories: 
rmv <- c(
  "duplicate",
  "pellet",
  "dark<detritus", 
  "unknown", 
  "big cone", 
  "pollen", 
  "flake", 
  "copepod moult", 
  "rods", 
  "filament<detritus", 
  "bubble", 
  "temporary<living"
)

rplc <- c(
  "Ciliophora" = "ciliate", 
  "Thalassionema" = "diatom_pennate_chain", 
  "flagellates-like" = "flagellates", 
  "Chaetocers single" = "diatom_centric", 
  "diatom-centric" = "diatom_centric", 
  "Cyanobacteria<Bacteria" = "cyanobacteria", 
  "ciliate-tintinnids" = "ciliate", 
  "cyanobacteria-like" = "cyanobacteria", 
  "Dinoflagellates" = "dinoflagellate", 
  "diatom-centric-chain" = "diatom_centric_chain", 
  "Nitzschia" = "diatom_pennate", 
  "Entomoneis" = "diatom_pennate", 
  "Achnanthes" = "diatom_pennate_chain", 
  "Bacillaria" = "diatom_pennate_chain", 
  "Corethron" = "diatom_centric", 
  "Raphoneis" = "diatom_pennate", 
  "ciliate-oval" = "ciliate", 
  "diatom-pennate" = "diatom_pennate", 
  "Bacillariophyceae" = "diatom", 
  "Pleurosigmataceae" = "diatom_pennate", 
  "Diploneis" = "diatom_penante", 
  "Foraminifera" = "foraminiferan", 
  "Asterionellopsis" = "diatom_pennate_chain", 
  "Skeletonema" = "diatom_centric_chain", 
  "ciliate-strombidiids" = "ciliate", 
  "diatom-pennate-chain" = "diatom_pennate_chain", 
  "Bacteriastrum<Chaetocerales" = "diatom_centric_chain", 
  "Bacillariophyta" = "diatom", 
  "Prorocentrum micans<Prorocentrum" = "dinoflagellate", 
  "Rhizosolenia" = "diatom_centric", 
  "Fragilariopsis" = "diatom_pennate_chain"
)

colnames(freq)

##Grouping classifications into broad classes according to the rplc vector above

merge_grpd <- merge_vol %>% 
  
  mutate(
    
    object_annotation_category = recode(.x = object_annotation_category, rplc)
    
    )





##Generating a frequency table from the raw data. The no. of images for 
#count() to generate n column
#mutate() populate a vol_img column from merge, changing to numeric, and matching to the sample_id
#introduce site_date column to easily collapse replicates
#pivot_wider() pivoting the data wider to show absences, calculating across easier than down
freq <- merge_vol %>% 
  
  dplyr::count(
    sample_id, 
    object_annotation_category
    ) %>% 
  
  dplyr::mutate(
    vol_img = merge_vol$acq_vol_img[match(sample_id, merge$sample_id)], 
    site_date = substr(sample_id, 1, 10)
    ) %>% 
  
  tidyr::pivot_wider(
    names_from = "object_annotation_category",
    values_from = "n"
    ) %>% 
  
  replace(is.na(.), 0)

##calculating cell concentration
#for most specific classifications
spc_cell_conc <- freq %>%  
  
  group_by(site_date) %>% 
  
  summarise(
    across(c(vol_img, Asterionellopsis:Corethron), sum)
  ) %>% 
  
  mutate(across(Asterionellopsis:Corethron, ~ .x/(vol_img/1000)), 
         across(Asterionellopsis:Corethron, round)
)

#for group-level classification
grpd_cell_conc <- 
##create group_id based on functional group associations




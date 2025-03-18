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
    
  ) %>% 
  
  mutate(
    
    object_annotation_category = forcats::fct_recode(object_annotation_category, rplc)
    
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
  "unknown big cone", 
  "pollen", 
  "flake", 
  "copepod moult", 
  "rods", 
  "filament<detritus", 
  "bubble", 
  "temporary<living", 
  "detritus", 
  "Lorica", 
  "Unknowns", 
  "frustule"
)

rplc <- c(
  "ciliate" = "Ciliophora", 
  "diatom.pennate.chain" = "Thalassionema", 
  "flagellates" = "flagellates-like", 
  "diatom.centric" = "Chaetoceros single", 
  "cyanobacteria" = "Cyanobacteria<Bacteria", 
  "ciliate" = "ciliate-tintinnids", 
  "diatom.centric" = "diatom-centric",
  "cyanobacteria" = "cyanobacteria-like", 
  "dinoflagellate" = "Dinoflagellates", 
  "diatom.centric.chain" = "diatom-centric-chain", 
  "diatom.pennate" = "Nitzschia", 
  "diatom.pennate" = "Entomoneis", 
  "diatom.pennate.chain" = "Achnanthes", 
  "diatom.pennate.chain" = "Bacillaria", 
  "diatom.centric" = "Corethron", 
  "diatom.pennate" = "Raphoneis", 
  "ciliate" = "ciliate-oval", 
  "diatom.pennate" = "diatom-pennate", 
  "diatom" = "Bacillariophyceae", 
  "diatom.pennate" = "Pleurosigmataceae", 
  "diatom.penante" = "Diploneis", 
  "foraminiferan" = "Foraminifera", 
  "diatom.pennate.chain" = "Asterionellopsis", 
  "diatom.centric.chain" = "Skeletonema", 
  "ciliate" = "ciliate-strombidiids", 
  "diatom.pennate.chain" = "diatom-pennate-chain", 
  "diatom.centric.chain" = "Bacteriastrum<Chaetocerales", 
  "diatom" = "Bacillariophyta", 
  "dinoflagellate" = "Prorocentrum micans<Prorocentrum", 
  "diatom.centric" = "Rhizosolenia", 
  "diatom.pennate.chain" = "Fragilariopsis"  
)

colnames(freq)

##Grouping classifications into broad classes according to the rplc vector above





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




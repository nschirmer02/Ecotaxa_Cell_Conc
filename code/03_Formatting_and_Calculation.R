##Loading in data
merge <- read.csv(
  
  file = here::here("data", "01_2024_merged.csv")
  
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
  "frustule", 
  "dinoflagellate-cysts"
)

##Generating group-level class column
#using the list below to generate a new column containing broader classifications for the data
#to allow for group-level analysis. When new names are added input them utilizing the following template: 

#object_annotation_category == "[Ecotaxa Classifier]" ~ "[Group Idenfication]"

replace <- c(
  object_annotation_category == "Ciliophora" ~ "ciliate", 
  object_annotation_category == "Thalassionema" ~ "diatom.pennate.chain",
  object_annotation_category == "flagellates-like" ~ "flagellates",
  object_annotation_category == "Chaetoceros single" ~ "diatom.centric",
  object_annotation_category == "Cyanobacteria<Bacteria" ~ "cyanobacteria",
  object_annotation_category == "ciliate-tintinnids" ~ "ciliate",
  object_annotation_category == "diatom-centric" ~ "diatom.centric",
  object_annotation_category == "cyanobacteria-like" ~ "cyanobacteria",
  object_annotation_category == "Dinoflagellates" ~ "dinoflagellate",
  object_annotation_category == "diatom-centric-chain" ~ "diatom.centric.chain",
  object_annotation_category == "Nitzschia" ~ "diatom.pennate", 
  object_annotation_category == "Entomoneis" ~ "diatom.pennate",
  object_annotation_category == "Achnanthes" ~ "diatom.pennate.chain",
  object_annotation_category == "Bacillaria" ~ "diatom.pennate.chain",
  object_annotation_category == "Corethron" ~ "diatom.centric",
  object_annotation_category == "Raphoneis" ~ "diatom.pennate",
  object_annotation_category == "ciliate-oval" ~ "ciliate",
  object_annotation_category == "diatom-pennate" ~ "diatom.pennate",
  object_annotation_category == "Bacillariophyceae" ~ "diatom",
  object_annotation_category == "Pleurosigmataceae" ~ "diatom.pennate",
  object_annotation_category == "Diploneis" ~ "diatom.penante", 
  object_annotation_category == "Foraminifera" ~ "foraminiferan",
  object_annotation_category == "Asterionellopsis" ~ "diatom.pennate.chain",
  object_annotation_category == "Skeletonema" ~ "diatom.centric.chain",
  object_annotation_category == "ciliate-strombidiids" ~ "ciliate",
  object_annotation_category == "diatom-pennate-chain" ~ "diatom.pennate.chain",
  object_annotation_category == "Bacteriastrum<Chaetocerales" ~ "diatom.centric.chain",
  object_annotation_category == "Bacillariophyta" ~ "diatom",
  object_annotation_category == "Prorocentrum micans<Prorocentrum" ~ "dinoflagellate",
  object_annotation_category == "Rhizosolenia" ~ "diatom.centric", 
  object_annotation_category == "Fragilariopsis" ~ "diatom.pennate.chain", 
  object_annotation_category == "cyanobacteria-like filaments" ~ "cyanobacteria", 
  object_annotation_category == "cyanobacteria-like" ~ "cyanobacteria"
)

##Changing vol_img column from character to numerical, removing "ml"
#Removing undesired object annotation categories (see details below)
merge_vol <- merge %>%
  
  dplyr::mutate(
    
    acq_vol_img = as.numeric(substr(acq_vol_img, 1, nchar(acq_vol_img) - 3)) 
    
  ) %>% 
  
  dplyr::filter(
    
    !object_annotation_category %in% rmv
    
  ) %>% 
  
  dplyr::mutate(
    
    object_annotation_grouping = case_when(
      object_annotation_category == "Ciliophora" ~ "ciliate", 
      object_annotation_category == "Thalassionema" ~ "diatom.pennate.chain",
      object_annotation_category == "flagellates-like" ~ "flagellates",
      object_annotation_category == "Chaetoceros single" ~ "diatom.centric",
      object_annotation_category == "Cyanobacteria<Bacteria" ~ "cyanobacteria",
      object_annotation_category == "ciliate-tintinnids" ~ "ciliate",
      object_annotation_category == "diatom-centric" ~ "diatom.centric",
      object_annotation_category == "cyanobacteria-like" ~ "cyanobacteria",
      object_annotation_category == "Dinoflagellates" ~ "dinoflagellate",
      object_annotation_category == "diatom-centric-chain" ~ "diatom.centric.chain",
      object_annotation_category == "Nitzschia" ~ "diatom.pennate", 
      object_annotation_category == "Entomoneis" ~ "diatom.pennate",
      object_annotation_category == "Achnanthes" ~ "diatom.pennate.chain",
      object_annotation_category == "Bacillaria" ~ "diatom.pennate.chain",
      object_annotation_category == "Corethron" ~ "diatom.centric",
      object_annotation_category == "Raphoneis" ~ "diatom.pennate",
      object_annotation_category == "ciliate-oval" ~ "ciliate",
      object_annotation_category == "diatom-pennate" ~ "diatom.pennate",
      object_annotation_category == "Bacillariophyceae" ~ "diatom",
      object_annotation_category == "Pleurosigmataceae" ~ "diatom.pennate",
      object_annotation_category == "Diploneis" ~ "diatom.penante", 
      object_annotation_category == "Foraminifera" ~ "foraminiferan",
      object_annotation_category == "Asterionellopsis" ~ "diatom.pennate.chain",
      object_annotation_category == "Skeletonema" ~ "diatom.centric.chain",
      object_annotation_category == "ciliate-strombidiids" ~ "ciliate",
      object_annotation_category == "diatom-pennate-chain" ~ "diatom.pennate.chain",
      object_annotation_category == "Bacteriastrum<Chaetocerotales" ~ "diatom.centric.chain",
      object_annotation_category == "Bacillariophyta" ~ "diatom",
      object_annotation_category == "Prorocentrum micans<Prorocentrum" ~ "dinoflagellate",
      object_annotation_category == "Rhizosolenia" ~ "diatom.centric", 
      object_annotation_category == "Fragilariopsis" ~ "diatom.pennate.chain", 
      object_annotation_category == "cyanobacteria-like filaments" ~ "cyanobacteria", 
      object_annotation_category == "cyanobacteria-like" ~ "cyanobacteria",
      object_annotation_category == "ciliate-other" ~ "ciliate"
                                           
                                           )
    
  )

##Generating a frequency table from the raw data. The no. of images for 
#count() to generate n column
#mutate() populate a vol_img column from merge, changing to numeric, and matching to the sample_id
#introduce site_date column to easily collapse replicates
#pivot_wider() pivoting the data wider to show absences, calculating across easier than down
#replace() to change all NA's generated by empty rows to 0

freq <- merge_vol %>% 
  
  dplyr::count(
    sample_id, 
    object_annotation_category
    ) %>% 
  
  dplyr::mutate(
    vol_img = merge_vol$acq_vol_img[match(sample_id, merge_vol$sample_id)], 
    site_date = substr(sample_id, 1, 10)
    ) %>% 
  
  tidyr::pivot_wider(
    names_from = "object_annotation_category",
    values_from = "n"
    ) %>% 
  
  replace(is.na(.), 0)

##calculating cell concentration
#for most specific classifications
spc_conc <- freq %>%  
  
  dplyr::group_by(site_date) %>% 
  
  dplyr::summarise(
    across(c(vol_img, Asterionellopsis:Corethron), sum)
  ) %>% 
  
  dplyr::mutate(across(Asterionellopsis:Corethron, ~ .x/(vol_img/1000)), 
         across(Asterionellopsis:Corethron, round)
)

#for group-level classification

grp_freq <- merge_vol %>% 
  
  dplyr::count(
    sample_id, 
    object_annotation_grouping
  ) %>% 
  
  dplyr::mutate(
    vol_img = merge_vol$acq_vol_img[match(sample_id, merge_vol$sample_id)], 
    site_date = substr(sample_id, 1, 10)
  ) %>% 
  
  tidyr::pivot_wider(
    names_from = "object_annotation_grouping",
    values_from = "n"
  ) %>% 
  
  replace(is.na(.), 0)

##calculating cell concentration
#for most specific classifications
grp_conc <- grp_freq %>%  
  
  dplyr::group_by(site_date) %>% 
  
  dplyr::summarise(
    across(c(vol_img, ciliate:foraminiferan), sum)
  ) %>% 
  
  dplyr::mutate(across(ciliate:foraminiferan, ~ .x/(vol_img/1000)), 
         across(ciliate:foraminiferan, round)
  )





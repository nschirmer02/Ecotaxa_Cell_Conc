#Changing vol_img column from character to numerical, removing "ml" 
merge <- read.csv(here::here(path = "data/01_2024_merged.csv"))

#Generating a frequency table with the number of images of each taxa by site
freq <- merge %>% 
  count(sample_id, object_annotation_category) %>% 
  mutate(
    vol_img = merge$acq_vol_img[match(sample_id, merge$sample_id)], 
    site_date = substr(sample_id, 1, 10)) %>%
  pivot_wider()


#calculating cell concentration
comb_conc <- freq %>% 
  group_by(site_date, object_annotation_category) %>% 
  summarise(
    n = sum(n)
  ) 
View(comb_conc)

#create group_id based on functional group associations
print(levels(factor(merge$sample_id)))

calc_conc <- function (x) {
  x <- x %>% 
    mutate(
      acq_vol_img = as.numeric(substr(acq_vol_img, 1, nchar(acq_vol_img) - 3)), 
      sample_id = factor(sample_id)
    )
  freq <- x %>% 
    count()
  
}

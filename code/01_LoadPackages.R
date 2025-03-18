#Used for streamlining package install and loading using a single function: p_load
#Can a list of required packages as demonstrated below, if you have not installed the package p_load will automatically install for you!
install.packages("pacman")

#readr: Package contains the read_tsv function. Ecotaxa files are in .tsv format and require this function to be read into R
#here: Streamlining file paths for calling in data
#dplyr/tidyverse: Packages utilized for efficient and readable dataframe mutations
#forcats: Easily manipulating categorical variables formatted as factors 
pacman::p_load(readr, here, dplyr, tidyverse, forcats)



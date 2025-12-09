# This file is for checking reads of indicator species list provided by Ben

#first load libraries
library(tidyverse)
library(dplyr)
library(stringr)
library(purrr)
library(ggrepel)
library(openxlsx)

############################################################### PREP #####################################################################################

#set wd and load data
getwd()
setwd("E:/Msc/Thesis/R")
df <- read_tsv("E:/Msc/Thesis/Data/Sequencing/beaver_250424_raw_taxonomy_data.tsv")
#this is for assigning whether genera is native, introduced or absent in our location
genera_status <- read.csv('E:/Msc/Thesis/Data/Sequencing/genera status list2.csv', header = TRUE, sep = ',')
# Make new, leaner df 
df_indicator <- df %>%
  filter( # exclude irrelevant libraries, bacteria, animals etc.
    !library %in% c("Lib.J.1035", "Lib.J.1048", 'Lib.J.1049')) %>% 
  filter(!phylum_name %in% c(
    "Chordata",
    "Annelida",
    "Arthropoda",
    "Mollusca",
    "Nematoda",
    "Acidobacteriota" ,
    "Actinomycetota"  , 
    "Pseudomonadota"  ,
    "Bacteroidota"    ,
    "Chloroflexota"   ,
    "Bacillota"       ,
    "Planctomycetota" ,
    "Chlamydiota"     ,
    "Myxococcota"     ,
    "Thermodesulfobacteriota",
    "Mycoplasmatota"  ,
    "Verrucomicrobiota",
    "Candidatus Methylomirabilota",
    "Gemmatimonadota" ,
    "Nitrospirota"    ,
    "Campylobacterota"))  %>% 
  select( # Next we select the relevant columns
    .,
    1,
    2,
    4,
    7:12,
    15,
    17,
    18,
    "sum_genus_count",
    "family_count_assembly",
    "deam5p_frac_genus",
    "deam3p_frac_genus",
    "median_read_length_genus",
    "ancientness_genus"
  ) %>%
  mutate(deamination_rate = (deam5p_frac_genus + deam3p_frac_genus) / 2)  %>% #add deamination rate
  mutate(location = case_when( # This section is so that we can run per location
    str_detect(external_id.x, "^M")  ~ "Moehne"  ,
    str_detect(external_id.x, "^W1") ~ "Wester 1",
    str_detect(external_id.x, "^W2") ~ "Wester 2"
  )) %>%
  group_by(library, genus_name) %>%
  filter(!genus_count_assembly < 10) %>% # this is to exclude low read entries
  left_join(genera_status, by = 'genus_name') %>% # add indicator list
  ungroup() %>%
  group_by(library)

# now we have our df, we must start checking with Ben's list
# load file
indicator_list <- read.xlsx("E:/Msc/Thesis/Data/indicator species.xlsx", sheet = 3, colNames = TRUE, rowNames = FALSE, skipEmptyCols = TRUE)

# 1 extract genus from xlsx file
genera_of_interest <- indicator_list %>%
  filter(Genus_in_DB == "Yes") %>%
  mutate(
    Indicator_species = str_trim(as.character(Indicator_species)),
    genus_name = word(Indicator_species, 1)
  ) %>%
  select(Indicator_species, genus_name)


# 3 filter for matching genera, keep taxa level 
df_filtered <- df_indicator %>%
  filter(genus_name %in% genera_of_interest$genus_name)

# 4 get all combinations of taxa and library
taxa_library_combos <-  df_filtered %>%
  distinct(genus_name, taxa_name, library)


# 5 add genus reads and classify based on this
result_table <- taxa_library_combos %>%
  left_join(df_indicator %>% select(genus_name, taxa_name, library, genus_count_assembly),
            by = c("genus_name", "taxa_name", "library")) %>%
  mutate(result = case_when(
    is.na(genus_count_assembly) ~ "not present",
    genus_count_assembly < 100 ~ "insufficient reads",
    TRUE ~ as.character(genus_count_assembly)
  ))

# 6 make each library a column: pivot wide
final_matrix <- result_table %>%
  pivot_wider(
    id_cols = c(genus_name, taxa_name),
    names_from  =  library,
    values_from =  result,
    names_prefix = "genus_reads_"
    
  )
# 7 save final table
write.xlsx(final_matrix, "indicator_genus_taxa_reads.xlsx")

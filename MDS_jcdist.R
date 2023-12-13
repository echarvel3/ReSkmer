### The BEGINNING ~~~~~
##
# ~ Plot a PCoa/MDS with jc distance matrix from skmer -distance | from Homère J. Alves Monteiro and Eduardo Charvel
# R 4.3.2 GUI 1.80 Big Sur ARM build (8281)

### Clearing environment and setting working directory
rm(list = ls(all = TRUE))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Loading necessary packages
library(tidyverse)
library(ape)
library(cowplot)
library(knitr)
library(scales)
library(miscTools)

### Sourcing required functions
source("individual_mdsGskim_functions_hjam.R")

### Reading and processing distance matrix
dist_jc <- read_tsv("../data/ref-dist-jc.txt", col_names = FALSE) %>%
  dplyr::select(1:nrow(.)) %>%
  as.matrix()

pairwise_distances <- dist_jc[-1, -1]
ibs_mat <- as.data.frame(as.matrix(pairwise_distances))
ibs_mat[is.na(ibs_mat) | ibs_mat == "nan"] <- 0

### Creating annotation file
sample_ids <- dist_jc[-1, 1]
cleaned_ids <- gsub("unclassified-kra_", "", sample_ids)
cleaned_ids <- gsub("_", "", cleaned_ids)
species_names <- rep("Clupea", length(sample_ids))

annotation_df <- data.frame(sample_id = sample_ids, cleaned_id = cleaned_ids, species = species_names)
write.csv(annotation_df, "../data/jcdist_Clupeasub_annot.csv", row.names = FALSE)

### Performing PCoA with initial annotation
PCoA(ibs_mat, annotation_df$cleaned_id, annotation_df$species, 3, 1, 2, show.ellipse = TRUE)

### Reading metadata and updating annotation_df
metadata_clupea <- read_tsv("../../Gskimming/Metadata/filereport_read_run_PRJEB52723_tsv (5).txt", col_names = TRUE)

annotation_df <- merge(annotation_df, metadata_clupea, by.x = "cleaned_id", by.y = "run_accession")

### Adding 'type' column based on sample_alias
annotation_df <- annotation_df %>%
  mutate(
    type = case_when(
      substr(sample_alias, 1, 8) == "BAL_22_M" ~ "modern",
      substr(sample_alias, 1, 8) == "BAL_22_H" ~ "ancient",
      TRUE ~ "unknown"
    )
  )

### Performing PCoA with updated annotation and type information
PCoA(ibs_mat, annotation_df$cleaned_id, annotation_df$type, 3, 1, 2, show.ellipse = FALSE, show.label = FALSE)

### Reading skims stats file
stat_clupea <- read_tsv("../data/stats-skims_processing_pipeline.csv", col_names = FALSE)
str(stat_clupea)
stat_clupea_df <- stat_clupea %>%
  pivot_wider(names_from = X2, values_from = X3) %>%
  rename(
    coverage = `coverage`,
    genome_length = `genome_length`,
    read_length = `read_length`,
    error_rate = `error_rate`
  )%>%
  mutate(X1 = str_extract(X1, "[^/]+$")) %>%
  mutate(X1 = gsub("unclassified-kra_", "",X1))%>%
  mutate(X1 = gsub("_", "",X1))

annotation_df_tot <- merge(annotation_df, stat_clupea_df, by.x = "cleaned_id", by.y = "X1")

# Range of coverage by adding CoverageCategory ~
annotation_df_tot$CoverageCategory <- ifelse(annotation_df_tot$coverage <= 0.7, "< 1x",
                           ifelse(annotation_df_tot$coverage <= 2, "< 2x",
                                  ifelse(annotation_df_tot$coverage <= 5, "< 5x",
                                         ifelse(annotation_df_tot$coverage <= 10, "< 10x",
                                                ifelse(annotation_df_tot$coverage <= 20, "< 20x",
                                                       ifelse(annotation_df_tot$coverage <= 30, "< 30x",
                                                              ifelse(annotation_df_tot$coverage <= 50, "< 50x", "Above 50x")))))))
                                                                     
                                                              
                                                       
                                                
annotation_df_tot$error_rateCategory <- ifelse(annotation_df_tot$error_rate <= 0.005, "< 0.5%",
                                             ifelse(annotation_df_tot$error_rate <= 0.01, "< 1%",
                                                    ifelse(annotation_df_tot$error_rate <= 0.05, "< 5%", "error rate dangerously high")))
                                                           


### 
PCoA2(ibs_mat, annotation_df_tot$cleaned_id, annotation_df_tot$CoverageCategory, 3, 1, 2, show.ellipse = FALSE, show.label = FALSE)
PCoA2(ibs_mat, annotation_df_tot$cleaned_id, annotation_df_tot$error_rateCategory, 3, 1, 2, show.ellipse = FALSE, show.label = FALSE)

#!/usr/bin/env Rscript

rm(list = ls())
setwd("C:/Users/PinWei/my_Haskins_project")

library(dplyr)
library(writexl)

DF <- readRDS( 
  file.path("Data", "Chang_et_al", "AoA_character_naming.rds")
) 
DF$character <- as.character(DF$character)

char_matrix <- DF %>%
  distinct(subject_id, character) %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from = subject_id,
    values_from = value,
    values_fill = 0  
  ) %>%
  arrange(character) %>% 
  column_to_rownames("character") %>%
  as.data.frame()

char_matrix <- 
  char_matrix[do.call(order, as.data.frame(-char_matrix)), ] %>% 
  rownames_to_column(., "character")

writexl::write_xlsx(
  char_matrix, 
  file.path("Data", "Chang_et_al", "subj_char_matrix.xlsx")
)
# 
# subj_char_list <- DF %>%
#   group_by(subject_id) %>%
#   summarise(chars = list(sort(character))) %>%
#   ungroup()
# 
# group_list <- list()
# 
# for ( x in seq_len(nrow(subj_char_list)) ) {
#   assigned <- FALSE
#   sid <- subj_char_list$subject_id[x]
#   chars <- subj_char_list$chars[[x]]
#   
#   for ( g in seq_along(group_list) ) {
#     common_chars <- intersect(chars, group_list[[g]]$char_union)
#     union_chars <- union(chars, group_list[[g]]$char_union)
#     jaccard <- length(common_chars) / length(union_chars)
# 
#     if ( jaccard >= 0.6 ) {
#       group_list[[g]]$sid_list <- c(group_list[[g]]$sid_list, sid)
#       group_list[[g]]$char_union <- union_chars
#       assigned <- TRUE
#       break
#     }
#   }
#   
#   if ( ! assigned ) {
#     group_list[[length(group_list) + 1]] <- list(
#       sid_list = c(sid), 
#       char_union = chars
#     )
#   }
# }
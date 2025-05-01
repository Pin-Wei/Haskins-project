#!/usr/bin/env Rscript

rm(list = ls())
setwd("C:/Users/PinWei/my_Haskins_project")

library(dplyr)
library(tidyr)
library(purrr)
# library(readxl)
library(writexl)
library(tmcn) # a text mining toolkit for Chinese

z <- 1 # whether to use z-scored data or not

input.folder <- file.path(
  "Data", "Chang_et_al"
)
csr.folder <- file.path(
  "..", "my_CSR", "Data_Linguistic", "Chang_et_al", "Naming"
)

DF.Chang.20 <- readRDS( # the trial-wise dataset
    file.path(input.folder, "AoA_character_naming.rds")
  ) %>%
  dplyr::rename(
    Char = character, 
    RT = rt, 
    zRT = z_rt
  ) %>%
  select(all_of(c(
    "Char", "subject_id", c("RT", "zRT")[1], 
    "LogCF", "NS", "CON", "PC", "SC", "SAR", "IMG", "AoA"
  ))) 

if ( z == 2 ) { # perform z-scoring
  DF.Chang.20 <- DF.Chang.20 %>% 
    # group_by(subject_id) %>% 
    dplyr::mutate(across(
      .cols = all_of(c("LogCF", "NS", "CON", "PC", "SC", "SAR", "IMG", "AoA")),
      .fns = ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
    )) # %>% 
    # ungroup() 
}

## save file-1
writexl::write_xlsx(
  DF.Chang.20, 
  file.path(input.folder, paste0("Chang_2020_", c("", "z_")[z], "CSR.xlsx"))
) 

## save file-2
DF.Chang.20 %>% 
  select(all_of(c(
    "LogCF", "NS", "CON", "PC", "SC", "SAR", "IMG", "AoA", c("RT", "zRT")[1]
  ))) %>% 
  writexl::write_xlsx(
    file.path(csr.folder, paste0("all_subjs_", c("raw_", "zvars_")[z], "20 (indv).xlsx"))
  ) 

DF.Chang.16 <- read.csv(
    file.path(input.folder, "Chang_Lee_2016.csv")
  ) %>% 
  dplyr::mutate(
    LogCF = log(Frequency)
  ) %>% 
  dplyr::rename(
    Char = Character, 
    # NS = Stroke,
    # CON = `Consistency..token.`, 
    # PC = `Phonetic.Combinability`, 
    # SC = `Semantic.Combinability`, 
    # SAR = Semantic.Ambiguity.Rating, 
    # mean_ACC = `Naming.Acc`, 
    mean_RT = `Naming.RT`
  ) %>% 
  select(all_of(c(
    "Char", "mean_RT" # "LogCF", "NS", "CON", "PC", "SC", "SAR"
  )))

DF.merged <- list(
    DF.Chang.16, 
    DF.Chang.20 %>% select(-all_of(c("subject_id", "RT")))
  ) %>% 
  purrr::reduce(
    dplyr::left_join, by = "Char"
  ) %>% 
  na.omit() %>% 
  unique()

## save file-3
writexl::write_xlsx(
  DF.merged, 
  file.path(input.folder, paste0("Chang_2016_", c("", "z_")[z], "CSR.xlsx"))
) 

## save file-4
DF.merged %>% 
  select(all_of(c(
    "LogCF", "NS", "CON", "PC", "SC", "SAR", "IMG", "AoA", "mean_RT"
  ))) %>% 
  writexl::write_xlsx(
    file.path(csr.folder, paste0("all_subjs_", c("raw_", "zvars_")[z], "16 (mean).xlsx"))
  ) 
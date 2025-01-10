rm(list = ls())

library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

setwd("C:/Users/PinWei/my_Haskins_project/")

data.dir <- file.path("Data", "Chang_et_al")

## Load and combine files: -----------------------------------------------------

nam.data <- readRDS(file.path(data.dir, "AoA_character_naming.rds"))
lex.data <- readRDS(file.path(data.dir, "AoA_lexical_decision.rds"))
Chang.Lee.20 <- rbind(nam.data, lex.data)

Chang.Lee.20 <- Chang.Lee.20 %>% 
  dplyr::mutate(
    task = ifelse(task == "naming", "Naming", "LD"))

## Modify column "REG": --------------------------------------------------------

Chang.Lee.20$PhoR <- Chang.Lee.20$REG
Chang.Lee.20 <- Chang.Lee.20 %>% 
  dplyr::mutate(
    REG = ifelse(PhoR == 1, 1, 0), # assign 1 if regular (1); otherwise assign 0.
    UNP = ifelse(PhoR == 2, 1, 0)  # assign 1 if unpronounceable (2); otherwise assign 0.
  )
Chang.Lee.20 <- select(Chang.Lee.20, -all_of("PhoR")) 

## Reorder columns: ------------------------------------------------------------

id_labels <- c(
  "item", "character", "subject_id", "task")

init_phonome_features <- c(
  "Stop", "Aspirated", "Voiced", "Bilabial", "Labiodental", 
  "Alveolar", "Palatoalveolar", "Alveolopalatal")

vars <- c(
  "LogCF", # log transformed character frequency
  "NS",    # number of strokes
  "REG",   # regular (1) or irregular (0)
  "UNP",   # unpronounceable (1) or pronounceable (0)
  "CON",   # phonological consistency
  "PC",    # phonetic combinability, number of characters that can be created by a phonetic radical
  "SC",    # semantic combinability, number of characters that can be created by a semantic radical
  "SAR",   # semantic ambiguity rating, measuring the number of meanings of a character
  "IMG",   # imageability, measuring how easily a mental image could be aroused by a character
  "AoA")

Chang.Lee.20 <- Chang.Lee.20 %>% 
  select(all_of(c(
    id_labels, "rt", "z_rt", init_phonome_features, vars
  )))

## Save to file: 
writexl::write_xlsx(Chang.Lee.20, 
                    file.path(data.dir, "Chang_Lee_2020.xlsx"))

## Perform z-transformation to specific columns: -------------------------------

cols_to_z <- colnames(Chang.Lee.20)[! colnames(Chang.Lee.20) %in% c(
  id_labels, "rt", "z_rt", init_phonome_features, "REG", "UNP"
)]

Chang.Lee.20 <- Chang.Lee.20 %>% 
  group_by(subject_id) %>% 
  dplyr::mutate(across(
    .cols = all_of(cols_to_z),
    .fns = ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
  )) %>% 
  ungroup() 

## Save to file: 
writexl::write_xlsx(Chang.Lee.20, 
                    file.path(data.dir, "Chang_Lee_2020_z.xlsx"))

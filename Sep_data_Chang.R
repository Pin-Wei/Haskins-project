rm(list = ls())

library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

setwd("C:/Users/PinWei/my_Haskins_project/")

z <- 2

data.path <- file.path(
  "Data", "Chang_et_al", 
  c("Chang_Lee_2020.xlsx", "Chang_Lee_2020_z.xlsx")[z])

out.top <- file.path(
  "..", "my_CSR", "Data_Linguistic", "Chang_et_al")

DF <- readxl::read_excel(data.path)

cols_of_interest <- c(
  # "Stop", "Aspirated", "Voiced", "Bilabial", "Alveolar", "Palatoalveolar", "Alveolopalatal", 
  "LogCF", # log transformed character frequency
  "NS",    # number of strokes
  # "REG",   # regular (1) or irregular (0)
  # "UNP",   # unpronounceable (1) or pronounceable (0)
  "CON",   # phonological consistency
  "PC",    # phonetic combinability, number of characters that can be created by a phonetic radical
  "SC",    # semantic combinability, number of characters that can be created by a semantic radical
  "SAR",   # semantic ambiguity rating, measuring the number of meanings of a character
  "IMG",   # imageability, measuring how easily a mental image could be aroused by a character
  "AoA"
)

for ( task in c("LD", "Naming") ) {
  
  out.dir <- file.path(out.top, task)
  if ( ! file.exists(out.dir) ) { 
    dir.create(out.dir, recursive=TRUE) }
  
  sub.DF <- DF %>% 
    subset(task == task)
  
  for ( sid in sub.DF$subject_id ) {
    
    out.path <- file.path(
      out.dir, paste0(
        c("", "zscored_")[z], "sub_", sid, ".xlsx"))
    
    if ( ! file.exists(out.path) ) {
      
      subj.DF <- sub.DF %>% 
        subset(
          subject_id == sid) %>% 
        dplyr::select(all_of(
          c(cols_of_interest, c("rt", "z_rt")[z])))
      
      writexl::write_xlsx(
        subj.DF, path = out.path, format_headers = TRUE)
    }
  }
}
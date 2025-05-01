rm(list = ls())

library(dplyr)
library(tidyr)
library(writexl)

setwd("C:/Users/PinWei/my_Haskins_project/")

data.path <- file.path(
  "Data", "Tse_et_al", "Tse_Yap_Chan_nz.csv")
# data.path <- file.path(
#   "Data", "Tse_et_al", "Tse_Yap_Chan.RData")

out.top <- file.path(
  "C:/", "Users", "PinWei", 
  "my_CSR", "Data_Linguistic", "Tse_et_al")

DF <- read.csv(data.path)
# original_name <- load(data.path) 
# DF <- get(original_name) 
# rm(list = original_name)
# rm(original_name)

DF <- DF %>% 
  dplyr::select(
    -all_of(paste0("char1_", 1:13))) %>% 
  dplyr::select(
    -all_of(c("Word_Trad", "Wordnum")))

cols_of_interest <- c(
  "C1_St.num", "C2_St.num",
  "C1_logF", "C2_logF",
  "Word_logF",
  # "C1_Ph.num", "C2_Ph.num",
  # "C1_Homo.D", "C2_Homo.D",
  "C1_Ph.cons", "C2_Ph.cons",
  "C1_OP.cons", "C2_OP.cons",
  "C1_A_Nb.num", "C2_B_Nb.num",
  # "C1_A_Nb.logF", "C2_B_Nb.logF",
  # "C1_S.num", "C2_S.num",
  "C1_S.Trans", "C2_S.Trans",
  "C1_S.T.Nb", "C2_S.T.Nb",
  "Imag"
)

for ( task in c("LD", "Naming") ) {
  
  out.dir <- file.path(out.top, task)
  if ( ! file.exists(out.dir) ) { 
    dir.create(out.dir, recursive=TRUE) }
  
  sub.DF <- DF %>% 
    subset(Task == task)
  
  for ( sid in sub.DF$ID ) {
    
    out.path <- file.path(
      out.dir, paste0("sub_", sid, ".xlsx"))
    
    if ( ! file.exists(out.path) ) {
      
      subj.DF <- sub.DF %>% 
        subset(ID == sid) %>% 
        dplyr::select(all_of(
          c(cols_of_interest, "Zrt2"))) %>% 
        na.omit()
      
      writexl::write_xlsx(
        subj.DF, path = out.path, format_headers = TRUE)
    }
  }
}
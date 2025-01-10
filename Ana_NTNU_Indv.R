rm(list = ls())

# source("Ana_NTNU_Indv.R")

library(dplyr)
library(tidyr)
library(lme4)
library(openxlsx)

## Variables -------------------------------------------------------------------

server <- c("local", "remote")[1]

fn <- c(
  "Data_all.RData", 
  "Data_all.z.RData", 
  "Data_clean.RData", 
  "Data_clean.z.RData"
)[3]

## Paths (auto setup) ----------------------------------------------------------

if ( server == "local" ) {
  setwd("C:/Users/PinWei/my_Haskins_project")
  data.path <- file.path("Data", "NTNU", fn)
  out.folder <- file.path("Stats", "NTNU")
  
} else { # "remote"
  setwd("/media/data2/pinwei/Haskins_project")
  data.path <- file.path("Data_single_characters", "NTNU", fn)
  out.folder <- file.path("Stats_NTNU")
}

if ( ! file.exists(out.folder)) { 
  dir.create(out.folder, recursive=TRUE) }

## Main ------------------------------------------------------------------------

## Load data:
original_name <- load(data.path) 
DF <- get(original_name) 
rm(list = original_name)
rm(original_name)

DF <- DF %>% 
  dplyr::rename(
    OPC = `OP.Con.type`,  
    OSC = `OS.Con.type`
    ) %>% 
  dplyr::select(all_of(
    c("SID", "Sex", "Grade", "Score", 
      "Char", "Correct", 
      "logF", "OPC", "OSC", "Imag")
    ))

## Fixed effects to iterate over:
fixed.effects <- c("logF", "OPC", "OSC", "Imag")

mdl.slopes <- DF %>% 
  split( ## split data by subject
    ., DF$SID) %>%
  lapply( ## for each subset of DF 
    ., function(sub.DF) {
      sapply( ## for each fixed effect
        fixed.effects, function(eff) {
          mdl <- glm(as.formula(paste0("Correct ~ ", eff)), 
                     data = sub.DF, 
                     family = binomial)
          return(coef(mdl)[[eff]])
        })
    }) %>% ## a list (subjects) of lists (slopes)
  dplyr::bind_rows() %>% 
  as.data.frame()

subj.list <- DF %>% split(DF$SID) %>% names()
mdl.slopes$SID <- subj.list

subj.desc <- DF %>% dplyr::distinct(
  SID, Sex, Grade, Score, .keep_all = TRUE) %>%
  select(SID, Sex, Grade, Score)

new.DF <- merge(subj.desc, mdl.slopes, by = "SID")

## Save to files:
save(new.DF, file = file.path(out.folder, "Indv_slopes.RData"))
write.csv(new.DF, 
          file = file.path(out.folder, "Indv_slopes.csv"), 
          row.names = F)


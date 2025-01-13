rm(list = ls())

# source("Ana_Chang_allvars_Indv.R")

library(readxl)
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest) 
# library(stargazer)
library(jtools)
library(car)
library(stats)
# library(openxlsx)
library(MuMIn)

## Variables -------------------------------------------------------------------

task.type <- c("Naming", "LD")[1]

mdl.type <- c("GLM", "GLMM")[2]

dv <- "z_rt"

var_list <- c("LogCF", "NS", "CON", "PC", "SAR", "IMG", "SC", "AoA")

server <- c("local", "remote")[
  as.integer(readline("Local [1] or remote [2]: "))
]

## Setup directories -----------------------------------------------------------

if ( server == "local" ) {
  setwd("C:/Users/PinWei/my_Haskins_project")
  data.dir <- file.path("Data", "Chang_et_al")
  data.path <- file.path(data.dir, "Chang_Lee_2020_z.xlsx")
  stats.outdir <- file.path("Stats", "Chang_et_al")
  
} else { # "remote"
  setwd("/media/data2/pinwei/Haskins_project")
  data.path <- file.path("Data_single_characters", "Chang_Lee_2020_z.xlsx")
  stats.outdir <- file.path("Stats_Chang")
}

if ( ! file.exists(stats.outdir)) { 
  dir.create(stats.outdir, recursive=TRUE) }

## Load data and run regression model ------------------------------------------

ana.data <- readxl::read_excel(data.path) %>% 
  subset(task == task.type)

formula <- as.formula(paste(
  dv, "~", paste(var_list, collapse = " + ")))

print(unique(ana.data$subject_id))

results <- ana.data %>% 
  split(
    ., ana.data$subject_id) %>% 
  lapply(., function(x) {
    mdl <- lm(formula, data = x)
    coefs <- mdl$coefficients
    rsq <- MuMIn::r.squaredGLMM(mdl)
    indv.df <- cbind(
      t(as.data.frame(coefs)), 
      data.frame(
        R2m = rsq[1],
        R2c = rsq[2],
        AIC = stats::AIC(mdl), 
        BIC = stats::BIC(mdl)
      )
    )
    return(indv.df)
  }) %>% 
  dplyr::bind_rows() %>% 
  as.data.frame()

subj.list <- ana.data %>% 
  split(ana.data$subject_id) %>% 
  names()

results$SID <- subj.list

write.csv(
  results[, c(
    "SID", "(Intercept)", var_list, 
    "R2m", "R2c", "AIC", "BIC"
  )], 
  file = file.path(stats.outdir, paste0(
    "[", task.type, "] all 8 variables using Indv data (GLM).csv")), 
  row.names = F
)
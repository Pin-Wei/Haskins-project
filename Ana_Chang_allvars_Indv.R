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

server <- c("local", "remote")[
  as.integer(readline("Local [1] or remote [2]: "))
]

task.type <- c("Naming", "LD")[1]

mdl.type <- c("GLM", "CSR")[
  as.integer(readline("GLM [1] or CSR [2]: "))
]

dv <- "z_rt"

var_list <- c("LogCF", "NS", "CON", "PC", "SAR", "IMG", "SC", "AoA")

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

if ( mdl.type == "GLM" ) {
  formula <- as.formula(paste(
    dv, "~", paste(var_list, collapse = " + ")))
  
} else if ( mdl.type == "CSR" ) {
  formula <- as.formula(paste(
    dv, "~", 
    paste(var_list, 
          collapse = " + "), # linear terms
    "+", 
    paste0("I(", var_list, "^2)", 
           collapse = " + "), # quadratic terms
    "+", 
    paste(combn(var_list, 2, function(x) paste(x, collapse = " * ")), 
          collapse = " + ") # interaction terms
  ))
}

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
        # nP = length(coef(mdl)), 
        # nT = nobs(mdl), 
        LogLik = logLik(mdl), 
        R2m = rsq[1],
        R2c = rsq[2],
        AIC = stats::AIC(mdl), 
        AICc = MuMIn::AICc(mdl), 
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
    "SID", # "nP", "nT", 
    "(Intercept)", var_list, 
    "LogLik", "R2m", "R2c", "AIC", "AICc", "BIC"
  )], 
  file = file.path(stats.outdir, paste0(
    "[", task.type, 
    "] all 8 variables using Indv data (", 
    mdl.type, ").csv")), 
  row.names = F
)
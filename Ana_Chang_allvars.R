rm(list = ls())

# source("Ana_Chang_allvars.R")

library(readxl)
library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest) 
library(stargazer)
library(jtools)
library(car)
library(stats)
library(openxlsx)
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

stats.out.fn <- paste0(
  "[", task.type, "] all 8 variables (", mdl.type, ").txt")

## Load data and run regression model ------------------------------------------

ana.data <- readxl::read_excel(data.path) %>% 
  subset(task == task.type)

if ( mdl.type == "GLM" ) {
  
  formula <- as.formula(paste(
    dv, "~", paste(var_list, collapse = " + ")))
    
  mdl <- lm(formula, data = ana.data)
  
} else if ( mdl.type == "GLMM") {
  
  formula <- as.formula(paste(
    dv, "~", paste(var_list, collapse = " + "), 
    "+ (1 | item)", 
    "+ (1 +", paste(var_list, collapse = " + "), " | subject_id)"))
  
  mdl <- lme4::lmer(formula = formula, 
                    data = ana.data, 
                    REML = FALSE, 
                    na.action = na.exclude, 
                    verbose = 1)
}

## Save to files ---------------------------------------------------------------

saveRDS(mdl, file = file.path(stats.outdir, 
                              paste0(mdl.type, ".rds")))

writeLines(
  c(
    capture.output(summary(mdl)), 
    capture.output(MuMIn::r.squaredGLMM(mdl)), 
    "", 
    paste("AIC:", stats::AIC(mdl)), 
    paste("BIC:", stats::BIC(mdl))
  ), 
  con = file.path(stats.outdir, stats.out.fn)
)

# writeLines(capture.output(jtools::summ(mdl, vifs = TRUE)), 
#            con = file.path(stats.outdir, 
#                            gsub(",txt", " (2),txt", stats.out.fn)))

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Model Summary")
openxlsx::writeData(wb, sheet = "Model Summary", 
                    x = stargazer(mdl, 
                                  align = TRUE, 
                                  type = "text"))
openxlsx::addWorksheet(wb, "VIF")
openxlsx::writeData(wb, sheet = "VIF", 
                    x = capture.output(car::vif(mdl)))
openxlsx::saveWorkbook(wb, 
                       file.path(stats.outdir, gsub(".txt", ".xlsx", stats.out.fn)), 
                       overwrite = TRUE)


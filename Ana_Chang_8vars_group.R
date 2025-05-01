rm(list = ls())
setwd("C:/Users/PinWei/my_Haskins_project")

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

var.list <- c(
  "LogCF", "NS", "CON", "PC", "SC", "SAR", "IMG", "AoA"
)

## Setup directories -----------------------------------------------------------

input.folder <- file.path(
  "Data", "Chang_et_al"
)
out.folder <- file.path(
  "Stats", "Chang_et_al", "Compare with CSR"
)
if ( ! file.exists(out.folder)) { 
  dir.create(out.folder, recursive=TRUE) 
}

## Load data and run regression model ------------------------------------------

for ( x in 1:2 ) {
  
  data <- readxl::read_excel(
    file.path(input.folder, c(
      "Chang_2020_z_CSR.xlsx", "Chang_2016_z_CSR.xlsx"
    )[x]))
  
  dv <- c("RT", "mean_RT")[x]
  
  formula.add <- c(
    paste0("+ (1 + ", paste(var.list, collapse = " + "), " | subject_id)"), ""
  )[x]
  
  formula <- as.formula(paste(
    dv, "~", paste(var.list, collapse = " + "), 
    "+ (1 | Char)", formula.add
  ))
  
  mdl <- lme4::lmer(formula = formula, 
                    data = data, 
                    REML = FALSE, 
                    na.action = na.exclude, 
                    verbose = 1)
  
  out.fn.txt <- paste0(
    "[GLMM] Naming.", dv, " ~ 8 vars (group-level).txt")
  
  writeLines(
    c(
      capture.output(summary(mdl)), 
      capture.output(MuMIn::r.squaredGLMM(mdl)), 
      "", 
      paste("AIC:", stats::AIC(mdl)), 
      paste("BIC:", stats::BIC(mdl))
    ), 
    con = file.path(out.folder, out.fn.txt)
  )
  
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
                         file.path(out.folder, gsub(".txt", ".xlsx", out.fn.txt)), 
                         overwrite = TRUE)
}
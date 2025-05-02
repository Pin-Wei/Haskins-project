rm(list = ls())

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

mdl.type <- c(
  "CSR", "GLM"
)[as.integer(readline(
  "CSR [1] or GLM/GLMM [2]: "
))]

server <- c("local", "remote")[as.integer(readline(
  "Local [1] or remote [2]: "
))]

## Setup directories -----------------------------------------------------------

if ( server == "local" ) {
  setwd("C:/Users/PinWei/my_Haskins_project")
  input.folder <- file.path("Data", "Chang_et_al")
  out.folder <- file.path("Stats", "Chang_et_al", "Compare with CSR")
} else { # "remote"
  setwd("/media/data2/pinwei/Haskins_project")
  input.folder <- file.path("Data_single_characters")
  out.folder <- file.path("Stats_Chang", "Compare with CSR")
}
  
if ( ! file.exists(out.folder)) { 
  dir.create(out.folder, recursive=TRUE) 
}

## Load data and run regression model ------------------------------------------

for ( x in 1:2 ) {
  
  input.data <- readxl::read_excel(file.path(
    input.folder, c(
      "Chang_2020_z_CSR.xlsx", "Chang_2016_z_CSR.xlsx"
    )[x]
  ))

  input.note <- c("trial-wise", "mean")[x]
  
  dv <- c("RT", "mean_RT")[x]
  
  if ( mdl.type == "CSR" ) {
    
    formula <- as.formula(paste(
      dv, "~", paste(var.list, collapse = " + "), # linear terms
      "+", paste0("I(", var.list, "^2)", collapse = " + "), # quadratic terms
      "+", paste(combn(var.list, 2, function(x) paste(x, collapse = " * ")), collapse = " + ") # interaction terms
    ))
    
    mdl <- lm(formula, data = input.data)
    
  } else if (( mdl.type == "GLM" ) & ( x == 1 )) {
    
    mdl.type <- "GLMM"
    
    formula <- as.formula(paste(
      dv, "~", paste(var.list, collapse = " + "), 
      "+ (1 | Char) + (1 +", paste(var.list, collapse = " + "), "| subject_id)"
    ))
    
    mdl <- lme4::lmer(formula = formula, 
                      data = input.data, 
                      REML = FALSE, 
                      na.action = na.exclude, 
                      verbose = 1)
    
  } else { # ( mdl.type == "GLM" ) & ( x == 2 )
    
    formula <- as.formula(paste(
      dv, "~", paste(var.list, collapse = " + ")
    ))
    
    mdl <- lm(formula, data = input.data)
  }
  
  out.fn.txt <- paste0(
    "[", mdl.type, "] Naming.", dv, " ~ 8 vars (group-level ", input.note, ").txt"
  )
  
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
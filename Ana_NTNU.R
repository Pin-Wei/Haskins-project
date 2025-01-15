rm(list = ls())

# source("Ana_NTNU.R")

library(dplyr)
library(tidyr)
library(lme4)
library(jtools)
library(stargazer)
library(car)
library(openxlsx)

## Variables -------------------------------------------------------------------

server <- c("local", "remote")[
  as.integer(readline("Local [1] or remote [2]: "))
]

fn <- c(
  "Data_all.RData", 
  "Data_all.z.RData", 
  "Data_clean.RData", 
  "Data_clean.z.RData"
)[3]

id.cols <- c(
  "SID", "Sex", "Grade", "Score", "Char", "Correct")

var.s <- c(
  "Imag", "OSC", paste0("OSC.", c("w2v", "GloVe", "DSG"))
)[2]

fixed.eff <- c("logF", var.s, "OPC")

mdl.type <- c("GLM", "GLMM")[1]

if ( mdl.type == "GLM" ) {
  formula <- as.formula(paste0(
    "Correct ~ ", paste(fixed.eff, collapse = " * ")))
  
} else if ( mdl.type == "GLMM") {
  random.eff <- fixed.eff
  
  formula <- as.formula(paste0(
    "Correct ~ ", paste(fixed.eff, collapse = " * "), 
    " + (1 | Char)", 
    # " + (1 | SID)",
    " + (1 + ", paste(random.eff, collapse = " + "), " || SID)"
  ))
}

## Paths (auto setup) ----------------------------------------------------------

if ( server == "local" ) {
  setwd("C:/Users/PinWei/my_Haskins_project")
  data.path <- file.path("Data", "NTNU", fn)
  out.stats <- file.path("Stats", "NTNU")
  out.figs  <- file.path("Figs", "NTNU")
    
} else { # "remote"
  setwd("/media/data2/pinwei/Haskins_project")
  data.path <- file.path("Data_single_characters", "NTNU", fn)
  out.stats <- file.path("Stats_NTNU")
  out.figs  <- file.path("Figs_NTNU")
}

for ( out.txt in c(out.stats, out.figs) ) {
  if ( ! file.exists(out.txt)) { 
    dir.create(out.txt, recursive=TRUE) }
}

## Load and modify data --------------------------------------------------------

original_name <- load(data.path) 
if ( original_name != "DF" ) {
  DF <- get(original_name) 
  rm(list = original_name)
  # rm(original_name)
}

DF <- DF %>% 
  dplyr::rename(
    OPC = `OP.Con.type`,  
    OSC = `OS.Con.type`
  ) %>% 
  dplyr::select(all_of(
    c(id.cols, fixed.eff)
  ))

DF$Char <- as.factor(DF$Char)
print(str(DF))

## Loop for subjects in different grade ----------------------------------------

# affix <- " (Overall)"

for ( gd in seq(1, 6) ) {
  
  affix <- paste0(" (G-", gd, ") ")
  
  # out.txt <- file.path(out.stats, paste0(
  #   prefix, paste(fixed.eff, collapse = " × "), affix, ".txt"))
  # 
  # k <- 1
  # while ( file.exists(out.txt) ) {
  #   out.txt <- gsub(".txt", paste0(" v.", k, ".txt"), out.txt)
  #   k <- k + 1
  # }
  
  ## Step-1. Select data and perform z-scoring ---------------------------------
  
  sub.DF <- DF %>% 
    subset(Grade == gd) %>%
    group_by(SID) %>% 
    dplyr::mutate(across(
      .cols = all_of(fixed.eff),
      .fns = ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
    )) %>% 
    ungroup() 
  
  ## Step-2. Replace outliers (z-score > 3) with NA ----------------------------
  
  for ( col in fixed.eff ) {
    sub.DF <- sub.DF %>% 
      mutate(!!col := ifelse(
        abs(.data[[col]]) > 3, NA, .data[[col]]))
  }

  ## Step-3. Modeling ----------------------------------------------------------
  
  print("Start modeling...")
  print(formula)
  # print(str(sub.DF))
  
  fp <- file.path(out.stats, paste0(
    "[", mdl.type, "] ", 
    paste(fixed.eff, collapse = " × "), affix, 
    ".rds"))
  
  if ( file.exists(fp) ) {
    
    mdl <- readRDS(fp)
    
  } else {
    
    if ( mdl.type == "GLM" ) {
      
      mdl <- glm(formula = formula, 
                 data    = sub.DF, 
                 family  = binomial("logit"), 
                 na.action = na.exclude)
      
    } else if ( mdl.type == "GLMM") {
      
      mdl <- lme4::glmer(formula = formula, 
                         data    = sub.DF, 
                         family  = binomial("logit"), # "binomial", 
                         na.action = na.exclude, 
                         verbose = 1)
    }
    
    ## Step-4. Save to files ----------------------------------------------------- 
    
    ## 1. 
    saveRDS(mdl, file = file.path(out.stats, paste0(
      "[", mdl.type, "] ", 
      paste(fixed.eff, collapse = " × "), affix, 
      ".rds")))
  }
  
  ## 2. 
  out.txt <- capture.output(
    jtools::summ(mdl, vifs = TRUE))
  
  writeLines(out.txt, con = file.path(out.stats, paste0(
    "[", mdl.type, "] ", 
    paste(fixed.eff, collapse = " × "), affix, 
    ".txt")))
  
  out.txt.2 <- capture.output(
    print(summary(mdl), correlation = TRUE))
  
  writeLines(out.txt.2, con = file.path(out.stats, paste0(
    "[", mdl.type, "] ", 
    paste(fixed.eff, collapse = " × "), affix, 
    " (1).txt")))
  
  ## 3. 
  wb <- openxlsx::createWorkbook()
  s <- "Model Summary"
  openxlsx::addWorksheet(wb, s)
  openxlsx::writeData(wb, sheet = s, 
                      x = stargazer(mdl, 
                                    align = TRUE, 
                                    type = "text"))
  s <- "VIF"
  openxlsx::addWorksheet(wb, s)
  openxlsx::writeData(wb, sheet = s, 
                      x = capture.output(car::vif(mdl)))
  openxlsx::saveWorkbook(wb, file.path(out.stats, paste0(
    "[", mdl.type, "] ", 
    paste(fixed.eff, collapse = " × "), affix, 
    ".xlsx")), overwrite = TRUE)
}

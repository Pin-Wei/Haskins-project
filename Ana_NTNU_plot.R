rm(list = ls())

# source("Ana_NTNU.R")

library(dplyr)
library(tidyr)
library(lme4)
library(ggplot2)
library(sjPlot)

## Variables -------------------------------------------------------------------

server <- c("local", "remote")[1]

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
)[1]

fixed.eff <- c("logF", var.s, "OPC")

mdl.type <- c("GLM", "GLMM")[1]

formula <- as.formula(paste0(
  "Correct ~ ", paste(fixed.eff, collapse = " * ")))

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
  
  # print("Start modeling...")
  # print(formula)
      
  mdl <- glm(formula = formula, 
             data    = sub.DF, 
             family  = binomial("logit"), 
             na.action = na.exclude)
    
  ## Step-4. Plot and save -----------------------------------------------------
  
  sjPlot::plot_model(mdl, 
                     vline.color = "red", 
                     sort.est = TRUE, 
                     show.values = TRUE, 
                     value.offset = .3) 
  
  ggsave(file.path(out.figs, paste0(
    "[Main] ", paste(fixed.eff, collapse = " × "), affix, 
    ".jpg")), width = 8, height = 6, dpi = 200)
  
  sjPlot::plot_model(mdl, 
                     type = c("eff", "pred", "int")[1], 
                     mdrt.values = c("quart", "zeromax", "meansd")[3], 
                     terms = fixed.eff, 
                     axis.title = "Correctness") 
  
  ggsave(file.path(out.figs, paste0(
    "[Interact] ", paste(fixed.eff, collapse = " × "), affix, 
    ".jpg")), width = 8, height = 6, dpi = 200)
}

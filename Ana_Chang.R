rm(list = ls())

library(readxl)
library(dplyr)
library(tidyr)
library(rstatix)
library(lme4)
library(lmerTest) # to get p-value estimations that are not part of the standard lme4 packages
library(ggplot2)
library(sjPlot)

## Setup directories -----------------------------------------------------------
setwd("C:/Users/PinWei/my_Haskins_project/")

data.dir <- file.path("Data", "Chang_et_al")
data.path <- file.path(data.dir, "Chang_Lee_2020_z.xlsx")

stats.outdir <- file.path("Stats", "Chang_et_al")
if ( ! file.exists(stats.outdir)) { 
  dir.create(stats.outdir, recursive=TRUE) }

figs.outdir  <- file.path("Figs", "Chang_et_al")
if ( ! file.exists(figs.outdir)) { 
  dir.create(figs.outdir, recursive=TRUE) }

## Load data -------------------------------------------------------------------

task.type <- c("Naming", "LD")[1]

ana.data <- readxl::read_excel(data.path) %>% 
  subset(task == task.type)

## Define variables ------------------------------------------------------------

dv <- "z_rt"

var.o <- c(
  "LogCF", "NS")[1]

var.p <- c(
  "REG", "UNP", "CON", "PC")[3]

var.s <- c(
  "SAR", "IMG", "SC")[3]

## Run regression model and plot -----------------------------------------------

var_list <- c(var.o, var.s, var.p)
  
formula <- as.formula(paste(
  dv, "~", paste(var_list, collapse = " * "), 
  "+ (1 | subject_id) + (1 | item)"))

# mdl <- lm(formula, data = ana.data)
mdl <- lmer(formula, data = ana.data, REML = FALSE)

stats.out.fn <- paste0(
  "[", task.type, "] ", paste(var_list, collapse = "_"), ".txt")

writeLines(capture.output(
  summary(mdl)), 
  con = file.path(stats.outdir, stats.out.fn))

# write.csv(broom::tidy(mdl),
#           file.path(stats.outdir, stats.out.fn))

sjPlot::plot_model(mdl, 
  type = c("eff", "pred", "int")[1], 
  mdrt.values = c("quart", "zeromax", "meansd")[3], 
  terms = var_list, 
  axis.title = "RT (Z transformed)") 
  # axis.labels = c()

# legend("right", legend = c("M-SD", "M", "M+SD"))
# 
# title(xlab = "Log Character Frequency (Scaled)")

ggsave(file.path(figs.outdir, gsub(".txt", ".jpg", stats.out.fn)),
       width = 8, height = 6, dpi = 200)

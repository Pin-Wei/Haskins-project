rm(list = ls())

# library(xlsx)
library(openxlsx)
library(dplyr)
library(tidyr)
library(stringr)
library(rstatix)
library(lme4)
library(lmerTest) 
library(ggplot2)
library(sjPlot)

## Setup directories -----------------------------------------------------------

setwd("C:/Users/PinWei/my_Haskins_project/")

data.dir <- file.path(
  "Data", "Tse_et_al")

stats.out.top <- file.path(
  "Stats", "Tse_et_al")

for ( fd in c("lm", "ANOVA")) {
  stats.outdir <- file.path(stats.out.top, fd)
  
  if ( ! file.exists(stats.outdir)) { 
    dir.create(stats.outdir, recursive=TRUE) }
}

figs.out.top  <- file.path(
  "Figs", "Tse_et_al")

for ( fd in c("lm", "ANOVA")) {
  figs.outdir <- file.path(figs.out.top, fd)
  
  if ( ! file.exists(figs.outdir)) { 
    dir.create(figs.outdir, recursive=TRUE) }
}


## Run additional regression model ---------------------------------------

load(file.path(data.dir, "Tse_Yap_Chan.RData"))

task <- c("LD", "Naming")[2]

ana.data <- subset(Tse.Yap.Chan, Task == task)

var.list <- c(
  "Word_logF.Z", 
  "C1_OP.cons.Z", 
  "C2_OP.cons.Z", 
  "C1_A_Nb.num.Z", 
  "C2_B_Nb.num.Z", 
  "Imag.Z"
)

formula <- as.formula(paste0(
  "Zrt2 ~ ", 
  paste(var.list, collapse = " * "), 
  " + (1 | ID) + (1 | Wordnum)"))

mdl <- lmer(formula, data = ana.data)

stat.fn <- paste0("LME modeling for zRT in ", task, ".txt")

writeLines(capture.output(
  summary(mdl)), 
  con = file.path(stats.out.top, stat.fn))

## [old] Load data and define variables ----------------------------------------------

load(file.path(data.dir, "CLP_data.RData"))
# str(CLP_data)

count.out.file <- file.path(
  stats.out.top, "ANOVA", "N of each group.xlsx")

count.list <- list()

var.o <- c(
  "Freq_C1", "Freq_C2", "Freq_word"
)[3]

# var.p <- c(
#   "P.cons_1", "P.cons_2", "OP.cons_1", "OP.cons_2"
# )[3]

var.s <- c(
  "Fami", "Conc", "Imag"
)[3]

for ( task in c("lexical decision", "naming")) {

  k <- which(
    colnames(CLP_data) == "IPF_1")

  ana.data <- CLP_data %>%
    subset(., switch(task,
                     `lexical decision` = lex.Acc,
                     naming = nam.Acc) >= 0.7) %>%
    na.omit(.) %>%
    dplyr::mutate(    # modify columns
      dplyr::across(  # apply functions across multiple columns
        .cols = where(is.numeric) & colnames(.)[k:ncol(.)],
        .fns  = ~ scale(.x)))

  rm(k)

  for ( meas in c("zRT", "Acc")) {

    dv <- paste(substr(task, 1, 3), meas, sep = ".")

  for ( var.p in c(
    "P.cons_1",
    "P.cons_2",
    "OP.cons_1",
    "OP.cons_2"
  )) {

  ## Categorize into binary columns ----------------------------------------

  ana.data <- ana.data %>%
    dplyr::mutate(across(
      .cols = all_of(c(var.o, var.s)),
      .fns = ~ factor(
        ifelse(.x >= median(.x, na.rm = TRUE),
               "High", "Low"),
        levels = c("Low", "High")),
      .names = "{.col}_category"))

  ana.data <- ana.data %>%
    dplyr::mutate(across(
      .cols = all_of(c(var.p)),
      .fns = ~ factor(
        ifelse(.x >= mean(.x, na.rm = TRUE), # since binary
               "High", "Low"),
        levels = c("Low", "High")),
      .names = "{.col}_category"))

  var.list <- c(paste0(var.o, "_category"),
                paste0(var.p, "_category"),
                paste0(var.s, "_category"))

  var.names <- stringr::str_replace_all(
    var.list, "_category", "")

  ## Count the number of data points for each categories -------------------
  # xtabs(as.formula(paste0("~ ", paste(var.list, collapse = " + "))), 
  #       data = ana.data)
  
  group.counts <- ana.data %>%
    group_by(
      interaction(
        ana.data[[var.list[1]]],
        ana.data[[var.list[2]]],
        ana.data[[var.list[3]]])) %>%
    summarise(
      count = n()) %>%
    tidyr::separate_wider_delim(
      ., cols = 1, delim = ".", names = var.names)

  sheet.name <- paste(var.names, collapse = " x ")

  count.list[[sheet.name]] <- data.frame(group.counts)

  ## ANOVA test and interaction plot ---------------------------------------

  formula <- as.formula(paste(
    dv, "~", paste(var.list, collapse = " * ")))

  AOV.3w <- aov(formula, data = ana.data)

  stat.fn <- paste0(paste(c(dv, gsub("_", ".", var.list)), collapse = "__"), ".csv")
  write.csv(broom::tidy(AOV.3w),
            file = file.path(stats.out.top, "ANOVA", stat.fn))

  vname_list <- c()

  for ( va in var.list ) {
    vname <- switch(
      gsub("_category", "", va),
      Freq_C1   = "C1 frequency (log transformed)",
      Freq_C2   = "C2 frequency (log transformed)",
      Freq_word = "Word frequency (log transformed)",
      P.cons_1  = "C1 Phonological\nConsistency",
      P.cons_2  = "C2 Phonological\nConsistency",
      OP.cons_1 = "C1 O-P Consistency",
      OP.cons_2 = "C2 O-P Consistency",
      Fami      = "Familiarity",
      Conc      = "Concreteness",
      Imag      = "Imagibility"
    )
    vname_list <- c(vname_list, vname)
  }

  png(file.path(figs.out.top, "ANOVA", gsub(".csv", ".png", stat.fn)),
      width = 600, height = 400)

  interaction.plot(
    response = ana.data[[dv]],
    x.factor = ana.data[[var.list[1]]],
    trace.factor = interaction(ana.data[[var.list[2]]],
                               ana.data[[var.list[3]]]),
    fun = mean,
    type = "b",
    lty = c(1, 1, 2, 2), # line types
    col = c("blue", "red", "blue", "red"),
    ylab = switch(dv,
                  lex.zRT = "RT (z-transformed)",
                  lex.Acc = "Accuracy rate",
                  nam.zRT = "RT (z-transformed)",
                  nam.Acc = "Accuracy rate"),
    xlab = vname_list[1],
    trace.label = paste(vname_list[2], "\n x", vname_list[3])
  )

  dev.off()  # close the graphic device

  ## Data visualization ----------------------------------------------------

  ggplot(data = ana.data,
         aes(x     = var.o,
             y     = dv,
             group = s.group,
             color = s.group)) +

    geom_point(size = 1.2, alpha = .7) +

    geom_smooth(method = lm, formula = y ~ x,
                se = FALSE, size = .5, alpha = .8) +

    scale_colour_brewer("s.group",
                        name = switch(var.s,
                                      Fami = "Familiarity",
                                      Conc = "Concreteness",
                                      Imag = "Imagibility"),
                        palette = "Spectral",
                        direction = -1) +

    theme_minimal() +

    labs(x = switch(var.o,
                    Freq_C1    = "C1 frequency (log transformed)",
                    Freq_C2    = "C2 frequency (log transformed)",
                    Freq_word  = "Word frequency (log transformed)",
                    P.cons_1   = "C1 Phonological\nConsistency",
                    P.cons_2   = "C2 Phonological\nConsistency",
                    OP.cons_1  = "C1 O-P Consistency",
                    OP.cons_2  = "C2 O-P Consistency"),
         y = switch(dv,
                    lex.zRT = "RT (z-transformed)",
                    lex.Acc = "Accuracy rate",
                    nam.zRT = "RT (z-transformed)",
                    nam.Acc = "Accuracy rate"),
         title = paste("Effect of", var.o, "on", task, "performance"),
         subtitle = paste("for different levels of", var.s))

  ## Run regression model and plot -----------------------------------------

  for ( var.list in list(
    c(var.o, var.s, var.p)
    # c(var.o, var.p),
    # c(var.o, var.s),
    # c(var.p, var.s)
  )) {
    vname_list <- c()

    for ( va in var.list ) {
      vname <- switch(
        gsub("_category", "", va),
        Freq_C1   = "C1 frequency (log transformed)",
        Freq_C2   = "C2 frequency (log transformed)",
        Freq_word = "Word frequency (log transformed)",
        P.cons_1  = "C1 Phonological\nConsistency",
        P.cons_2  = "C2 Phonological\nConsistency",
        OP.cons_1 = "C1 O-P Consistency",
        OP.cons_2 = "C2 O-P Consistency",
        Fami      = "Familiarity",
        Conc      = "Concreteness",
        Imag      = "Imagibility"
      )
      vname_list <- c(vname_list, vname)
    }

    var.list <- stringr::str_replace_all(
      var.list, var.p, paste0(var.p, "_category"))

    formula <- as.formula(paste(
      dv, "~", paste(var.list, collapse = " * ")))

    mdl <- lm(formula, data = ana.data)

    P <- sjPlot::plot_model(
      mdl,
      type = "eff",
      terms = var.list,
      axis.title = switch(dv,
                          lex.zRT = "RT (z-transformed)",
                          lex.Acc = "Accuracy rate",
                          nam.zRT = "RT (z-transformed)",
                          nam.Acc = "Accuracy rate"),
      legend.title = vname_list[2])

    if ( length(var.list) == 3 ) {
      fig.fn <- paste0(dv, "__",
                       gsub("_", ".", var.list[1]), "__",
                       gsub("_", ".", var.list[2]), "__",
                       gsub("_", ".", var.list[3]), ".jpg")

      ggsave(file.path(figs.out.top, "lm", fig.fn),
             width = 8, height = 6, dpi = 200)

      stat.fn <- gsub(".jpg", ".csv", fig.fn)
      write.csv(broom::tidy(mdl),
                file.path(stats.out.top, "lm", stat.fn))

    } else if ( length(var.list) == 2 ) {
      pseudo <- c(var.o, var.p, var.s)
      remain <- pseudo[! pseudo %in% var.list]
      fig.fn <- paste0(dv, "__",
                       gsub("_", ".", var.list[1]), "__",
                       gsub("_", ".", var.list[2]), " (",
                       gsub("_", ".", remain), ").jpg")

      ggsave(file.path(figs.out.top, "lm", fig.fn),
             width = 8, height = 6, dpi = 200)

    } else {
      print("WARRING: undefined var.list length, outputs are not saved.")
    }
  }

  }
  }
}

openxlsx::write.xlsx(count.list, file = count.out.file)
#!/usr/bin/env Rscript

rm(list = ls())

library(readxl)
library(dplyr)
library(tidyr) 
library(reshape2)
library(purrr)
library(writexl)

## Variables -------------------------------------------------------------------

server <- c("local", "remote")[
  as.integer(readline("Local [1] or remote [2]: "))
]

cols.from.Hsieh <- c(
  "Char", "OS.Con.type", "OS.Con.token")

cols.from.Chang <- c(
  "Char", "Freq", "OP.Con.type", "OP.Con.token", 
  "REG", "NS", "HD", "Fami", "SAR")

cols.from.Liu <- c(
  "Char", "CF", "PF", "Fami.2", "Imag", "Conc", "NM")

cols.out <- c(
  "SID", "Sex", "Age_month", "Grade", # "Score", 
  "Char", "Correct", 
  "NS", "logF", "Freq", # "CF", "PF", 
  "HD", # "HD.normed", 
  "P.Reg", "P.Unp", 
  "OP.Con.type", "OP.Con.token", 
  "OS.Con.type", "OS.Con.token", # "OS.Var.type", 
  # paste0("OSC.", c("w2v", "GloVe", "DSG")), 
  # "OSC.w2v.normed", "OSC.DSG.normed", 
  "SAR", "Imag", # "Imag.normed", 
  "Conc", "Fami.2", "Fami", "NM"
)

## Setup paths -----------------------------------------------------------------

if ( server == "local" ) {
  setwd("C:/Users/PinWei/my_Haskins_project")
  NTNU.naming <- file.path(
    "Data", "NTNU", "RawTable_cleaned.xlsx")
  Hsieh.et.al <- file.path(
    "Data", "Hsieh_family_semantic_consistency.csv")
  my.w2v <- file.path(
    "Data", "char_consistency_metrics_w2v.xlsx")
  my.GloVe <- file.path(
    "Data", "char_consistency_metrics_GloVe.xlsx")
  my.DSG <- file.path(
    "Data", "char_consistency_metrics_DSG.xlsx")
  Chang.et.al <- file.path(
    "Data", "Chang_et_al", "Chang_Lee_2016.csv")
  Liu.et.al <- file.path(
    "Data", "Liu_et_al_2007.xlsx")
  Output.char.DF <- file.path(
    "Data", "NTNU", "char_DF.xlsx")
  Output.all.noz <- file.path(
    "Data", "NTNU", "Data_all.RData")
  Output.clean.noz <- file.path(
    "Data", "NTNU", "Data_clean.RData")

} else { # "remote"
  setwd("/media/data2/pinwei/Haskins_project")
  NTNU.naming <- file.path(
    "Data_single_characters", "NTNU", "RawTable_cleaned.xlsx")
  Hsieh.et.al <- file.path(
    "Data_single_characters", "Hsieh_family_semantic_consistency.csv")
  my.w2v <- file.path(
    "SC_matrices", "char_consistency_metrics_w2v.xlsx")
  my.GloVe <- file.path(
    "SC_matrices", "char_consistency_metrics_GloVe.xlsx")
  my.DSG <- file.path(
    "SC_matrices", "char_consistency_metrics_DSG.xlsx")
  Chang.et.al <- file.path(
    "Data_single_characters", "Chang_Lee_2016.csv")
  Liu.et.al <- file.path(
    "Data_single_characters", "Liu_et_al_2007.xlsx")
  Output.char.DF <- file.path(
    "Data_single_characters", "NTNU", "char_DF.xlsx")
  Output.all.noz <- file.path(
    "Data_single_characters", "NTNU", "Data_all.RData")
  Output.clean.noz <- file.path(
    "Data_single_characters", "NTNU", "Data_clean.RData")
}

## Load data -------------------------------------------------------------------

DF.NTNU.wide <- readxl::read_excel(NTNU.naming) %>% 
  dplyr::mutate(
    Sex = ifelse(Sex == 1, "F", "M")) %>% 
  subset(
    Screen == 1) %>% 
  subset(
    select = -c(Screen, `著...828`, `著...1974`)) 

DF.NTNU <- DF.NTNU.wide %>% 
  reshape2::melt(
    id = c("SID", "Grade", "Sex", "Age_month", "Score"), 
    variable.name = "Char", 
    value.name = "Correct") %>% 
  dplyr::mutate(
    dplyr::across( 
      .cols = all_of(c("Grade", "Sex", "Correct")),
      .fns  = as.factor))

DF.NTNU$Char <- as.character(DF.NTNU$Char)

DF.Hsieh <- read.csv(Hsieh.et.al) %>% # 5675 char
  dplyr::rename(
    Char         = character,
    OS.Con.type  = cons_type_PG,
    OS.Con.token = cons_token_PG
  ) %>%
  subset(select = cols.from.Hsieh)

# DF.w2v <- readxl::read_excel(my.w2v) %>% 
#   dplyr::rename(
#     OSC.w2v = Se.Sim.Mean
#   ) %>% 
#   subset(select = c("Char", "OSC.w2v"))
# 
# DF.GloVe <- readxl::read_excel(my.GloVe) %>% 
#   dplyr::rename(
#     OSC.GloVe = Se.Sim.Mean
#   ) %>% 
#   subset(select = c("Char", "OSC.GloVe"))
# 
# DF.DSG <- readxl::read_excel(my.DSG) %>% 
#   dplyr::rename(
#     OSC.DSG = Se.Sim.Mean
#   ) %>% 
#   subset(select = c("Char", "OSC.DSG"))

DF.Chang <- read.csv(Chang.et.al) %>% # 3314 char
  dplyr::rename(
    Char         = Character, 
    Freq         = Frequency, 
    OP.Con.type  = `Consistency..type.`, 
    OP.Con.token = `Consistency..token.`, 
    REG          = Regularity, 
    NS           = Stroke,
    HD           = Homophone.Density, 
    Fami         = Familiarity, 
    SAR          = Semantic.Ambiguity.Rating
  ) %>% 
  subset(select = cols.from.Chang)

DF.Liu <- readxl::read_excel(Liu.et.al) %>% # 2355 char
  dplyr::rename(
    Char   = Word, 
    Fami.2 = FAM, 
    Imag   = IMG, 
    Conc   = CON
  ) %>% 
  subset(select = cols.from.Liu)

## Merge dataframes ------------------------------------------------------------

DF.out <- list(
    DF.NTNU, DF.Hsieh, # DF.w2v, DF.GloVe, DF.DSG
    DF.Chang, DF.Liu
  ) %>% 
  purrr::reduce(
    dplyr::left_join, by = "Char"
  ) 

rm(list = c("DF.NTNU", "DF.Hsieh", "DF.Chang", "DF.Liu"))

## Perform further processing --------------------------------------------------

DF.out <- DF.out %>% 
  dplyr::mutate(
    P.Reg = as.factor(ifelse(REG == 1, 1, 0)), # assign 1 if regular (1); otherwise assign 0.
    P.Unp = as.factor(ifelse(REG == 2, 1, 0))  # assign 1 if unpronounceable (2); otherwise assign 0.
  ) %>% 
  group_by(SID) %>% 
  dplyr::mutate(
    logF = log(Freq)
  ) %>% 
  ungroup() %>% 
  dplyr::select(
    all_of(cols.out))

## Only keep chars that have all the specified features ------------------------

DF.out.clean <- DF.out %>% 
  tidyr::drop_na() # remain 693 char

## Compute number of overlapped characters:
# DF.out.clean <- DF.out %>%
#     tidyr::drop_na(all_of(c(
#       "OS.Con.type", # cols.from.Hsieh
#       "OP.Con.type", # cols.from.Chang
#       "Imag" # cols.from.Liu
#       ))) %>%
#     pull(Char) %>%
#     unique() %>%
#     length()

## Save these information-complete characters ----------------------------------

char_list <- unique(DF.out.clean$Char)

write.table(char_list, 
            file.path(top.dir, "char_list.txt"), 
            sep = "\n", quote = FALSE, 
            row.names = FALSE, col.names = FALSE)

char.DF <- DF.out.clean %>% 
  dplyr::select(-all_of(
    c("SID", "Sex", "Age_month", "Grade", "Correct")
  )) %>% 
  dplyr::distinct(
    Char, .keep_all = TRUE)

writexl::write_xlsx(char.DF, Output.char.DF)

## Use other characters to calculate naming score ------------------------------

DF.NTNU.score <- DF.NTNU.wide %>% 
  dplyr::select(
    -all_of(char_list)) %>% 
  dplyr::mutate(
    Score = rowSums(across(
      -c("SID", "Sex", "Age_month", "Grade", "Score")
    ))) %>% 
  dplyr::select(
    all_of(c("SID", "Score")))

## Save the original versions --------------------------------------------------

DF.out <- dplyr::left_join(
  x = DF.out, y = DF.NTNU.score, by = "SID")

save(DF.out, file = Output.all.noz)

DF.out.clean <- dplyr::left_join(
  x = DF.out.clean, y = DF.NTNU.score, by = "SID")

save(DF.out.clean, file = Output.clean.noz)

## Save the z-scored versions --------------------------------------------------

cols_to_z <- cols.out[! cols.out %in% c(
  "SID", "Sex", "Age_month", "Grade", "Score", 
  "Char", "Correct", "P.Reg", "P.Unp"
)]

DF.out.z <- DF.out %>% 
  group_by(SID) %>% 
  dplyr::mutate(across(
    .cols = all_of(cols_to_z),
    .fns = ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
  )) %>% 
  ungroup() 

save(DF.out.z, file = gsub(".RData", ".z.RData", Output.all.noz))

DF.out.clean.z <- DF.out.clean %>% 
  group_by(SID) %>% 
  dplyr::mutate(across(
    .cols = all_of(cols_to_z),
    .fns = ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
  )) %>% 
  ungroup() 

save(DF.out.clean.z, file = gsub(".RData", ".z.RData", Output.clean.noz))

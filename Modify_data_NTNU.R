rm(list = ls())

library(readxl)
library(dplyr)
library(tidyr) 
library(reshape2)
library(writexl)

## Top -------------------------------------------------------------------------
setwd("C:/Users/PinWei/my_Haskins_project/")

top.dir           <- file.path("Data", "NTNU")
data.path         <- file.path(top.dir, "RawTable_cleaned.xlsx")
output.all.path   <- file.path(top.dir, "Data_all.RData")
output.clean.path <- file.path(top.dir, "Data_clean.RData")

DF.wide <- readxl::read_excel(data.path) %>% 
  dplyr::mutate(
    Sex = ifelse(Sex == 1, "F", "M")) %>% 
  subset(
    Screen == 1) %>% 
  subset(
    select = -c(Screen, `著...828`, `著...1974`)) 

DF <- DF.wide %>% 
  reshape2::melt(
    id = c("SID", "Grade", "Sex", "Age_month", "Score"), 
    variable.name = "Char", 
    value.name = "Correct") %>% 
  dplyr::mutate(
    dplyr::across( 
      .cols = all_of(c("Grade", "Sex", "Correct")),
      .fns  = as.factor))

DF$Char <- as.character(DF$Char)

# ## Add (my) family semantic consistency metrics --------------------------------
# 
# for ( sc.ver in c("w2v", "GloVe", "DSG") ) {
#   
#   DF_temp <- readxl::read_excel(
#     file.path("Data", paste0("char_consistency_metrics_", 
#                              sc.ver, ".xlsx")))
#   
#   sc_renamed <- paste0("OSC.", sc.ver)
#   colnames(DF_temp)[colnames(DF_temp) == "Se.Sim.Mean"] <- sc_renamed
#   
#   sel_SC_cols <- c("Char", sc_renamed)
#   DF_temp <- subset(DF_temp, select = sel_SC_cols)
#   
#   DF <- merge(
#     x = DF, y = DF_temp, by = "Char", all.x = TRUE)
#   
#   rm(DF_temp)
# }
# 
## Add family semantic consistency metrics -- Hsieh et al. (2024) --------------

DF_temp <- read.csv(
  file.path("Data", "Hsieh_family_semantic_consistency.csv"))
  ### 5675 char

sel_Hsieh_cols <- c(
  "Char", "OS.Con.type", "OS.Con.token")

DF_temp <- DF_temp %>%
  dplyr::rename(
    Char         = character,
    OS.Con.type  = cons_type_PG,
    OS.Con.token = cons_token_PG
  ) %>%
  subset(select = sel_Hsieh_cols)

DF <- merge(
  x = DF, y = DF_temp, by = "Char", all.x = TRUE)
  ### 3099 char overlap

rm(DF_temp)

## Add features of single characters -- Chang et al. (2016) --------------------

DF_temp <- read.csv(
  file.path("Data", "Chang_et_al", "Chang_Lee_2016.csv"))
  ### 3314 char

sel_Chang_cols <- c(
  "Char", "Freq", "OP.Con.type", "OP.Con.token", 
  "REG", "NS", "HD", "Fami", "SAR")

DF_temp <- DF_temp %>% 
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
  subset(select = sel_Chang_cols)

DF <- merge(
  x = DF, y = DF_temp, by = "Char", all.x = TRUE)
  ### 1738 char overlap

rm(DF_temp)

# DF_temp.2 <- readxl::read_excel(
#   file.path("Data", "Chang_et_al", "Chang_Lee_2020.xlsx"))
# 
# DF_temp.2 <- DF_temp.2 %>% 
#   dplyr::distinct(character, .keep_all = TRUE)
#
# ### only 1353 characters!

## Add features of single characters -- Liu et al. (2007) ----------------------

DF_temp <- readxl::read_excel(
  file.path("Data", "Liu_et_al_2007.xlsx"))
  ### 2355 char

sel_Liu_cols <- c(
  "Char", "CF", "PF", "Fami.2", "Imag", "Conc", "NM")

DF_temp <- DF_temp %>% 
  dplyr::rename(
    Char   = Word, 
    Fami.2 = FAM, 
    Imag   = IMG, 
    Conc   = CON
  ) %>% 
  subset(select = sel_Liu_cols)

DF <- merge(
  x = DF, y = DF_temp, by = "Char", all.x = TRUE)
  ### 1281 char overlap

rm(DF_temp)

## Further processing ----------------------------------------------------------

DF <- DF %>% 
  dplyr::mutate(
    P.Reg = as.factor(ifelse(REG == 1, 1, 0)), # assign 1 if regular (1); otherwise assign 0.
    P.Unp = as.factor(ifelse(REG == 2, 1, 0))  # assign 1 if unpronounceable (2); otherwise assign 0.
  )

col_names <- c(
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

DF <- DF %>% 
  group_by(SID) %>% 
  dplyr::mutate(
    logF = log(Freq)) %>% 
  dplyr::select(all_of(col_names)) %>% 
  ungroup() 

DF.clean <- DF %>% 
  tidyr::drop_na() # remain 693 char
 
# DF %>% 
#   tidyr::drop_na(all_of(c(
#     "OS.Con.type", # sel_Hsieh_cols
#     "OP.Con.type", # sel_Chang_cols
#     "Imag" # sel_Liu_cols
#     ))) %>% 
#   pull(Char) %>% 
#   unique() %>% 
#   length()

## use other trials to caculate naming score:

char_list <- unique(DF.clean$Char)

DF.score <- DF.wide %>% 
  dplyr::select(
    -all_of(char_list)) %>% 
  dplyr::mutate(
    Score = rowSums(across(
      -c("SID", "Sex", "Age_month", "Grade", "Score")
      ))) %>% 
  dplyr::select(
    all_of(c("SID", "Score")))

DF <- merge(
  x = DF, y = DF.score, by = "SID", all.x = TRUE)

DF.else <- merge(
  x = DF, y = DF.score, by = "SID", all.x = TRUE)

## z-scoring:

cols_to_z <- col_names[! col_names %in% c(
  "SID", "Sex", "Age_month", "Grade", "Score", 
  "Char", "Correct", "P.Reg", "P.Unp")]

DF.z <- DF %>% 
  group_by(SID) %>% 
  dplyr::mutate(across(
    .cols = all_of(cols_to_z),
    .fns = ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
  )) %>% 
  ungroup() 

DF.clean.z <- DF.clean %>% 
  group_by(SID) %>% 
  dplyr::mutate(across(
    .cols = all_of(cols_to_z),
    .fns = ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
  )) %>% 
  ungroup() 

DF.else <- DF %>% 
  subset(! Char %in% char_list)

DF.else.z <- DF.else %>% 
  group_by(SID) %>% 
  dplyr::mutate(across(
    .cols = all_of(cols_to_z),
    .fns = ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
  )) %>% 
  ungroup() 

## Save to files ---------------------------------------------------------------

save(DF, 
     file = output.all.path)

save(DF.clean, 
     file = output.clean.path)

save(DF.z, 
     file = gsub(".RData", ".z.RData", output.all.path))

save(DF.clean.z, 
     file = gsub(".RData", ".z.RData", output.clean.path))

write.table(char_list, 
            file.path(top.dir, "char_list.txt"), 
            sep = "\n", quote = FALSE, 
            row.names = FALSE, col.names = FALSE)

save(DF.else, 
     file = gsub("_all.RData", "_else.RData", output.all.path))

save(DF.else.z, 
     file = gsub("_all.RData", "_else.z.RData", output.all.path))
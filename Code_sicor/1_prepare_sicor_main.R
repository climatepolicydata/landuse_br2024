#### ---------------------------------------------------------------------- ####
####    CPI/PUC-Rio - Shared Data                                           ####
####    Database: SICOR (BCB) - Rural Credit                                ####
####    Script: Cleaning and joining main database files                    ####
####    Created: May 12th, 2023, by: Wagner F. Oliveira                     ####
####    Last updated: Aug 2025, by: Julia Niemeyer                          ####
#### ---------------------------------------------------------------------- ####


############################### ACTION NEEDED ################################

#### ---------------------------------------------------------------------- ####
####    Environment                                                         #### 
#### ---------------------------------------------------------------------- ####

# ## set anos de analise caso não esteja rodando pelo MASTER
ano_ini = 2013 #the initial year to star analysis
ano_fim = 2024 #the final year to end your analysis
#ano_base = 2024 #the year to base inflation
# #
# # # ## set the path to your github clone
github <- "Documents/"



#####################  
# Clean Memory
#rm(list=ls())
gc()
################

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, data.table, lubridate, stringr, geobr,
               sf, janitor, rio, scales, RColorBrewer, stringi, ggpubr,
               tidygeocoder, glue, bit64)

# Set user
who <- normalizePath(str_remove_all(Sys.getenv("HOME"), "Documents"), "/")

# Working Directories
dir <- "A:/finance/sicor/" # main directory
raw <- paste0(dir, "rawData/") # where raw data are stored
clean <- paste0(dir, "cleanData/") # where cleaned data will be stored
auxil <- paste0(raw, "auxiliary/") # where auxiliary data are stored

# Remove Scientific Notation
options(scipen = 999)

#### ---------------------------------------------------------------------- ####
####    Loading and basic cleaning each year main file, joining together    #### 
#### ---------------------------------------------------------------------- ####

# Read each year file
years <- ano_ini:(ano_fim+1)
list_bases <- map(years,
                  ~ fread(glue(raw, "base/SICOR_OPERACAO_BASICA_ESTADO_{.x}.csv"))
                  )

# Name and class check of variables over database years
var_check <- tibble(colname = names(list_bases[[length(list_bases)]]))
for (i in 1:length(list_bases)) {
  
  x <- list_bases[[i]] %>%
    imap_dfr(~ tibble(colname = .y, classes = class(.x) %>%
                        str_c(collapse = ", ")))
  
  var_check <- full_join(var_check, x, by = 'colname')
  
  d <- 2012+i
  colnames(var_check)[ncol(var_check)] <- d
  rm(x,d)
  
}

# Create database year variable
for (i in 1:length(list_bases)) {
  
  list_bases[[i]] <- list_bases[[i]] %>%
    mutate(ANO_BASE = 2012+i)
  
}

# Variable "CD_CONTRATO_STN" is character in 2021-2023
# Changing to integer64 for compatibility
for (i in 9:11) {
  list_bases[[i]] <- list_bases[[i]] %>%
    mutate(x = as.integer64(CD_CONTRATO_STN)) %>%
    mutate(x = na_if(x, is.na(CD_CONTRATO_STN) == T)) %>%
    select(-CD_CONTRATO_STN) %>%
    rename(CD_CONTRATO_STN = x)
}

# Joining all years together
df <- do.call(bind_rows, list_bases)

# Clean names
df <- df %>%
  clean_names() %>%
  rename(ref_bacen = number_ref_bacen) %>%
  relocate(ref_bacen, nu_ordem, ano_base)

# Save full database
saveRDS(df, paste0(clean, "sicor_main_", ano_ini, "_", ano_fim, ".Rds"))

# Clean memory
rm(list=ls())
gc()

#### ---------------------------------------------------------------------- ####
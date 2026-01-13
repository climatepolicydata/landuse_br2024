#### ---------------------------------------------------------------------- ####
####    CPI/PUC-Rio - Shared Data                                           ####
####    Database: SICOR (BCB) - Rural Credit                                ####
####    Script: Cleaning and joining main database files                    ####
####    Created: Apr 24th, 2023, by: Renan Morais                           ####
####    Last updated: Aug, 2025, by: Julia Niemeyer               ####
#### ---------------------------------------------------------------------- ####


############################### ACTION NEEDED ################################
# ## set anos de analise caso não esteja rodando pelo MASTER
ano_ini = 2013 #the initial year to star analysis
ano_fim = 2024 #the final year to end your analysis
#ano_base = 2024 #the year to base inflation
# #
# # # ## set the path to your github clone
github <- "Documents/"





#### ---------------------------------------------------------------------- ####
####    Environment                                                         #### 
#### ---------------------------------------------------------------------- ####

# Clean Memory

#rm(list=ls())
gc()

# Packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl, data.table, lubridate, geobr,
               sf, janitor, rio, scales, RColorBrewer, stringi, ggpubr,
               tidygeocoder, glue, bit64, writexl, openxlsx, httr, magrittr)

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
####    Join main sicor database with 'empreendimentos' table               #### 
#### ---------------------------------------------------------------------- ####

# Load data
sicor_op_basica <- readRDS(paste0(clean, "sicor_main_", ano_ini, "_", ano_fim, ".Rds"))

df_empreendimento <- read.csv(paste0(auxil, "Empreendimento.csv"),
                  sep = ",", encoding = "latin1") %>%
    clean_names()

# Prepare data before join
df_empreendimento <- df_empreendimento %>%
    mutate_all(tolower) %>%
    select(-data_fim) %>%
    rename(cd_empreendimento = x_codigo) %>%
    mutate(cd_empreendimento = as.integer64(cd_empreendimento))

# Join
df <- left_join(sicor_op_basica, df_empreendimento,
           by = "cd_empreendimento")

# Save cleaned data
saveRDS(df, paste0(clean, "sicor_main_", ano_ini, "_", ano_fim, "_empreendimento.Rds"))

# Clean memory
rm(list=ls())
gc()

#### ---------------------------------------------------------------------- ####
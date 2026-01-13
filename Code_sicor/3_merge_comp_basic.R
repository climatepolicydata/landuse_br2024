#### ---------------------------------------------------------------------- ####
####    CPI/PUC-Rio - Shared Data                                           ####
####    Database: SICOR (BCB) - Rural Credit                                ####
####    Script: Joining basic complement data with the main dataset         ####
####    Created: May 12th, 2023, by: Wagner F. Oliveira                     ####
####    Last updated: Aug  24th, 2025, by: Julia Niemeyer                   #### ---------------------------------------------------------------------- ####

#### ---------------------------------------------------------------------- ####
####    Environment                                                         #### 
#### ---------------------------------------------------------------------- ####

# Clean Memory
rm(list=ls())
gc()

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

# Sources
#https://www.bcb.gov.br/estabilidadefinanceira/creditorural?modalAberto=tabelas_sicor

#### ---------------------------------------------------------------------- ####
####    Load SICOR data files                                               #### 
#### ---------------------------------------------------------------------- ####

### Load full database
df <- readRDS(paste0(clean, "sicor_main_2013_2025_V2.Rds"))

### Load Basic complementary file (contains Municipality)
basic <- fread(paste0(raw, "complement/SICOR_COMPLEMENTO_OPERACAO_BASICA.csv")) %>%
  clean_names() %>%
  rename(ref_bacen = number_ref_bacen)

# This file contains information on the municipality of the operation only for 
# the universe of subsidized credit operations.

#### ---------------------------------------------------------------------- ####
####    Identifying municipalities with the basic complementary file        #### 
#### ---------------------------------------------------------------------- ####

# Check number of contracts in the main file
df %>%
  distinct(ref_bacen, nu_ordem) %>%
  nrow() # No duplicates 

# Check number of contracts in the complementary file
basic %>%
  distinct(ref_bacen, nu_ordem) %>%
  nrow() # No duplicates 

### Merge full database and basic complementary file
basic <- basic %>% mutate(is_basic = 1)
df <- df %>%
  left_join(basic, by = c("ref_bacen", "nu_ordem")) %>%
  mutate(is_basic = replace_na(is_basic, 0))

# Check for observations in the basic complementary file that are not in the
# operations database
teste <- left_join(basic, df, by = c("ref_bacen", "nu_ordem")) %>%
  filter(is.na(ano_base) == T)
nrow(teste)/nrow(basic) # 0 observations

### Save merged data
saveRDS(df, paste0(clean, "sicor_main_2013_2025_basic_complement_V2.Rds"))

### Clean memory
rm(list=ls())
gc()

#### ---------------------------------------------------------------------- ####
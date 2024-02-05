#### ---------------------------------------------------------------------- ####
####    CPI/PUC-Rio                                                         ####
####    Project: Taxonomy technical note                                    ####
####    Script: Evaluating the impact of taxonomies on rural credit         ####
####    Created: Nov 28th, 2023, by: Wagner F. Oliveira                     ####
####    Last updated: Jan 11th, 2024, by: Carolina Moniz De Moura           ####
#### ---------------------------------------------------------------------- ####

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
               tidygeocoder, glue, bit64,writexl, openxlsx, httr, magrittr, readr, data.table, dplyr, plyr)

# Set user
who <- str_remove(Sys.getenv("HOME"), "Documents")

# Working Directories
dir <- "A:/finance/sicor/" # main directory
raw <- paste0(dir, "rawData/") # where raw data are stored
clean <- paste0(dir, "cleanData/") # where cleaned data will be stored
auxil <- paste0(raw, "auxiliary/") # where auxiliary data are stored

dir_taxonomies <- "A:/projects/taxonomy/"
dir_taxonomies_auxil <- paste0(dir_taxonomies, "auxiliary/")
dir_tmp <- paste0(dir_taxonomies, "tmp/")

# Remove Scientific Notation
options(scipen = 999)

# Sources
#https://www.bcb.gov.br/estabilidadefinanceira/creditorural?modalAberto=tabelas_sicor

#### ---------------------------------------------------------------------- ####
####    Load SICOR data files                                               #### 
#### ---------------------------------------------------------------------- ####
setwd(clean)

### Load full database
df_sicor <- readRDS(paste0(clean, "sicor_main_2013_2023_with_empreendimento.Rds"))

df_sicor <- df_sicor %>% select(-cesta,
                                -unidade_medida_previsao,
                                -cedula_mae,
                                -zoneamento,
                                -vl_area_financ,
                                -vl_juros,
                                -vl_aliq_proagro,
                                -vl_prev_prod,
                                -vl_perc_risco_fundo_const,
                                -dt_fim_plantio,
                                -vl_juros_enc_finan_posfix,
                                -dt_vencimento,
                                -vl_quantidade,
                                -vl_rec_proprio,
                                -vl_rec_proprio_srv,
                                -vl_produtiv_obtida,
                                -dt_inic_colheita,
                                -vl_perc_custo_efet_total,
                                -cd_fase_ciclo_producao,
                                -vl_prestacao_investimento,
                                -vl_receita_bruta_esperada,
                                -vl_perc_risco_stn,
                                -dt_fim_colheita,
                                -cd_contrato_stn)

#### ---------------------------------------------------------------------- ####
####    Cleaning and Creating variables                                     #### 
#### ---------------------------------------------------------------------- ####

### Create Year variable
df_sicor <- df_sicor %>% mutate(ano = as.numeric(format(mdy(df_sicor$dt_emissao),'%Y')))


df <- df %>%
  group_by() %>%
  dplyr::mutate(id_equals = dplyr::cur_group_id()) %>%
  ungroup()

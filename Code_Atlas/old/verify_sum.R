##################

# Author : Renan Morais
# Date: 29-05-2023
# Email: renanflorias@hotmail.com
# Goal: transformação base atlas rural para landscape
# resource: 


########################### Libraries ######################################

pacman::p_load(tidyverse, 
               stringi, 
               janitor, 
               writexl,
               openxlsx, 
               httr,
               magrittr, 
               readr,
               data.table,
               dplyr,
               plyr,
               pivottabler)

################## directories ###################

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
dir_sisser_mapa_dt_clean <- ("A:/finance/atlas_Seguro_Rural/rawData/verify_renan")
dir_sisser_mapa_output <- ("A:/finance/sisser/rawData")


#dir_mapa_raw <- paste0(root, "Dropbox (CPI)/Climate Finance Brazil/01_DATA/MAPA/0.Database/2.Raw")

setwd(dir_sisser_mapa_output)


df_atlas <- read.csv("psrdadosabertos2022a2023csv.csv", sep = ";", header = T, fileEncoding = "latin1") %>% janitor::clean_names()

df_atlas_16_21 <- read.csv("psrdadosabertos2016a2021csv.csv", sep = ";", header = T, fileEncoding = "latin1") %>% janitor::clean_names()

df_atlas_full <- rbind(df_atlas, df_atlas_16_21)

df_atlas_full <- df_atlas_full %>% 
  mutate(vl_subvencao_federal = ifelse(is.na(df_atlas_full$vl_subvencao_federal), 0, vl_subvencao_federal)) %>% 
  mutate(vl_subvencao_federal = gsub(",",".", vl_subvencao_federal)) %>% 
  mutate(vl_subvencao_federal = as.numeric(vl_subvencao_federal))

group <- df_atlas_full %>% aggregate(vl_subvencao_federal ~ ano_apolice, FUN = "sum")

sum(df_atlas_full$vl_subvencao_federal)

############# 

setwd(dir_sisser_mapa_dt_clean)

df_atlas_new <- read.csv("psrdadosabertos2022a2023csv.csv", sep = ";", header = T, fileEncoding = "latin1") %>% janitor::clean_names()

df_atlas_16_21_new <- read.csv("psrdadosabertos2016a2021csv.csv", sep = ";", header = T, fileEncoding = "latin1") %>% janitor::clean_names()


df_atlas_full_new <- rbind(df_atlas_new, df_atlas_16_21_new)

df_atlas_full_new <- df_atlas_full_new %>% 
  mutate(vl_subvencao_federal = ifelse(is.na(df_atlas_full_new$vl_subvencao_federal), 0, vl_subvencao_federal)) %>% 
  mutate(vl_subvencao_federal = gsub(",",".", vl_subvencao_federal)) %>% 
  mutate(vl_subvencao_federal = as.numeric(vl_subvencao_federal))

group_new <- df_atlas_full_new %>% aggregate(vl_subvencao_federal ~ ano_apolice, FUN = "sum")

sum(df_atlas_full_new$vl_subvencao_federal)

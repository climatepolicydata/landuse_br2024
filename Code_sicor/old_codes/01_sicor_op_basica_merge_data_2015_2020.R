##################

# Author : Renan Morais
# Date: 24-04-2023
# Email: renanflorias@hotmail.com
# Goal: base merge of "sicor operacao basica"
# resource: https://sparkbyexamples.com/r-programming/r-read-multiple-csv-files/


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
               data.table)

###### import dataset ########

###refatorado####
root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")


## Leitura da base de dados
df<-  list.files( #identificando os files existentes na pasta em formato csv
  path = paste0(root, "Dropbox (CPI)/Climate Finance Brazil/01_DATA/BCB/0_Database/2_Raw/sicor_op_basic/"),
  pattern = "*.csv",
  full.names = TRUE)  %>% 
  lapply( function(file){ # lendo os files
    fread(file, sep = ";")}) %>% 
    bind_rows()


# Columns to remove
columns_to_remove <- c(
  "DT_INIC_PLANTIO",
  "DT_INIC_COLHEITA", 
  "DT_FIM_PLANTIO", 
  "DT_FIM_COLHEITA", 
  "VL_JUROS_ENC_FINAN_POSFIX",
  "VL_PRODUTIV_OBTIDA",
  "CNPJ_AGENTE_INVEST",
  "CD_REF_BACEN_INVESTIMENTO",
  "VL_PERC_RISCO_STN"
)

df <- df %>% select(-one_of(columns_to_remove))

# Write cleaned dataset to a CSV file
write_csv(
  df,
  file = paste0(root,"Dropbox (CPI)/Climate Finance Brazil/01_DATA/BCB/0_Database/3_Dataset cleaned/sicor_2015_2020_25042023_v2.csv"
))

rm(df)


################# meu código #######
# 
# setwd("C:/Users/rflorias/Dropbox (CPI)/Climate Finance Brazil/01_DATA/BCB/0_Database/2_Raw/sicor_op_basic")
# 
# df <-
#   list.files(path = "C:/Users/rflorias/Dropbox (CPI)/Climate Finance Brazil/01_DATA/BCB/0_Database/2_Raw/sicor_op_basic/", pattern = "*.csv") %>% 
#   map_df(~read.csv(.,sep = ";"))
# df
#                              # ,
#                             # pattern = "*.csv", full.names = TRUE)
# 
# # list_csv_files <- list.files(path = "C:/Users/rflorias/Dropbox (CPI)/Climate Finance Brazil/01_DATA/BCB/0_Database/2_Raw/sicor_op_basic",
# #                              +                             pattern = "*.csv", full.names = TRUE)
# # df_empilhados <- readr::read_csv2(list_csv_files, id="file_name")
# 
# 
# remover <- c("DT_INIC_PLANTIO",
#              "DT_INIC_COLHEITA", 
#              "DT_FIM_PLANTIO", 
#              "DT_FIM_COLHEITA", 
#              "VL_JUROS_ENC_FINAN_POSFIX",
#              "VL_PRODUTIV_OBTIDA",
#              "CNPJ_AGENTE_INVEST",
#              "CD_REF_BACEN_INVESTIMENTO",
#              "VL_PERC_RISCO_STN",
#              "file_name")
# 
# df[,remover] <- NULL
# 
# setwd("C:/Users/rflorias/Dropbox (CPI)/Climate Finance Brazil/01_DATA/BCB/0_Database/3_Dataset cleaned")
# 
# write_csv(df, file = "C:/Users/rflorias/Dropbox (CPI)/Climate Finance Brazil/01_DATA/BCB/0_Database/3_Dataset cleaned/sicor_2015_2020_25042023_v2.csv")




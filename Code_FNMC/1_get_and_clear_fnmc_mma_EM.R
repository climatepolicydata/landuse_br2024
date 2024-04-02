##################

# Author : Renan Morais
# Date: 18-08-2023
# Email: renanflorias@hotmail.com
# Goal: clear operações indiretas automaticas BNDES
# resource:

########################### Libraries ######################################


library(tidyverse)
library(janitor)
library(stringi)
##### directory #########

dir_fnmc_raw <- ("A:/finance/fnmc_Mma/rawData")

dir_fnmc_clear <- ("A:/finance/fnmc_Mma/cleanData")


##### import datasets #########
setwd(dir_fnmc_raw)

df_fnmc <- read.csv2("projetos_fnmc___dados_abertos_ago_2022.csv", stringsAsFactors = FALSE) %>% as_tibble
df_fnmc_clear <- df_fnmc %>% 
  janitor::clean_names() %>% mutate(nome_do_projeto = str_trim(str_to_lower(stri_trans_general(nome_do_projeto,"Latin-ASCII"))),
  instituicao_convenente = str_trim(str_to_lower(stri_trans_general(instituicao_convenente,"Latin-ASCII"))),
  tipo_de_instituicao = str_trim(str_to_lower(stri_trans_general(tipo_de_instituicao,"Latin-ASCII"))),
  no_convenio_ou_ted = str_trim(no_convenio_ou_ted))
df_fnmc_clear %>% view
df_fnmc_clear <- df_fnmc_clear %>% mutate(no_convenio_ou_ted = str_to_lower(stri_trans_general(no_convenio_ou_ted,"Latin-ASCII")))
setwd(dir_fnmc_clear)

write.csv2(df_fnmc_clear, "fnmc_dados_abertos_Abril_2024_clear.csv")

##################

# Author : Renan Morais
# Date: 18-08-2023
# Email: renanflorias@hotmail.com
# Goal: clear operações indiretas automaticas BNDES
# resource:

########################### Libraries ######################################

pacman::p_load(tidyverse, stringi, janitor, writexl, openxlsx, httr, magrittr, readr, data.table, dplyr, plyr)

##### directory #########

dir_bndes_n_aut_raw <- ("A:/finance/bndes_N_aut/rawData")

dir_bndes_n_aut_clear <- ("A:/finance/bndes_N_aut/CleanData")


##### import datasets #########
setwd(dir_bndes_n_aut_raw)


df_n_aut_clear <- read.csv2("operacoes_financiamento_operacoes_nao_automaticas.csv") %>% 
  janitor::clean_names() %>%  
  mutate_if(is.character, tolower)

df_n_aut_clear <- df_n_aut_clear %>% mutate(ano = as.numeric(format(dmy(df_n_aut_clear$data_da_contratacao),'%Y'))) %>% 
  mutate_if(is.character, ~ stri_trans_general(., "Latin-ASCII")) %>% #retira acentos
  mutate(subsetor_cnae_nome =  str_trim(subsetor_cnae_nome)) %>%  #remove espaços em branco sem padrão
  mutate(natureza_do_cliente = str_trim(natureza_do_cliente)) %>% 
  mutate(instrumento_financeiro = str_trim(instrumento_financeiro)) %>% 
  mutate(descricao_do_projeto = str_trim(descricao_do_projeto))





setwd(dir_bndes_n_aut_clear)

saveRDS(df_n_aut_clear, "operacoes_financiamento_operacoes_nao_automaticas_clear.RDS")

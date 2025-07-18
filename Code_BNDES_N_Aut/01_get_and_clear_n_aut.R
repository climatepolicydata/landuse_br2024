##################

# Author : Renan Morais
# Date: 18-08-2023
# Email: renanflorias@hotmail.com
# Goal: clear operações indiretas automaticas BNDES
# resource:


#Modified by Julia Niemeyer
# 16/07/2025



########################### Libraries ######################################

pacman::p_load(tidyverse, stringi, janitor, writexl, openxlsx, httr, magrittr, readr, dplyr)

##### directory #########

dir_bndes_n_aut_raw <- ("A:\\finance\\bndes_naut\\rawData\\")

dir_bndes_n_aut_clear <- ("A:\\finance\\bndes_naut\\cleanData")


##### import datasets #########
setwd(dir_bndes_n_aut_raw)


df_n_aut_clear <- read.csv2("operacoes-financiamento-operacoes-nao-automaticas_2025.csv", fileEncoding = "latin1") %>% 
  janitor::clean_names() %>%  
  mutate_if(is.character, tolower)

df_n_aut_clear <- df_n_aut_clear %>% mutate( ano = as.numeric(year(ymd(data_da_contratacao)))) %>% 
  mutate_if(is.character, ~ stri_trans_general(., "Latin-ASCII")) %>% #retira acentos
  mutate(subsetor_cnae_nome =  str_trim(subsetor_cnae_nome)) %>%  #remove espaços em branco sem padrão
  mutate(natureza_do_cliente = str_trim(natureza_do_cliente)) %>% 
  mutate(instrumento_financeiro = str_trim(instrumento_financeiro)) %>% 
  mutate(descricao_do_projeto = str_trim(descricao_do_projeto))


df_n_aut_clear$ano%>%unique


setwd(dir_bndes_n_aut_clear)

saveRDS(df_n_aut_clear, paste0(dir_bndes_n_aut_clear, "\\operacoes_financiamento_operacoes_nao_automaticas_clear_2025.RDS"))

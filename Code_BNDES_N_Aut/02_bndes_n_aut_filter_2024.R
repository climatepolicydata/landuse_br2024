##################

# Author : Renan Morais
# Date: 07-11-2023
# Email: renanflorias@hotmail.com
# Goal: transform database (bnde_n_aut) for landscape
# resource: 


########################### Libraries ######################################
pacman::p_load(tidyverse, 
               stringi, 
               janitor, 
               writexl,
               openxlsx, 
               httr,
               readr,
               data.table,
               dplyr,
               plyr,
               pivottabler)




df <- readRDS("C:/Users/eduar/Dropbox (CPI)/EduardoMinsky/PARAMIM/BNDES_N_Aut/operacoes_financiamento_operacoes_nao_automaticas_clear.RDS")
sector_bndes_n_aut <- read.xlsx("06_bndes_naut_relational_tables.xlsx", sheet = "sector_landscape")


df_filter <- df %>% filter(ano >= 2021 & ano <= 2023)%>% 
                    filter(fonte_de_recurso_desembolsos != "recursos vinculados - fundo amazonia")

rm(df)
df_filter <- df_filter %>% select(-c(valor_desembolsado_reais,custo_financeiro,juros,
prazo_carencia_meses,prazo_amortizacao_meses,inovacao,area_operacional,setor_cnae,setor_bndes,tipo_de_garantia,tipo_de_excepcionalidade))

lista_instrument <- c("bndes florestal", 
                      "distribuicao de gas e biocombustiveis - incentivada b", 
                      "producaoo de alimentos e biocombustiveis - incentivada b")

lista_descricao <- c("cana-de-acucar|etanol|canavial|biomassa|cana de acucar")

df_filter_reviewed <- df_filter %>% filter(subsetor_cnae_nome %in% sector_bndes_n_aut$subsetor_cnae_nome |
                                               instrumento_financeiro %in% lista_instrument | 
                                               grepl(lista_descricao, descricao_do_projeto))



df_filter_reviewed%>%write_rds("df_bndes_n_aut_filter_reviewed.rds")

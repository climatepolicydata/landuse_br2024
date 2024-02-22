# Author : Renan Morais Florias
# Date: 05.09.2022
# Email: renan.florias@cpiglobal.org
# Goal: function automatic for index of deflate
# resource: https://sidra.ibge.gov.br/tabela/1737
#https://metadados.ibge.gov.br/consulta/estatisticos/operacoes-estatisticas/IA/2023/9/0
#Tabela 1737 - IPCA - Série histórica com número-índice, variação mensal e variações acumuladas em 3 meses, em 6 meses, no ano e em 12 meses (a partir de dezembro/1979) (Vide Notas)


#biblioteca padrão
pacman::p_load(tidyverse,
               readxl,
               readr,
               openxlsx,
               data.table,
               writexl,
               ggplot2,
               janitor,
               dplyr)

########################## directories ############################

dir <- "A:/finance/sicor/" # main directory
raw <- paste0(dir, "rawData/") # where raw data are stored
clean <- paste0(dir, "cleanData/") # where cleaned data will be stored
auxil <- paste0(raw, "auxiliary/") # where auxiliary data are stored

dir_taxonomies <- "A:/projects/taxonomy/"
dir_taxonomies_auxil <- paste0(dir_taxonomies, "auxiliary/")
dir_tmp <- paste0(dir_taxonomies, "tmp/")

setwd(clean)

### Load full database
df_sicor <- readRDS(paste0(clean, "sicor_main_2013_2023_with_empreendimento.Rds"))


root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

ibge_ipca <- read.xlsx("A:/macro/IPCA/cleanData/IPCA.xlsx", sheet = "export_month") %>% 
  select(ano, mes, deflator_last12) %>% 
  mutate(ano_mes = paste0(ano,mes))

deflator_automatico <- function(tabela_deflator, base) {
  tabela_deflator <- tabela_deflator %>% 
    mutate(deflator = 100/deflator_last12)
  
  base <- base %>% 
    # dplyr::rename(value_original_currency = vl_parc_credito)%>% 
    # mutate(ano_mes = paste0(ano,mes))%>%  "se não existir essas duas váriaveis precisamos tirar o comentário dessas linhas e cria.
    left_join(tabela_deflator, by = "ano_mes") %>% 
    mutate(value_brl_deflated = as.numeric(value_original_currency * deflator))
  
  return(base)
}


base_deflacionada <- deflator_automatico(ibge_ipca, df_sicor)

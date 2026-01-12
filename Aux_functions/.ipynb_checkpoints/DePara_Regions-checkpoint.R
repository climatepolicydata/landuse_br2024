### Função que faz DePara entre UF e Região

# Created by Julia Niemeyer
# Date 14/07/2025

## load libraries
library(readxl)
library(dplyr)

root <- paste0("C://Users//", Sys.getenv("USERNAME"), "//")


# Lendo o arquivo CSV do IBGE contendo Estados, Municipios, Regiões e Biomas
planilha = read_excel(paste0(root, '/CPI/SP-Program - Brazil Landscape/2025/3. Data Scoping/DePara_UF_States_Regiao_Mun.xlsx')) %>%
  mutate(across(everything(), tolower)) 


#Função que insere região pelo UF
inserir_regiao <- function(base_dados, planilha) {
  
  # padronizar siglas para evitar problemas com espaços ou caixa alta/baixa
  base_dados <- base_dados %>%
    mutate(uf = str_to_lower(str_trim(uf)))
  
  planilha <- planilha %>%
    distinct(SIGLA_UF, .keep_all = TRUE)  %>% # pega apenas a primeira linha de cada UF
    mutate(SIGLA_UF = str_to_lower(str_trim(SIGLA_UF)))
  
  # fazer join e inserir a região corretamente
  base_dados %>%
    left_join(planilha %>% select(SIGLA_UF, NM_REGIAO),
              by = c("uf" = "SIGLA_UF")) %>%
    mutate(region = NM_REGIAO) %>%
    select(-NM_REGIAO)  # remove coluna auxiliar
}


# Função que insere NOME DO municipio, regiao e UF pelo código do municipio
inserir_mun <- function(base_dados, planilha) {
  
  # padronizar siglas para evitar problemas com espaços ou caixa alta/baixa
  base_dados <- base_dados %>%
    mutate(municipality = as.numeric(str_trim(municipality)))
  
  planilha <- planilha %>%
    mutate(CD_MUN = as.numeric(str_trim(CD_MUN)))
  
  # fazer join e inserir a região corretamente
  base_dados %>%
    left_join(planilha %>% select(CD_MUN, NM_MUN, NM_REGIAO, SIGLA_UF),
              by = c("municipality" = "CD_MUN")) %>%
    mutate(municipality = NM_MUN,
           region = NM_REGIAO,
           uf = SIGLA_UF) %>%
    select(-NM_MUN)  # remove coluna auxiliar
}




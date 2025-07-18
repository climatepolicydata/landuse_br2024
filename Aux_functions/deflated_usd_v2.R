
#biblioteca padrão
pacman::p_load(tidyverse,
               readxl,
               readr,
               openxlsx,
               data.table,
               writexl,
               ggplot2,
               janitor,
               dplyr,
               XML,
               methods,
               xml2)

########################## directories ############################
dir_usd_fed <- ("A:\\macro\\usd_FED\\rawData\\")


#setwd(dir_usd_fed)

usd_inflation <- read_xls(paste0(dir_usd_fed, "USD Inflation_FED.xls"),skip = 10) %>% janitor::clean_names() %>% select(observation_date,fpcpitotlzgusa,inflacao_anual) %>% 
  dplyr::mutate(year = year(observation_date)) %>% select(year, fpcpitotlzgusa)

#criando a funcao

deflator_usd <- function(ano_ini, ano_fim, base) {
  anos = ano_ini:ano_fim
  deflators <- numeric(length(anos))
  serie_basedosdados <- usd_inflation
  
  serie_basedosdados <- serie_basedosdados  %>% dplyr::rename(ano = year, variacao_doze_meses = fpcpitotlzgusa)
  # selecao e filtros de valores de interesse ao calculo, queremos sempre a variacao anual, por isso o mes == 12
  serie_filtrada <- serie_basedosdados %>% 
    select(ano, variacao_doze_meses) %>% 
    dplyr::filter(ano >= ano_ini & ano <= ano_fim ) %>% 
    dplyr::arrange(desc(ano))
  
  indice = 1
  
  
  # Função recursiva para calcular os deflatores
  deflator_usd <- function(ano_atual) {
    if (ano_atual == ano_base) {
      return(1)
    } else if (ano_atual > ano_base) {
      variacao_anual <- prod(1 + serie_filtrada$variacao_doze_meses[serie_filtrada$ano == (ano_base - 1)] / 100)
      return(deflator_usd(ano_atual - 1) / variacao_anual)
    } else {
      variacao_anual <- prod(1 + serie_filtrada$variacao_doze_meses[serie_filtrada$ano == ano_atual] / 100)
      return(deflator_usd(ano_atual + 1) * variacao_anual)
    }
  }
  
  for (i in seq_along(anos)) {
    deflators[i] <- deflator_usd(anos[i])
  }
  
  tabela_finalUSD <- data.frame(year = anos, deflatorUSD = deflators) %>% arrange(year)
  
  cat("deflator automatico USD atualizado para", ano_fim, "\n")
  return(tabela_finalUSD)
}

#aplicando a funcao na base 
#essa funcao eh arbitraria, devido as diferentes variáveis que queremos multiplicar e diferentes bases

# calculo_deflator_usd <- function(tabela_deflator, base_select_deflator, tabela_cambio) {
#   
#   base_select_deflator <- base_select_deflator %>% 
#     left_join(tabela_deflator, by= "year")%>%
#     left_join(tabela_cambio, by= "year") %>%
#     dplyr::mutate(value_usd_deflated = ifelse(original_currency == "brl" * deflator))
#   
#   
#   return(base_select_deflator)
#   
# }
#DeflateV3
# caso original_currency == 'BRL', value_BRLm = value_original_currency,  value_brl_deflated = as.numeric(value_original_currency * deflator),  value_USDm= value_BRLm/bid,  
## Caso original_currency == 'USD', value_USDm = original_currency, 

#deflateUSD
# caso original_currency == 'BRL',value_USDm_deflated = value_USDm*deflator_USD
# Caso original_currency == 'USD', value_USDm_deflated = value_USDm*deflator_USD, value_BRLm = value_USDm*bid,  value_brl_deflated = value_USDm_deflated*bid

calculo_deflator_usd <- function(tabela_deflator, base_select_deflator, tabela_cambio) {
  
  base_select_deflator <- base_select_deflator %>% 
    left_join(tabela_deflator, by = "year") %>%
    #left_join(tabela_cambio, by = "year") %>%
    dplyr::mutate(
      # Deflator sobre valor em USD
      value_usd_deflated = value_USDm * deflatorUSD) %>%
    dplyr::rename("deflator_usd" = "deflatorUSD")
  
  cat("Cálculo do deflator USD concluído.\n")
  return(base_select_deflator)
}


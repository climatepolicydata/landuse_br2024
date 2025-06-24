
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


setwd(dir_usd_fed)

usd_inflation <- read_xls("USD Inflation_FED.xls",skip = 10) %>% janitor::clean_names() %>% select(observation_date,fpcpitotlzgusa,inflacao_anual) %>% 
  dplyr::mutate(year = year(observation_date)) %>% select(year, fpcpitotlzgusa)

#criando a funcao

deflator_usd <- function(ano_ini, ano_fim, anos, base) {
  
  serie_basedosdados <- base
  
  serie_basedosdados <- serie_basedosdados  %>% dplyr::rename(ano = year, variacao_doze_meses = fpcpitotlzgusa)
  # selecao e filtros de valores de interesse ao calculo, queremos sempre a variacao anual, por isso o mes == 12
  serie_filtrada <- serie_basedosdados %>% 
    select(ano, variacao_doze_meses) %>% 
    dplyr::filter(ano >= ano_ini & ano <= ano_fim ) %>% 
    dplyr::arrange(desc(ano))
  
  indice = 1
  
  
  
  #criacao do data frame para o deflator
  for (l in anos) {
    # chamei novamente a base feita pela funcao do api, pois a base precisa ser percorrida ano a ano e
    # se nao criarmos essa tabela, a tabela a ser percorrida novamente terá sempre o ano inicial como observacao
    tabela <- serie_filtrada 
    
    tabela <- tabela %>% 
      dplyr::filter(ano == l)
    
    if (l == ano_fim) {
      tabela <- tabela %>%  dplyr::mutate(deflator = indice)
      tabela_final <- tabela
      indice_ano_anterior = indice * (1+ (tabela$variacao_doze_meses/100))
    } else {
      tabela <- tabela %>% dplyr::mutate(deflator = indice_ano_anterior)
      
      tabela_final <- rbind(tabela_final, tabela)
      indice_ano_anterior = indice_ano_anterior * (1 + (tabela$variacao_doze_meses/100))
    }
  }
  tabela_final <- tabela_final %>% 
    select(ano, deflator) %>%
    dplyr::rename(year = ano) %>% 
    dplyr::arrange(year)
  return(tabela_final)
}

#aplicando a funcao na base 
#essa funcao eh arbitraria, devido as diferentes variáveis que queremos multiplicar e diferentes bases

calculo_deflator_usd <- function(tabela_deflator, base_select_deflator) {
  
  base_select_deflator <- base_select_deflator %>% 
    left_join(tabela_deflator, by= "year")%>%
    dplyr::mutate(value_usd_deflated = ifelse(original_currency == "brl" * deflator))
  
  
  return(base_select_deflator)
  
}
# eh necessario a escolha dos anos nos quais quer criar os indices. Assim, a funcao toma como base/indice = 1 o ano final

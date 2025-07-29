# Author : Julia Niemeyer & João Valente
# Date: 20/05/2025
# Goal: automatic function for index of deflate
# resource:  IPCA do IPEA http://ipeadata.gov.br/Default.aspx


# Author : Renan Morais Florias
# Date: 05.09.2022
# Email: renan.florias@cpiglobal.org
# Goal: function automatic for index of deflate
# resource: https://sidra.ibge.gov.br/tabela/1737
#https://sidra.ibge.gov.br/pesquisa/snipc/ipca/tabelas/brasil/janeiro-2024
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
               dplyr,
               sidrar)

########################## directories ############################

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

#criando a funcao de deflacao
deflator_automatico <- function(ano_ini, ano_fim, tabela) {
  
  #a variavel anos completa os anos no intervalo de anos escolhidos acima.
  anos = seq(ano_fim,ano_ini) 
  deflators <- numeric(length(anos))
  
  #Criar vetor de strings no formato "ano12"
  datas <- paste0(ano_ini:ano_fim, "12")
  
  
  #pegando a tabela atualizada do sidra
  sidra <- get_sidra(
    x = 1737,                          # Tabela com dados de IPCA por grupos
    #variable = 63,                     # Variação acumulada no ano (%)
    period = datas,          # últimos 120 meses (ou ajuste como quiser)
    geo = "Brazil",                    # Nível Brasil
    #classific = "c315",                # Classificação: grupos de produtos
    category = list("7169"),           # 7169 = Índice geral
    header = T,
    format = 3                         # Data.frame limpo
  )
  
  ibge_ipca <- sidra %>%
    filter(Variável == "IPCA - Variação acumulada em 12 meses")
  
  
  serie_basedosdados <- ibge_ipca
  
  # selecao e filtros de valores de interesse ao calculo, queremos sempre a variacao anual, por isso o mes == 12
  # serie_filtrada <- serie_basedosdados %>% 
  #   select(ano, mes, variacao_doze_meses) %>% 
  #   filter(mes == 12,ano >= ano_ini & ano <= ano_fim ) %>% 
  #   arrange(desc(ano))
  serie_filtrada <- serie_basedosdados %>% 
    separate(Mês, into = c("mes", "ano"), sep = " ") %>%
    select(ano, Valor) %>%
    dplyr::rename("year" = "ano") %>%
    mutate(year = as.integer(year)) %>%
    filter(year >= ano_ini & year <= ano_fim ) %>%
    arrange(desc(year))
  
  indice = 1
  
  # Função recursiva para calcular os deflatores
  calcular_deflator <- function(ano_atual) {
    if (ano_atual == ano_base) {
      return(1)
    } else if (ano_atual > ano_base) {
      variacao_anual <- prod(1 + serie_filtrada$Valor[serie_filtrada$year == (ano_base - 1)] / 100)
      return(calcular_deflator(ano_atual - 1) / variacao_anual)
    } else {
      variacao_anual <- prod(1 + serie_filtrada$Valor[serie_filtrada$year == ano_atual] / 100)
      return(calcular_deflator(ano_atual + 1) * variacao_anual)
    }
  }
  
  for (i in seq_along(anos)) {
    deflators[i] <- calcular_deflator(anos[i])
  }
  
  tabela_final <- data.frame(year = anos, deflator_BRL = deflators) %>% arrange(year)
  
  cat("deflator automatico atualizado para", ano_fim, "\n")
  return(tabela_final)
  #     
  
}


#aplica a funcao de deflator sobre os valores e aplica a tabela de cambio
# deflate_and_exchange <- function(tabela_deflator, base_select_deflator, tabela_cambio) {
# 
#     base_select_deflator <- base_select_deflator %>%
#       left_join(tabela_deflator, by= "year") %>%
#       left_join(tabela_cambio, by= "year") %>%
#       mutate(value_brl_deflated = as.numeric(value_original_currency * deflator),
#              value_usd = value_brl_deflated/bid)
# 
# 
#     return(base_select_deflator)
#     cat("deflator e cambio aplicados sobre a base de dados")
#   }

#DeflateV3
# caso original_currency == 'BRL', value_BRLm = value_original_currency,  value_brl_deflated = as.numeric(value_original_currency * deflator),  value_USDm= value_BRLm/bid,  
## Caso original_currency == 'USD', value_USDm = original_currency, 

#deflateUSD
# caso original_currency == 'BRL',value_USDm_deflated = value_USDm*deflator_USD
# Caso original_currency == 'USD', value_USDm_deflated = value_USDm*deflator_USD, value_BRLm = value_USDm*bid,  value_brl_deflated = value_USDm_deflated*bid


deflate_and_exchange_Landuse <- function(tabela_deflator, base_select_deflator, tabela_cambio) {
  
  base_select_deflator <- base_select_deflator %>%
    left_join(tabela_deflator, by = "year") %>%
    left_join(tabela_cambio, by = "year") %>%
    mutate(
      # BRL case
      value_BRLm = ifelse(original_currency == "USD",
                          as.numeric(value_original_currency * cambio),
                          value_original_currency),
      value_brl_deflated = as.numeric(as.numeric(value_BRLm) * deflator_BRL),
      value_USDm = ifelse(original_currency == "BRL",
                          as.numeric(as.numeric(value_BRLm)/cambio),
                          value_original_currency)
      # USD to BRL (for completeness)
      
    )
  
  cat("Deflator e câmbio aplicados sobre a base de dados\n")
  return(base_select_deflator)
}



deflate_and_exchange <- function(tabela_deflator, base_select_deflator, tabela_cambio) {
  
  base_select_deflator <- base_select_deflator %>%
    left_join(tabela_deflator, by = "year") %>%
    left_join(tabela_cambio, by = "year") %>%
    mutate(
      # BRL case
      value_BRLm = ifelse(original_currency == "USD", ## se for em dolar originalmente, o valor de BRL tem que aplicar cambio no original currency, se náo for USD é brl e ai so pega o valor de original em brl
                          as.numeric(value_BRLm * cambio),
                          value_BRLm),
      value_brl_deflated = as.numeric(as.numeric(value_BRLm) * deflator_BRL), #aplica deflacao brasileira sobre BRL
      value_USDm = ifelse(original_currency == "BRL", ## se for em brl o original currency, o valor de USDm tem que aplicar o cambio no original currency, se for dolar, é o valor em dolar.  
                          as.numeric(as.numeric(value_BRLm)/cambio),
                          value_USDm)
      # USD to BRL (for completeness)
      
    )
  
  cat("Deflator e câmbio aplicados sobre a base de dados\n")
  return(base_select_deflator)
}


deflate_and_exchange_AllData <- function(base_select_deflator) {
  
  base_select_deflator <- base_select_deflator %>%
   # left_join(tabela_deflator, by = "year") %>%
    #left_join(tabela_cambio, by = "year") %>%
    mutate(
      # BRL case
      value_BRLm = ifelse(original_currency == "USD",
                          as.numeric(value_BRLm * taxa_cambio_dolar),
                          value_BRLm),
      value_BRLm_deflated = as.numeric(as.numeric(value_BRLm) * deflator_BRL),
      value_USDm = ifelse(original_currency == "BRL",
                          as.numeric(as.numeric(value_BRLm)/taxa_cambio_dolar),
                          value_USDm)
      # USD to BRL (for completeness)
      
    )
  
  cat("Deflator e câmbio aplicados sobre a base de dados\n")
  return(base_select_deflator)
}










# ########################## directories ############################
# #criando a funcao para atualizar a tabvela de deflator
# deflator_automatico <- function(ano_ini, ano_fim, base, current_year) {
#   # Create character vector for the period
#   # Build vector
#   if (ano_ini == ano_fim & current_year == T) {
#     #print("criando deflator para o ano corrente...")
#     # Criar a tabela
#     tabela_final <- data.frame(
#       year = ano_ini,
#       deflator = 1
#     )
#     
#     cat("deflator para o ano corrente é igual a 1")
#     return(tabela_final)
#     
#   }
#   else {
#     #print("ano_ini diferente de ano_fim ")
#     # Criar vetor de strings no formato "ano12"
#     datas <- paste0(ano_ini:ano_fim, "12")
#     
#     
#     ##pegando a tabela atualizada do sidra
#     sidra <- get_sidra(
#       x = 1737,                          # Tabela com dados de IPCA por grupos
#       #variable = 63,                     # Variação acumulada no ano (%)
#       period = datas,          # últimos 120 meses (ou ajuste como quiser)
#       geo = "Brazil",                    # Nível Brasil
#       #classific = "c315",                # Classificação: grupos de produtos
#       category = list("7169"),           # 7169 = Índice geral
#       header = T,
#       format = 3                         # Data.frame limpo
#     )
#     
#     ibge_ipca <- sidra %>%
#       filter(Variável == "IPCA - Variação acumulada em 12 meses")
#     
#     
#     serie_basedosdados <- ibge_ipca
#     
#     # selecao e filtros de valores de interesse ao calculo, queremos sempre a variacao anual, por isso o mes == 12
#     tabela_final <- serie_basedosdados %>% 
#       separate(Mês, into = c("mes", "ano"), sep = " ") %>%
#       select(ano, Valor) %>% 
#       dplyr::rename("deflator" = "Valor",
#                     "year" = "ano") %>%
#       mutate(year = as.integer(year)) %>%
#       filter(year >= ano_ini & year <= ano_fim ) %>% 
#       arrange(desc(year))
#     
#     if(current_year == T){
#       # Criar a tabela
#       tabela_final <- tabela_final %>%
#         mutate(deflator = if_else(year == ano_fim, 1, deflator))
#       cat("deflator para o ano corrente é igual a 1\n")
#     }
#     
#     cat("deflator automatico atualizado para", ano_fim, "\n")
#     return(tabela_final)
#     
#   }
#   }
# 
# #aplica a funcao de deflator sobre os valores e aplica a tabela de cambio
# deflate_and_exchange <- function(tabela_deflator, base_select_deflator, tabela_cambio) {
#     
#     base_select_deflator <- base_select_deflator %>% 
#       left_join(tabela_deflator, by= "year") %>%
#       left_join(tabela_cambio, by= "year") %>%  
#       mutate(value_brl_deflated = as.numeric(value_original_currency * deflator),
#              value_usd = value_brl_deflated/bid)
#     
#     
#     return(base_select_deflator)
#     cat("deflator e cambio aplicados sobre a base de dados")
#   }
#   
# 
# #teste <- deflator_automatico(ano_ini, ano_fim, ibge_ipca)
# # 
# # print("informe como variavel ano_ini, ano_fim (será a base do deflator) e anos =  seq(ano_fim,ano_ini, -1) para a prox etapa")
# # 
# # print("rode: calculo_deflator(tabela_deflator = deflator_automatico(ano_ini, ano_fim, anos), base para aplicacao da funcao))")
# # 
# 
# # 
# # # write.xlsx(base_deflator_aplicado,"base_deflacionada.xlsx")
# 
# 

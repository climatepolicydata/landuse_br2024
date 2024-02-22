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
               dplyr)

########################## directories ############################
root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
  
ibge_ipca <- read.xlsx("A:/macro/IPCA/cleanData/ipca_ibge_cl.xlsx")

ibge_ipca <- ibge_ipca %>% 
  mutate(variacao_doze_meses = suppressWarnings(as.numeric(variacao_doze_meses)))

#criando a funcao

deflator_automatico <- function(ano_ini, ano_fim, anos, base) {
  
  # Defina o seu projeto no Google Cloud, é importante criar o projeto e colar o id no "set_billing_id". Fora isso, nao funcionarah
  # Existe um bug no datalake da Base dos Dados que não permite o download direto.
  # set_billing_id("scenic-notch-360215")
  # 
  # # criacao do data frame direto da base de dados
  # serie_basedosdados <- basedosdados::bdplyr("br_ibge_ipca.mes_brasil") %>% bd_collect()
  
  serie_basedosdados <- base
  
  # selecao e filtros de valores de interesse ao calculo, queremos sempre a variacao anual, por isso o mes == 12
  serie_filtrada <- serie_basedosdados %>% 
    select(ano, mes, variacao_doze_meses) %>% 
    filter(mes == 12,ano >= ano_ini & ano <= ano_fim ) %>% 
    arrange(desc(ano))
  
  indice = 1
  
  #criacao do data frame para o deflator
  for (l in anos) {
    # chamei novamente a base feita pela funcao do api, pois a base precisa ser percorrida ano a ano e
    # se nao criarmos essa tabela, a tabela a ser percorrida novamente terá sempre o ano inicial como observacao
    tabela <- serie_filtrada 
    
    tabela <- tabela %>% 
    filter(ano == l)
    
    if (l == ano_fim) {
      tabela <- tabela %>%  mutate(deflator = indice)
      tabela_final <- tabela
      indice_ano_anterior = indice * (1+ (tabela$variacao_doze_meses/100))
    } else {
      tabela <- tabela %>% mutate(deflator = indice_ano_anterior)
      
      tabela_final <- rbind(tabela_final, tabela)
      indice_ano_anterior = indice_ano_anterior * (1 + (tabela$variacao_doze_meses/100))
    }
  }
  tabela_final <- tabela_final %>% 
    select(ano, deflator) %>%
    dplyr::rename(year = ano) %>% 
    arrange(year)
  return(tabela_final)
}

#aplicando a funcao na base 
#essa funcao eh arbitraria, devido as diferentes variáveis que queremos multiplicar e diferentes bases

calculo_deflator <- function(tabela_deflator, base_select_deflator) {
  
  base_select_deflator <- base_select_deflator %>% 
    left_join(tabela_deflator, by= "year")%>%
    mutate(value_brl_deflated = as.numeric(value_brl * deflator))
  
  
  return(base_select_deflator)
  
}
# eh necessario a escolha dos anos nos quais quer criar os indices. Assim, a funcao toma como base/indice = 1 o ano final

ano_ini = 2015
ano_fim = 2020

#a variavel anos completa os anos no intervalo de anos escolhidos acima.
anos = seq(ano_fim,ano_ini, -1)

#Aqui utilizamos as duas funcoes para criar o objeto já com os calculos. Adicionamos os anos escolhidos junto a variavel anos na funcao...
# que cria e limpa a tabela do deflator e ela interage com a segunda funcao que necessita da base em que se quer aplicar o deflator.


teste <- deflator_automatico(ano_ini, ano_fim, anos,ibge_ipca)
# 
# print("informe como variavel ano_ini, ano_fim (será a base do deflator) e anos =  seq(ano_fim,ano_ini, -1) para a prox etapa")
# 
# print("rode: calculo_deflator(tabela_deflator = deflator_automatico(ano_ini, ano_fim, anos), base para aplicacao da funcao))")
# 

# 
# # write.xlsx(base_deflator_aplicado,"base_deflacionada.xlsx")



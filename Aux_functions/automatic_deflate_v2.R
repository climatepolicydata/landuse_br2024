# Author : Renan Morais Florias
# Date: 23-05-2024
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

########################## Directories ############################
root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

ibge_ipca <- read.xlsx("A:/macro/IPCA/cleanData/ipca_ibge_cl.xlsx")

ibge_ipca <- ibge_ipca %>% 
  mutate(variacao_doze_meses = suppressWarnings(as.numeric(variacao_doze_meses)))

deflator_automatico <- function(ano_ini, ano_fim, ano_base, serie_basedosdados) {
  
  # Seleção e filtros de valores de interesse ao cálculo, queremos sempre a variação anual, por isso o mês == 12
  serie_filtrada <- serie_basedosdados %>% 
    filter(ano >= ano_ini & ano <= ano_fim) %>% 
    arrange(ano)
  
  # Verificando se o ano base está no intervalo fornecido
  if (!(ano_base >= ano_ini && ano_base <= ano_fim)) {
    stop("O ano base deve estar dentro do intervalo de anos fornecido.")
  }
  
  anos <- seq(ano_fim, ano_ini, -1)
  deflators <- numeric(length(anos))
  
  # Função recursiva para calcular os deflatores
  calcular_deflator <- function(ano_atual) {
    if (ano_atual == ano_base) {
      return(1)
    } else if (ano_atual > ano_base) {
      variacao_anual <- prod(1 + serie_filtrada$variacao_doze_meses[serie_filtrada$ano == (ano_base - 1)] / 100)
      return(calcular_deflator(ano_atual - 1) / variacao_anual)
    } else {
      variacao_anual <- prod(1 + serie_filtrada$variacao_doze_meses[serie_filtrada$ano == ano_atual] / 100)
      return(calcular_deflator(ano_atual + 1) * variacao_anual)
    }
  }
  
  for (i in seq_along(anos)) {
    deflators[i] <- calcular_deflator(anos[i])
  }
  
  tabela_final <- data.frame(ano = anos, deflator = deflators) %>% arrange(ano)
  
  return(tabela_final)
}


# Definindo os anos de interesse
ano_ini = 2015
ano_fim = 2023  # Inclui 2023
ano_base = 2016  # Define o ano base

resultado <- deflator_automatico(ano_ini, ano_fim, ano_base, serie_basedosdados = ibge_ipca)

valores_monetarios <- data.frame(
  ano = c(2019, 2020, 2021, 2022, 2023),
  valor = c(1000, 1100, 1200, 1250, 1300)  # Valores monetários em reais
)

# Mesclando os valores monetários com os deflatores
valores_monetarios <- merge(valores_monetarios, resultado, by = "ano")

# Ajustando os valores de dinheiro de acordo com os deflatores
valores_monetarios$valor_ajustado <- valores_monetarios$valor / valores_monetarios$deflator

# Exibindo o resultado
print(valores_monetarios)

# Aqui utilizamos as duas funções para criar o objeto já com os cálculos.
teste <- deflator_automatico(ano_ini, ano_fim, ano_base, ibge_ipca)


# Author : Renan Morais Florias (adaptação Felipe Simplicio)
# Date:  23.05.2024
# Email: renan.florias@cpiglobal.org
# Goal: Rotina para captação dos dados anuais da taxa de cambio

pacman::p_load(tidyverse,
               readxl,
               readr,
               openxlsx,
               data.table,
               writexl,
               ggplot2,
               janitor,
               GetBCBData,
               rbcb,
               scales,
               stringi)



coleta_dados_sgs <- function(series, datainicial = "01/01/2012", datafinal = format(Sys.time(), "%d/%m/%Y")) {
  # Argumentos: vetor de séries, datainicial que pode ser manualmente alterada e datafinal que automaticamente usa a data de hoje
  # Cria estrutura de repetição para percorrer vetor com códigos de séries e depois juntar todas em um único dataframe
  
  base <- NULL
  
  for (i in seq_along(series)) {
    url <- paste0("http://api.bcb.gov.br/dados/serie/bcdata.sgs.", series[i], 
                  "/dados?formato=csv&dataInicial=", datainicial, "&dataFinal=", datafinal)
    dados <- read.csv(url, sep = ";")
    dados[,-1] <- as.numeric(gsub(",", ".", dados[,-1])) # As colunas do dataframe em objetos numéricos exceto a da data
    colnames(dados) <- c('data', 'valor') # Nomeia as colunas do dataframe
    
    if (is.null(base)) {
      base <- dados # Primeira repetição cria o dataframe
    } else {
      base <- merge(base, dados, by = "data", all = TRUE) # Demais repetições agregam colunas ao dataframe criado
    }
    print(paste(i, length(series), sep = '/')) # Printa o progresso da repetição
  }
  
  base$data <- as.Date(base$data, "%d/%m/%Y")
  base$ano <- as.integer(format(base$data, "%Y")) # Transforma coluna de data no formato de data
  base <- base[order(base$data),] # Ordena o dataframe de acordo com a data
  
  base <- base %>% select(ano, valor) %>% distinct() # Seleciona e renomeia as colunas de interesse
  base <- base %>% dplyr::rename(year = ano, cambio = valor)
  base <- base %>% dplyr::mutate(year = as.numeric(year))
  
  return(base)
}

# Função para multiplicar o cambio na base escolhida
calculo_cambio <- function(cambio_sgs, base_select) {
  base_select <- base_select %>%
    left_join(cambio_sgs, by = "year") %>%
    dplyr::mutate(value_brl = as.numeric(value_foreign_currency * cambio))
  
  return(base_select)
}


# Escolha dos argumentos para a criação da média anual
serie <- 3694 # Variável com código para a taxa diária de câmbio do BCB

cambio_sgs <- coleta_dados_sgs(serie) # Criando objeto em que ficam guardadas as séries

# # Exemplo de base_select
base_select <- data.frame(
  year = c(2020,2021,2022,2023),
  value_foreign_currency = c(1.2, 1.3, 1.4, 1.5)
)
# 
# # Aplicando a função calculo_cambio
resultado <- calculo_cambio(cambio_sgs, base_select)
# 
# print(resultado)

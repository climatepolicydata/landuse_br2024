# Author : Renan Morais Florias (adaptação Felipe Simplicio)
# Date:  02.08.2022
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


#Criando função para coleta de séries
coleta_dados_sgs = function(series,datainicial="01/01/2012", datafinal = format(Sys.time(), "%d/%m/%Y")){
  #Argumentos: vetor de séries, datainicial que pode ser manualmente alterada e datafinal que automaticamente usa a data de hoje
  #Cria estrutura de repetição para percorrer vetor com códigos de séries e depois juntar todas em um único dataframe
  for (i in 1:length(series)){
    dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",series[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
    dados[,-1] = as.numeric(gsub(",",".",dados[,-1])) #As colunas do dataframe em objetos numéricos exceto a da data
    nome_coluna = series[i] #Nomeia cada coluna do dataframe com o código da série
    colnames(dados) = c('data', nome_coluna)
    nome_arquivo = paste("dados", i, sep = "") #Nomeia os vários arquivos intermediários que são criados com cada série
    assign(nome_arquivo, dados)
    
    if(i==1)
      base = dados1 #Primeira repetição cria o dataframe
    else
      base = merge(base, dados, by = "data", all = T) #Demais repetições agregam colunas ao dataframe criado
    print(paste(i, length(series), sep = '/')) #Printa o progresso da repetição
  }
  
  base$data = as.Date(base$data, "%d/%m/%Y")
  base$ano = as.integer(format(base$data,"%Y"))#Transforma coluna de data no formato de data
  base = base[order(base$data),] #Ordena o dataframe de acordo com a data
  
  base <- base %>% select(ano, `3694`)
  base <- base %>% dplyr::rename(c(year = ano, cambio = `3694`))
  return(base)
}

#funcao para multiplicar o cambio na base escolhida
# a funcao eh arbitraria devido a escolha de variáveis a serem multiplicadas e o formato do data frame da base em questao.
calculo_cambio <- function(cambio_sgs, base_select) {
  try(log("not a number"), silent = TRUE)
  base_select <- base_select %>% 
    left_join(cambio_sgs, by= "year")%>%
    mutate(value_brl=as.numeric(value_foreign_currency * cambio))
  
  
  return(base_select)
}

#escolha dos argumentos para a criacao da media anual

serie = 3694 #Variavel com código para a taxa diaria de cambio do bcb

cambio_sgs = coleta_dados_sgs(serie) #Criando objeto em que ficam guardados as séries

#base_cambio_aplicado <- calculo_cambio(cambio_sgs = coleta_dados_sgs(serie) , "informe a base desejada")

# write.csv2(taxa_cambio, "Exemplo.csv", row.names = F) #Salvando arquivo csv em padrão brasileiro




    
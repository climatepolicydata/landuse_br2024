library(tidyverse)
library(readxl)
fnmc <- read_rds("A:\\projects\\landuse_br2024\\fnmc\\Clean_Data\\fnmc_dados_abertos_Abril_02_04_2024_clear.rds")


source_landscape_contrapartida <- read_excel("A:\\projects\\landuse_br2024\\fnmc\\09_fnmc_relational_tables.xlsx",sheet = "source_landscape_contrapartida") 
channel_landscape <- read_excel("A:\\projects\\landuse_br2024\\fnmc\\09_fnmc_relational_tables.xlsx",sheet = "channel_landscape") 
sector_landscape_climate_use <- read_excel("A:\\projects\\landuse_br2024\\fnmc\\09_fnmc_relational_tables.xlsx",sheet = "sector_landscape_climate_use") 
source_landscape_contrapartida
source_landscape_contrapartida <- source_landscape_contrapartida%>% dplyr::rename(key_join = source_original,source_finance_landscape = source_landscape) %>% select(-tipo_de_instituicao, -instituicao_executora)
channel_landscape <- channel_landscape %>% select(channel_original,channel_landscape)
sector_landscape_climate_use <- sector_landscape_climate_use %>% select(-id_pdf_fnmc)
# Separando em Concedido e contrapartida

fnmc <- fnmc %>% dplyr::filter((ano >= 2021) & (ano <= 2023)) 
fnmc %>% names
fnmc_concedido <- fnmc %>% select(-valor_contrapartida) %>% as_tibble()
fnmc_contrapartida<- fnmc %>% select(-valor_fnmc) %>% as_tibble()

 # Realizando landscape para Contrapartida
 #Primeiro criando a key para dar join com as tabelas relacionais
 fnmc_contrapartida <- fnmc_contrapartida %>% dplyr::mutate(key_join = str_c(tipo_de_instituicao,instituicao_executora,sep = "-"))

fnmc_contrapartida_landscape <- fnmc_contrapartida %>% dplyr::mutate(id_original = no_do_instrumento_de_repasse,
data_source = "FNMC" , 
year = ano,
project_name = nome_do_projeto,
project_description = "-",
source_original = "FNMC contrapartida") %>% inner_join(source_landscape_contrapartida,by = "key_join") %>% dplyr::mutate(value_original_currency = valor_contrapartida,
original_currency = "BRL", channel_original = str_c(tipo_de_instituicao,instituicao_executora,sep = "-")) %>% inner_join(channel_landscape, by = "channel_original") %>% dplyr::mutate(
  instrument_original = "Contrapartida Fundo Clima não reembolsáveis",instrument_landscape = "Grants")  %>% inner_join(
    sector_landscape_climate_use, by = "id_original"
  ) %>%dplyr::mutate(rio_marker="-",beneficiary_original = "-",beneficiary_public_private="-",localization_original = uf,region="-",
  uf = uf,municipality = "-")%>%select(-valor_repassado,-valor_nao_repassado,-ano,-no_do_instrumento_de_repasse,-nome_do_projeto,- instituicao_executora,-tipo_de_instituicao, -data_de_inicio_da_da_vigencia,-data_de_fim_da_vigencia,-valor_contrapartida,-key_join,-valor_total,-no_processo) %>% dplyr::mutate(sector_original = "-",subactivity_landscape="-",subsector_original = "-")

fnmc_contrapartida_landscape %>% view
#Realizando landscape para concedido
fnmc_concedido_landscape <- fnmc_concedido %>% dplyr::mutate(id_original = no_do_instrumento_de_repasse,
data_source = "FNMC",
year = ano,
project_name = nome_do_projeto,
project_description="-",
source_original = "FNMC concedido", 
source_finance_landscape = "Federal and state governments",
origin_domestic_international = "National",
origin_private_public = "Public",
value_original_currency = valor_fnmc,
original_currency = "BRL",
channel_original = str_c(tipo_de_instituicao,instituicao_executora,sep = "-")) %>% inner_join(channel_landscape,by = "channel_original") %>% dplyr::mutate(instrument_original = "Fundo Clima não reembolsáveis",instrument_landscape = "Grants") %>% inner_join(sector_landscape_climate_use,by = "id_original")%>%dplyr::mutate(sector_original = "-",subactivity_landscape="-",subsector_original = "-",
rio_marker = "-", beneficiary_original="-",beneficiary_public_private = "-",localization_original = uf,region = "-",uf = uf,municipality="-") %>%
    select(-valor_repassado,-no_processo,-valor_fnmc,-ano,-no_do_instrumento_de_repasse,-nome_do_projeto,- instituicao_executora,-tipo_de_instituicao, -data_de_inicio_da_da_vigencia,-data_de_fim_da_vigencia,-valor_nao_repassado,-valor_total)
# Dando rowbind
FNMC_Landscape <- rbind(fnmc_concedido_landscape,fnmc_contrapartida_landscape)

#Fazendo Deflacao
ibge_ipca <- read_excel("A:\\macro\\IPCA\\cleanData/ipca_ibge_cl.xlsx")
ibge_ipca <- ibge_ipca %>% 
  dplyr::mutate(variacao_doze_meses = suppressWarnings(as.numeric(variacao_doze_meses)))
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
    dplyr::filter(mes == 12,ano >= ano_ini & ano <= ano_fim ) %>% 
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

ano_ini = 2021
ano_fim = 2023
anos = seq(ano_fim,ano_ini, -1)
teste <- deflator_automatico(ano_ini, ano_fim, anos,ibge_ipca)
base_select_deflator <- FNMC_Landscape %>% 
    left_join(teste, by= "year")%>%
    dplyr::mutate(value_brl_deflated = value_original_currency * deflator)
base_select_deflator %>% view
# Fazendo a dolarizacao
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
cambio_sgs = coleta_dados_sgs(3694) 
tabela_cambio <-cambio_sgs %>% 
  dplyr::filter(year >= 2021 & year <= 2023)

base_select_deflator = base_select_deflator %>% 
  left_join(tabela_cambio,by='year') %>% 
  dplyr::mutate(value_usd = value_brl_deflated/cambio)
FNMC_Landscape_Final <- base_select_deflator %>% select(-deflator,-cambio)
FNMC_Landscape_Final%>%view
FNMC_Landscape_Final$value_original_currency %>% sum

FNMC_Landscape_Final %>% write.csv2("A:\\projects\\landuse_br2024\\fnmc\\FNMC_Landscape2024.csv")
FNMC_Landscape_Final %>% write_rds("A:\\projects\\landuse_br2024\\fnmc\\FNMC_Landscape2024.rds")

# Criando gráficos de evolução
fnmc_2023 <- read_csv2("A:\\projects\\brlanduse_landscape102023\\gov_fnmc\\FNMC_Landscape_02_04_2024.csv")
fnmc_2024 <- read_rds("A:\\projects\\landuse_br2024\\fnmc\\Preview Data\\FNMC_Landscape2024.rds")

ano_ini = 2015
ano_fim = 2023
anos = seq(ano_fim,ano_ini, -1)
teste <- deflator_automatico(ano_ini, ano_fim, anos,ibge_ipca)
base_select_deflator_2023 <- fnmc_2023 %>% 
  left_join(teste, by= "year")%>%
<<<<<<< HEAD
  mutate(value_brl_deflated_23 = value_original_currency * deflator)
=======
  dplyr::mutate(value_brl_deflated = value_original_currency * deflator)
>>>>>>> 8aa2f48a3d3cb2c8f50143e98d273906a952709f

base_select_deflator_2024 <- fnmc_2024 %>% 
  left_join(teste, by= "year")%>%
<<<<<<< HEAD
  mutate(value_brl_deflated_23 = value_original_currency * deflator)


asd <- base_select_deflator_2024 %>% group_by(year) %>% summarize(SomaNominal = sum(value_original_currency),
                                                           SomaDeflacao = sum(value_brl_deflated_23))
asd %>% write.xlsx("asd.xlsx")
getwd()
=======
  dplyr::mutate(value_brl_deflated = value_original_currency * deflator)
base_select_deflator_2023
a <- rbind(base_select_deflator_2023,base_select_deflator_2024)
a%>%view
a %>% group_by(climate_component,year) %>% summarize(sum(value_brl_deflated) )%>% view
>>>>>>> 8aa2f48a3d3cb2c8f50143e98d273906a952709f

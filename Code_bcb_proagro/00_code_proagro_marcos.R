##################

# Author : Marcos Duarte
# Date: 06-11-2023
# Email: marcos.duarte@cpiglobal.org
# Goal: filter and clean, Relational_transformation & Sectoral_select, and Outputladscape ,  - PROAGRO
# resource: Programa de Garantia da Atividade Agropecuária (Proagro) / BCB


########################### Libraries ######################################

pacman::p_load(tidyverse, 
               stringi, 
               janitor, 
               writexl,
               openxlsx, 
               httr,
               magrittr, 
               readr,
               data.table,
               dplyr,
               plyr,
               pivottabler,
               readxl)
options(scipen = 999)
##### directory #########
root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
root_servidor <- paste0("A:/")
finance <- paste0(root_servidor, "finance/")
proagro <- paste0(finance, "bcb_proagro/")


##########################################################################################
#######################  01 - Clean and Select ########################################### 
########################################################################################## 

#######

anos <- 2015:2023
lista_de_dataframes <- list()
# Loop para ler e processar os arquivos
for (i in anos) {
  # Construa o nome do arquivo com base no ano
  nome_arquivo <- paste0('proagro_', i, "_programa_subprograma_produto.csv")
  
  # Leia o arquivo
  caminho_arquivo <- file.path(proagro, "rawData/", nome_arquivo)
  df <- read.csv(caminho_arquivo, header = TRUE)
  
  # Realize a seleção e a mutação
  df <- df %>%
    select(Programa, Subprograma, Produto, ValorAdiconal) %>%
    dplyr::mutate(ano = i) 
  # Adicione o data frame à lista
  lista_de_dataframes[[i - 2014]] <- df
}
## REVISAR ISTO

clean_proagro <- bind_rows(lista_de_dataframes) %>%
  dplyr::mutate(ValorAdiconal = gsub("\\.", "", ValorAdiconal)) %>% ## retirando os pontos 
  dplyr::mutate(ValorAdiconal = as.numeric(str_replace_all(ValorAdiconal, ",", "."))) ## substituindo as virgulas por pontos 
colnames(clean_proagro)
saveRDS(clean_proagro,'A:/projects/landuse_br2024/bcb_proagro/output/01_clean_proagro.rds'
 )


##########################################################################################
############  02 and 03  - Relational_transformation & Sectoral_select  ################################## 
########################################################################################## 


relational_proagro <- clean_proagro %>%
  mutate_all(tolower) %>%
  dplyr::mutate(Produto = gsub('"', '', Produto),
         sector_original = case_when(
           Produto %in% c('eucalipto', 'madeira', 'seringueira') ~ 'agrícola',
           Produto %in% c('bovinos') ~ 'pecuária',
           Produto %in% c('pastagem') ~ 'agrícola/pecuária',
           TRUE ~ 'agrícola'
         ),
         sector_landscape = case_when(
           Produto %in% c('eucalipto', 'madeira', 'seringueira', 'florestamento - tratos culturais') ~ 'Forest',
           Produto %in% c('bovinos', 'pastagem') ~ 'Cattle',
           TRUE ~ 'Crop'
         ),
         Programa = gsub('"', '', Programa),
         beneficiary_original = case_when(
           grepl("pronaf", Programa, ignore.case = TRUE) ~ "pronaf",
           grepl("pronamp", Programa, ignore.case = TRUE) ~ "pronamp",
           Programa == "financiamento sem vínculo a programa específico" ~ "outras linhas de crédito rural não especificadas",
           Programa == "não informado" ~ "outras linhas de crédito rural não especificadas",
           grepl("funcafé", Programa, ignore.case = TRUE) ~ "funcafé",
           TRUE ~ Programa
         ),
         beneficiary_landscape = case_when(
           beneficiary_original %in% c('pronaf', 'pncf') ~ 'family farmers',
           beneficiary_original %in% c('pronamp') ~ 'medium-scale rural producers',
           TRUE ~ 'rural producers'
         ),
         beneficiary_public_private = 'private'
  )
saveRDS(clean_proagro,'A:/projects/landuse_br2024/bcb_proagro/output/03_sectoral_select_proagro.rds'
)
unique(relational_proagro$Programa)
##########################################################################################
# 04 - Climate_select
# Todas as contratações do Proagro são consideradas como atividade alinhada a objetivos climáticos
##########################################################################################


##########################################################################################
# 05 - Outputladscape
##########################################################################################


output_proagro <- relational_proagro %>%
  dplyr::mutate(
         data_sorce = 'bcb_proagro',
         year = ano,
         project_name = paste0('proagro_',Subprograma),
         project_description = '-',
         source_original = 'produtor rural',
         source_finance_landscape = 'Rural producers',
         origin_domestic_international = 'National',
         origin_private_public = 'Private',
         value_original_currency = ValorAdiconal,
         original_currency = 'BRL',
         year = as.numeric(year),
         value_original_currency = as.numeric(value_original_currency))



source(paste0(root,github,"/GitHub/brlanduse_landscape102023/Aux_functions/automatic_deflate.r"))

source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/funcao_taxa_cambio_v3.r"))

ano_ini = 2015
ano_fim = 2023

#a variavel anos completa os anos no intervalo de anos escolhidos acima.
anos = seq(ano_fim,ano_ini, -1)


tabela_deflator <- deflator_automatico(ano_ini, ano_fim, anos,ibge_ipca)


cambio_sgs = coleta_dados_sgs(serie) 

tabela_cambio <-cambio_sgs %>% 
  dplyr::filter(year >= 2015 & year <= 2023)


deflate_and_exchange <- function(tabela_deflator, base_select_deflator, tabela_cambio) {
  
  base_select_deflator <- base_select_deflator %>% 
    left_join(tabela_deflator, by= "year")%>%
    left_join(tabela_cambio, by= "year") %>%  
    dplyr::mutate(value_brl_deflated = as.numeric(value_original_currency * deflator),
                  value_usd = value_brl_deflated/cambio)
  
  
  return(base_select_deflator)
}


output_proagro_calculus <- deflate_and_exchange(tabela_deflator, output_proagro, tabela_cambio)


# ## IPCA Fuction - real currency
# ipca_year_f <- function(year, base) {
#   path_ipca <- paste0(root_servidor, 'macro/IPCA/cleanData/')
#   ipca <- read_excel(paste0(path_ipca, 'IPCA.xlsx'), sheet = 'export_ano')
#   
#   if (year == 2020) {
#     base1 <- base %>%
#       dplyr::mutate(ano = as.double(year)) %>%
#       left_join(ipca, by = c("ano" = "ano")) %>%
#       dplyr::mutate(
#         value_original_currency = as.numeric(value_original_currency),
#         ipca_index_2020 = as.numeric(ipca_index_2020),
#         value_brl_deflated = round(value_original_currency / ipca_index_2020,2)
#       ) %>%
#       select(-ano)
#     
#     return(base1)
#   }
# }
# 
# 
# 
# output_proagro <- ipca_year_f(2020, output_proagro) %>%
#   select(-mês, -ipca_ano, -ipca_index_2021 ,-ipca_index_2022) %>%
#   dplyr::mutate(instrument_original = paste0('proagro', Subprograma))
# 
# path_usd <- paste0(root_servidor, 'macro/txcambio_BRL_USD/rawData/')
# usd_exchange <- read.csv(paste0(path_usd,'bcb_sgs_3694_Exchange rate.eng.csv'), sep = ';')
# colnames(usd_exchange) <- c('year','exchange_cmu_usd')
# usd_exchange <-usd_exchange %>%
#   dplyr::mutate(exchange_cmu_usd = as.numeric(exchange_cmu_usd)) 
# 
# output_proagro <- left_join(output_proagro, usd_exchange, by = 'year') %>%
#   dplyr::mutate(value_usd = value_brl_deflated/exchange_cmu_usd  )
###
output_proagro_calculus <- output_proagro_calculus %>%
  dplyr::mutate(
    project_description = 'contribuição dos beneficiários do programa, denominada Adicional do proagro',
    instrument_original = paste0('proagro', Subprograma),
    instrument_landscape = 'Risk Management',
    subsector_original = '-',
    activity_landscape = 'serviços financeiros',
    subactivity_landscape = 'proagro',
    climate_component = 'Adaptation',
    rio_marker = '-',
    beneficiary_landscape = '-',
    beneficiary_public_private = '-',
    localization_original = '-',
    region = '-',
    uf = '-',
    municipality = '-',
    value_brl_deflated_ave_bi = (value_brl_deflated / 6) * (1 / 1e8),
    value_usd_ave_bi = (value_usd / 6) * (1 / 1e8),
    data_source = 'bcb_proagro',
    channel_original = 'Instituições Financeiras',
    channel_landscape = 'Financial Institution',
    id_original = "-") %>%
  select(id_original,
    data_source,
    year,
    project_name,
    project_description,
    source_original,
    source_finance_landscape,
    origin_domestic_international,
    origin_private_public,
    value_original_currency,
    original_currency,
    value_brl_deflated,
    value_usd,
    channel_original,
    channel_landscape,
    instrument_original,
    instrument_landscape,
    sector_original,
    sector_landscape,
    subsector_original,
    activity_landscape,
    subactivity_landscape,
    climate_component,
    rio_marker,
    beneficiary_original,
    beneficiary_landscape,
    beneficiary_public_private,
    localization_original,
    region,
    uf,
    municipality
  )


### SAVE OUTPUT LANDSCAPE

write.csv(output_proagro_calculus,'A:/projects/landuse_br2024/bcb_proagro/output/05_output_proagro.csv'
)
write_xlsx(output_proagro_calculus, 'A:/projects/landuse_br2024/bcb_proagro/output/05_output_proagro.xlsx')
saveRDS(output_proagro_calculus,'A:/projects/landuse_br2024/bcb_proagro/output/05_output_proagro.rds')

rm(df,lista_de_dataframes,relational_proagro, usd_exchange, clean_proagro)


    
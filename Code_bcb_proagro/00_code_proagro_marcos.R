##################

# Author : Marcos Duarte
# Date: 06-11-2023
# Email: marcos.duarte@cpiglobal.org
# Goal: filter and clean, Relational_transformation & Sectoral_select, and Outputladscape ,  - PROAGRO
# resource: Programa de Garantia da Atividade Agropecuária (Proagro) / BCB



##modified by Julia Niemeyer
# 17/07/2025

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

anos <- 2019:2024
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

saveRDS(clean_proagro,'A:/projects/landuse_br2024/bcb_proagro/output/01_clean_proagro_2025.rds')


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

saveRDS(clean_proagro,'A:/projects/landuse_br2024/bcb_proagro/output/03_sectoral_select_proagro_2025.rds'
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















### SAVE OUTPUT LANDSCAPE

write.csv(output_proagro_calculus,'A:/projects/landuse_br2024/bcb_proagro/output/05_output_proagro.csv'
)
write_xlsx(output_proagro_calculus, 'A:/projects/landuse_br2024/bcb_proagro/output/05_output_proagro.xlsx')
saveRDS(output_proagro_calculus,'A:/projects/landuse_br2024/bcb_proagro/output/05_output_proagro.rds')

rm(df,lista_de_dataframes,relational_proagro, usd_exchange, clean_proagro)


    
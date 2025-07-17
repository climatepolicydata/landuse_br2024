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

ano_ini = 2019 #the initial year to star analysis
ano_fim = 2024 #the final year to end your analysis
ano_base = 2024 #the year to base inflation

## set the path to your github clone
github <- "Documents/"


root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
root_servidor <- paste0("A:/")
finance <- paste0(root_servidor, "finance/")
proagro <- paste0(finance, "bcb_proagro/")


############### import databases #####################

# import landuse br database to get columns names and order 
Landscape_columns <- read_xlsx(paste0(root, "CPI/SP-Program - Brazil Landscape/2025/3. Data Scoping/Methodology files/LandscapeFormat_Colunas.xlsx"), sheet = "ColunasFinal") %>%
  select(`LAND USE`, `LANDSCAPE BRAZIL`)

DePara <- read_xlsx(paste0(root, "CPI/SP-Program - Brazil Landscape/2025/3. Data Scoping/Methodology files/LandscapeFormat_Colunas.xlsx"),  sheet = "DeParaLandUse_Sectors") 

## import keys database (sector_key_cpi)
planilha_uniqueKeys <- read_xlsx(paste0(root, "CPI/SP-Program - Brazil Landscape/2025/3. Data Scoping/Methodology files/UniqueKeys_ToSector_Subsector_Solution.xlsx")) 



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




############ apply deflated and exchange #######

# Inserir o caminho onde foi feito o clone do projeto 
root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

############## ATUALIZADO EM 2025 -- automatico -- atualiza com base em ano_ini e ano_fim
source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/automatic_deflate_v3.r"))

############# ATUALIZADO EM 2024 -- pega valores para deflacionar USD na base USD A:\\macro\\usd_FED\\rawData\\Inflation_FED.xls
source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/deflated_usd_v2.r"))

####### rodar essa função para atualizar a tabela de taxa de cambio
source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/funcao_taxa_cambio_v4.r"))

#le a tabela atualizada pela funcao acima
cambio_sgs = read.csv(paste0("A:\\projects\\landuse_br2024\\macro_databases\\tabela_cambio_", ano_ini, "-", ano_fim, ".csv")) #%>% select(-X)


tabela_deflator <- deflator_automatico(ano_ini, ano_fim, ibge_ipca)
tabela_deflatorUSD <- deflator_usd(ano_ini, ano_fim, usd_inflation)


tabela_cambio <-cambio_sgs %>% 
  filter(year >= ano_ini & year <= ano_fim)


output_proagro_calculus <- deflate_and_exchange_Landuse(tabela_deflator, output_proagro, tabela_cambio)
output_proagro_calculus2 <- calculo_deflator_usd(tabela_deflatorUSD, output_proagro_calculus, tabela_cambio)


###########################################################################
################################ LANDSCAPE BR #############################
########## 
### Renomeando para landscape br 2025 com base no Landscape format

########

### Renomeando para landscape br 2025 com base no Landscape format

## Primeiro vamos ver quais colunas que estão em land use e não tem na base para decidir se criamos ou se ignoramos
landuse_cols <- Landscape_columns$`LAND USE`
landscape_cols <- Landscape_columns$`LANDSCAPE BRAZIL`

# Nomes existentes no df original
df_cols <- names(output_proagro_calculus2)

# Quais colunas não existem no df original?
setdiff(landscape_cols, df_cols)



# Criar dicionário de renomeação ignorando NAs
rename_vector <- Landscape_columns %>%
  filter(!is.na(`LAND USE`)) %>%
  mutate(`LAND USE` = trimws(`LAND USE`)) %>%
  distinct() %>%
  deframe()  # cria named vector: nomes atuais -> novos nomes

# Filtra o vetor de renomeação para colunas que existem no df
rename_vector_valid <- rename_vector[names(rename_vector) %in% names(output_proagro_calculus2)]

# Renomeia apenas as colunas que existem
output_proagro_calculus_renamed <- output_proagro_calculus2 %>%
  rename_with(~ rename_vector_valid[.x], .cols = names(rename_vector_valid))




### SECTOR, SUBSECTOR, SOLUTION

output_proagro_calculus_renamed <- output_proagro_calculus_renamed %>%
  dplyr::mutate(sub_sector_cpi = case_when(
                Produto %in% c('florestas', 'eucalipto', 'madeira', 'seringueira', 'florestamento - tratos culturais', 'extrativismo', 'pinus') ~ 'Forestry',
                TRUE ~ 'Agriculture'
              ))


output_proagro_calculus_renamed <- output_proagro_calculus_renamed %>%
  mutate(sector_cpi = "Agriculture, Forestry, Other land uses and Fisheries",
         solution_cpi = " Climate resiliency building rural insurance")


### Adicionar sector_key_cpi: 
# a partir da coluna "Key_Sector", "Key_Subsector" e Key_Solution da planilha do excel UniqueKeys fazendo correspondência com "sector_cpi", subsector_cpi e solution_cpi

DePara.keysector <- planilha_uniqueKeys %>%
  select(Sector, Key_Sector) %>%
  mutate(
    Sector = trimws(Sector),
    Key_Sector = trimws(Key_Sector)
  ) %>%
  filter(!is.na(Sector), !is.na(Key_Sector), Sector != "") %>%  # remove entradas problemáticas
  distinct(Sector, Key_Sector) %>%
  deframe()


DePara.keysubsector <- planilha_uniqueKeys %>%
  select(Subsector, Key_Subsector) %>%
  mutate(
    Subsector = trimws(Subsector),
    Key_Subsector = trimws(Key_Subsector)
  ) %>%
  filter(!is.na(Subsector), !is.na(Key_Subsector), Subsector != "") %>%  # remove entradas problemáticas
  distinct(Subsector, Key_Subsector) %>%
  deframe()



DePara.keysolution <- planilha_uniqueKeys %>%
  select(Solution, Key_Solution) %>%
  mutate(
    Solution = trimws(Solution),
    Key_Solution = trimws(Key_Solution)
  ) %>%
  filter(!is.na(Solution), !is.na(Key_Solution), Solution != "") %>%  # remove entradas problemáticas
  distinct(Solution, Key_Solution) %>%
  deframe()



#Substitui valores com base no Depara
df_proagro_final <- output_proagro_calculus_renamed %>%
  mutate(keysector = trimws(sector_cpi),
         keysubsector = trimws(sub_sector_cpi),
         keysolution = trimws(solution_cpi)) %>%
  mutate(keysector = recode(keysector, !!!DePara.keysector),
         keysubsector = recode(keysubsector, !!!DePara.keysubsector),
         keysolution = recode(keysolution, !!!DePara.keysolution)) %>%
  mutate(sector_key_cpi = str_c(keysector, "_", keysubsector, "_", keysolution)) %>%
  select(- c(keysector, keysubsector, keysolution))



# Ve quais colunas ainda não existem para poder criar
dif_cols <- setdiff(landscape_cols, names(df_proagro_final))
#""ID_Landscape"       "country_origin_cpi" "region_origin_cpi"  "indigenous_cpi" 

# Só executa se houver colunas ausentes
if (length(dif_cols) > 0) {
  for (col in dif_cols) {
    df_proagro_final[[col]] <- NA
  }
}



df_proagro_final2 <- df_proagro_final %>%
  mutate(data_source = "bcb_proagro",
         institution_type_layer1 = "Public",
         institution_type_layer2 = "Household/Individual",
         domestic_international = "domestic",
         country_origin_cpi = "Brazil",
         region_origin_cpi = "Brazil",
         use_cpi = "Adaptation",
         instrument_cpi = "Risk Management",
         instrument_original = str_c("Proagro_", Programa),
         subsector_original = Produto,
         sector_original = Produto,
         channel_original = "Instituições Financeiras",
         channel_landscape = "Financial Institutions",
         activity_landscape = "Serviços financeiros",
         subactivity_landscape = "Proagro"
         ) %>%
  #bota na ordem de landscape
  select(Landscape_columns$`LANDSCAPE BRAZIL`)

	







### SAVE OUTPUT LANDSCAPE

write.csv(df_proagro_final2, paste0(root, '\\CPI\\SP-Program - Brazil Landscape\\2025\\3. Data Scoping\\Bases Nacionais\\bcb_proagro\\05_output_proagro_2025.csv'))

write_xlsx(df_proagro_final2, paste0(root, '\\CPI\\SP-Program - Brazil Landscape\\2025\\3. Data Scoping\\Bases Nacionais\\bcb_proagro\\05_output_proagro_2025.xlsx'))
saveRDS(df_proagro_final2, paste0(root, '\\CPI\\SP-Program - Brazil Landscape\\2025\\3. Data Scoping\\Bases Nacionais\\bcb_proagro\\05_output_proagro_2025.rds'))

rm(df,lista_de_dataframes,relational_proagro, usd_exchange, clean_proagro)


    
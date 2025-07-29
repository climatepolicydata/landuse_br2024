##################

# Author : Renan Morais
# Date: 02-01-2024
# Email: renanflorias@hotmail.com
# Goal: clear B3 CBIOS database
# resource: 


# Modified by Julia Niemeyer
# Date 29/05/2025


########################## ACTION NEEDED ##################################

# ## set anos de analise caso não esteja rodando pelo MASTER
ano_ini = 2019 #the initial year to star analysis
ano_fim = 2024 #the final year to end your analysis
ano_base = 2024 #the year to base inflation
# #
## set the path to your github clone
github <- "Documents/"

########################### Directories ########################################

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
########################### Libraries ######################################
pacman::p_load(tidyverse, 
               stringi, 
               janitor, 
               writexl,
               readxl,
               openxlsx, 
               httr,
               readr,
               data.table,
               dplyr,
               plyr,
               pivottabler)

##### directory #########

dir_b3_output<- ("A:/finance/b3_cbios/cleanData")

dir_b3_project <-("A:/projects/landuse_br2024/b3/output")

################ import databases #############

setwd(dir_b3_output)

df_b3_clear <- readRDS("cbios_b3_nd_clear.rds")


# import landuse br database to get columns names and order 
Landscape_columns <- readxl:::read_xlsx(paste0(root, "CPI/SP-Program - Brazil Landscape/2025/3. Data Scoping/Methodology files/LandscapeFormat_Colunas.xlsx"), sheet = "ColunasFinal") %>%
  select(`LAND USE`, `LANDSCAPE BRAZIL`)

DePara <- readxl:::read_xlsx(paste0(root, "CPI/SP-Program - Brazil Landscape/2025/3. Data Scoping/Methodology files/LandscapeFormat_Colunas.xlsx"), sheet = "DeParaLandUse_Sectors") 

## import keys database (sector_key_cpi)
planilha_uniqueKeys <- read_xlsx(paste0(root, "CPI/SP-Program - Brazil Landscape/2025/3. Data Scoping/Methodology files/UniqueKeys_ToSector_Subsector_Solution.xlsx")) 


#função para calcular os valores financeiros dos anos

CalculaValorFinanc <-  function(data, select_ano){
  

  valor_finc_ano <- data %>% select(ano, valor_financeiro) %>% 
    group_by(ano) %>% 
    filter(ano == select_ano) %>% 
    summarise(soma = sum(valor_financeiro/2)) %>% 
    mutate(ano = select_ano)
  
  return(valor_finc_ano)

}

## o for evita que digitalizemos a mesma função para os anos de interesse (dupla contagem).
#caso seja necessário aumentar o intervalo de anos a serem analisados basta acrescentar na lista "anos"

interval <- ano_ini:ano_fim

valor_financeiro <- data.frame()
tabela_valores_ano <- data.frame()

for (i in interval){
  
  valor_financeiro <- CalculaValorFinanc(df_b3_clear, i)
  tabela_valores_ano <- bind_rows(valor_financeiro,tabela_valores_ano)
  
}

tabela_valores_ano <- tabela_valores_ano %>% arrange(ano)


#### transform landscape

df_b3_transform_landscape <- tabela_valores_ano %>% 
  mutate(data_source = "b3_cbios",
         id_original = "-",
         project_name = "CBIOs - Decarbonization Credit - Tradded volume",
         project_description = "The Decarbonization Credit (CBIO) is an instrument adopted by RenovaBio as a tool to reach this target. CBIOs will be issued by biofuels producers and importers duly certified by the National Petroleum Agency (ANP), based on their purchase and sale invoices. Each CBIO is equivalent to 1 ton of CO2 avoided, it will not expire and can only be withdrawn from circulation when its retirement is requested.",
         source_original = "Fuel distributors - mandatory and non-mandatory buyers.",
         source_finance_landscape = "Corporations",
         origin_domestic_international = "National",
         origin_private_public = "Private",
         original_currency = "BRL",
         channel_original = "Instituições Financeiras",
         channel_landscape = "Financial Institution",
         instrument_original = "CBIOs - Decarbonization Credit",
         instrument_landscape = "CBIOs",
         sector_original = "Petróleo e Biocombustível",
         sector_landscape = "Bioenergy and fuels",
         subsector_original = "-",
         activity_landscape = "Production of biofuels, including biodiesel and bioethanol",
         subactivity_landscape = "Issuance of CBIOs by biofuel producers and importers based on commercialization of their production.",
         climate_component = "Mitigation",
         rio_marker = "-",
         beneficiary_original = "biofuel producer /importer",
         beneficiary_landscape = "Corporations",
         beneficiary_public_private = "Private",
         localization_original = "-",
         region = "-",
         uf = "-",
         municipality = "-") %>% 
  dplyr::rename(year = ano,
                value_original_currency = soma)

rm(tabela_valores_ano,valor_financeiro, interval, df_b3_clear)







############ apply deflatd and exchange #######


root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/automatic_deflate_v3.r"))
############# ATUALIZADO EM 2024 -- pega valores para deflacionar USD na base USD A:\\macro\\usd_FED\\rawData\\Inflation_FED.xls
source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/deflated_usd_v2.r"))

source(paste0(root,github,"/GitHub/landuse_br2024/Aux_functions/funcao_taxa_cambio_v4.r"))

#le a tabela atualizada pela funcao acima
cambio_sgs = read.csv(paste0("A:\\projects\\landuse_br2024\\macro_databases\\tabela_cambio_", ano_ini, "-", ano_fim, ".csv")) #%>% select(-X)



tabela_deflator <- deflator_automatico(ano_ini, ano_fim, ibge_ipca)
tabela_deflatorUSD <- deflator_usd(ano_ini, ano_fim, usd_inflation)


tabela_cambio <- cambio_sgs %>% 
  filter(year >= ano_ini & year <= ano_fim)


df_b3_cbios_calculus <- deflate_and_exchange_Landuse(tabela_deflator, df_b3_transform_landscape, tabela_cambio)
df_b3_cbios_calculus2 <- calculo_deflator_usd(tabela_deflatorUSD, df_b3_cbios_calculus)


###########################################################################
################################ LANDSCAPE BR #############################
########## 
### Renomeando para landscape br 2025 com base no Landscape format

## Primeiro vamos ver quais colunas que estão em land use e não tem na base para decidir se criamos ou se ignoramos
landuse_cols <- Landscape_columns$`LAND USE`
landscape_cols <- Landscape_columns$`LANDSCAPE BRAZIL`

# Nomes existentes no df original
df_cols <- names(df_b3_cbios_calculus2)

# Quais colunas não existem no df original?
setdiff(landuse_cols, df_cols)
#[1] NA               "sub_sector_cpi"


# Criar dicionário de renomeação ignorando NAs
rename_vector <- Landscape_columns %>%
  filter(!is.na(`LAND USE`)) %>%
  mutate(`LAND USE` = trimws(`LAND USE`)) %>%
  distinct() %>%
  deframe()  # cria named vector: nomes atuais -> novos nomes

# Filtra o vetor de renomeação para colunas que existem no df
rename_vector_valid <- rename_vector[names(rename_vector) %in% names(df_b3_cbios_calculus2)]

# Renomeia apenas as colunas que existem
df_b3_calculus_renamed <- df_b3_cbios_calculus2 %>%
  rename_with(~ rename_vector_valid[.x], .cols = names(rename_vector_valid))


### Fazer DePara do sub_sector_cpi com base em sheet = "DeParaLandUse"
DePara.sub_sector <- DePara %>%
  filter(sector_landscape == 'sector_landscape') %>%
  mutate(`Variavel Land Use` = trimws(`Variavel Land Use`),
         sub_sector_cpi = trimws(sub_sector_cpi)) %>%
  distinct(`Variavel Land Use`, sub_sector_cpi) %>%
  deframe()  # cria um vetor nomeado: "valor_antigo" = "valor_novo"


#Substitui valores do sub_sector_cpi com base no Depara
df_b3_dePara <- df_b3_calculus_renamed %>%
  mutate(sub_sector_cpi = recode(sub_sector_cpi, !!!DePara.sub_sector))

## Adicionar 'sector_cpi" com base em "no depara
### Fazer DePara 
DePara.sector <- DePara %>%
  filter(sector_landscape == 'sector_landscape') %>%
  mutate(sub_sector_cpi = trimws(sub_sector_cpi),
         sector_cpi = trimws(sector_cpi)) %>%
  distinct(sub_sector_cpi, sector_cpi) %>%
  deframe()  # cria um vetor nomeado: "valor_antigo" = "valor_novo"

#Substitui valores do sub_sector_cpi com base no Depara
df_b3_dePara <- df_b3_dePara %>%
  mutate(sector_cpi = recode(sub_sector_cpi, !!!DePara.sector))



### Inserir informações em solution_cpi com base em "Solution" do UniqueKeys com base na relational
# Atlas e SES é "Rural Insurence for Climate Resilience"
DePara.solution <- DePara %>%
  filter(sector_landscape == 'sector_landscape') %>%
  mutate(sub_sector_cpi = trimws(sub_sector_cpi),
         solution_cpi = trimws(solution_cpi)) %>%
  distinct(sub_sector_cpi, solution_cpi) %>%
  deframe()  # cria um vetor nomeado: "valor_antigo" = "valor_novo"


df_b3_dePara <- df_b3_dePara %>%
  mutate(solution_cpi = recode(sub_sector_cpi, !!!DePara.solution))



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
df_b3_final <- df_b3_dePara %>%
  mutate(keysector = trimws(sector_cpi),
         keysubsector = trimws(sub_sector_cpi),
         keysolution = trimws(solution_cpi)) %>%
  mutate(keysector = recode(keysector, !!!DePara.keysector),
         keysubsector = recode(keysubsector, !!!DePara.keysubsector),
         keysolution = recode(keysolution, !!!DePara.keysolution)) %>%
  mutate(sector_key_cpi = str_c(keysector, "_", keysubsector, "_", keysolution)) %>%
  select(- c(keysector, keysubsector, keysolution))




# Ve quais colunas ainda não existem para poder criar
dif_cols <- setdiff(landscape_cols, names(df_b3_final))
dif_cols
#""ID_Landscape"       "country_origin_cpi" "region_origin_cpi"  "indigenous_cpi" 

# Só executa se houver colunas ausentes
if (length(dif_cols) > 0) {
  for (col in dif_cols) {
    df_b3_final[[col]] <- NA
  }
}



df_b3_final2 <- df_b3_final %>%
  mutate(country_origin_cpi = "Brazil",
         region_origin_cpi = "Brazil",
         ID_Landscape = id_original) %>% 
  #bota na ordem de landscape
  select(Landscape_columns$`LANDSCAPE BRAZIL`)



#save in rds and excel

saveRDS(df_b3_final2, paste0(dir_b3_project, "/b3_cbios_landscape_final_", ano_ini, "-", ano_fim, ".rds"))
write.xlsx(df_b3_final2, paste0(dir_b3_project,"/b3_cbios_landscape_final_", ano_ini, "-", ano_fim, ".xlsx"))

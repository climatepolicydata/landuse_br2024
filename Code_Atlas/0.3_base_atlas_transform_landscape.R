
##################

# Author : Julia Niemeyer
# Date: 28/05/2025

########################### ACTION NEEDED ######################################
ano_ini = 2019 #the initial year to star analysis
ano_fim = 2024 #the final year to end your analysis
ano_base = 2024 #the year to base inflation

## set the path to your github clone
github <- "Documents/"

# set path to sabe output
path_output <- "A:/projects/landuse_br2024/atlas/output"

########################### Libraries ######################################

pacman::p_load(tidyverse, 
               stringi, 
               janitor, 
               writexl,
               readxl,
               openxlsx, 
               httr,
               magrittr, 
               readr,
               data.table,
               dplyr,
               plyr,
               pivottabler)

################## directories ###################

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
dir_sisser_mapa_dt_clean <- ("A:/finance/atlas_Seguro_Rural/cleanData")



############### import databases #####################

# import landuse br database to get columns names and order 
Landscape_columns <- read_xlsx(paste0(root, "CPI/SP-Program - Brazil Landscape/2025/3. Data Scoping/Methodology files/LandscapeFormat_Colunas.xlsx"), sheet = "ColunasFinal") %>%
  select(`LAND USE`, `LANDSCAPE BRAZIL`)

DePara <- read_xlsx(paste0(root, "CPI/SP-Program - Brazil Landscape/2025/3. Data Scoping/Methodology files/LandscapeFormat_Colunas.xlsx"),  sheet = "DeParaLandUse_Sectors") 

## import keys database (sector_key_cpi)
planilha_uniqueKeys <- read_xlsx(paste0(root, "CPI/SP-Program - Brazil Landscape/2025/3. Data Scoping/Methodology files/UniqueKeys_ToSector_Subsector_Solution.xlsx")) 


df_atlas <- readRDS(paste0(dir_sisser_mapa_dt_clean, "/atlas_2006_", ano_fim, "_clear.rds"))





###########################################################################
################################ LANDSCAPE BR #############################
########## 
### Renomeando para landscape br 2025 com base no Landscape format

## Primeiro vamos ver quais colunas que estão em land use e não tem na base para decidir se criamos ou se ignoramos
landuse_cols <- Landscape_columns$`LAND USE`
landscape_cols <- Landscape_columns$`LANDSCAPE BRAZIL`

# Nomes existentes no df original
df_cols <- names(df_atlas_calculus2)

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
rename_vector_valid <- rename_vector[names(rename_vector) %in% names(df_atlas_calculus2)]

# Renomeia apenas as colunas que existem
df_atlas_calculus_renamed <- df_atlas_calculus2 %>%
  rename_with(~ rename_vector_valid[.x], .cols = names(rename_vector_valid))


### Fazer DePara do sub_sector_cpi com base em sheet = "DeParaLandUse"
DePara.sub_sector <- DePara %>%
  filter(sector_landscape == 'sector_landscape') %>%
  mutate(`Variavel Land Use` = trimws(`Variavel Land Use`),
         sub_sector_cpi = trimws(sub_sector_cpi)) %>%
  distinct(`Variavel Land Use`, sub_sector_cpi) %>%
  deframe()  # cria um vetor nomeado: "valor_antigo" = "valor_novo"

#Substitui valores do sub_sector_cpi com base no Depara
df_atlas_calculus_dePara <- df_atlas_calculus_renamed %>%
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
df_atlas_calculus_dePara <- df_atlas_calculus_dePara %>%
  mutate(sector_cpi = recode(sub_sector_cpi, !!!DePara.sector))


### Inserir informações em solution_cpi com base em "Solution" do UniqueKeys com base na relational
# Atlas e SES é "Rural Insurence for Climate Resilience"
# DePara.solution <- DePara %>%
#   filter(sector_landscape == 'sector_landscape') %>%
#   mutate(sub_sector_cpi = trimws(sub_sector_cpi),
#          solution_cpi = trimws(solution_cpi)) %>%
#   distinct(sub_sector_cpi, solution_cpi) %>%
#   deframe()  # cria um vetor nomeado: "valor_antigo" = "valor_novo"


df_atlas_calculus_dePara <- df_atlas_calculus_dePara %>%
  mutate(solution_cpi = "Climate resiliency building rural insurance")



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
df_atlas_final <- df_atlas_calculus_dePara %>%
  mutate(keysector = trimws(sector_cpi),
         keysubsector = trimws(sub_sector_cpi),
         keysolution = trimws(solution_cpi)) %>%
  mutate(keysector = recode(keysector, !!!DePara.keysector),
         keysubsector = recode(keysubsector, !!!DePara.keysubsector),
         keysolution = recode(keysolution, !!!DePara.keysolution)) %>%
  mutate(sector_key_cpi = str_c(keysector, "_", keysubsector, "_", keysolution)) %>%
  select(- c(keysector, keysubsector, keysolution))




# Ve quais colunas ainda não existem para poder criar
dif_cols <- setdiff(landscape_cols, names(df_atlas_final))
#""ID_Landscape"       "country_origin_cpi" "region_origin_cpi"  "indigenous_cpi" 

# Só executa se houver colunas ausentes
if (length(dif_cols) > 0) {
  for (col in dif_cols) {
    df_atlas_final[[col]] <- NA
  }
}



df_atlas_final2 <- df_atlas_final %>%
  mutate(country_origin_cpi = "Brazil",
         region_origin_cpi = "Brazil",
         ID_Landscape = id_original) %>% 
  #bota na ordem de landscape
  select(Landscape_columns$`LANDSCAPE BRAZIL`)






## Export
saveRDS(df_atlas_final2, paste0(path_output, "database_atlas_landscape_", ano_ini, "_", ano_fim, ".rds"))
write.csv2(df_atlas_final2, paste0(path_output, "database_atlas_landscape_", ano_ini, "_", ano_fim, ".csv"), fileEncoding = "Latin1")



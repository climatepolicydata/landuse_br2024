library(tidyverse)
library(readxl)
library(stringi)
library(DescTools)
library(xlsx)

df_bndes_filter <- readRDS("df_bndes_n_aut_filter_reviewed.rds")

source_bndes_n_aut <- read_excel("06_bndes_naut_relational_tables.xlsx", sheet = "source_landscape")%>% 
  dplyr::rename(fonte_de_recurso_desembolsos  = `source_of finance_original` )

channel_bndes_n_aut <- read_excel("06_bndes_naut_relational_tables.xlsx", sheet = "channel_landscape")

instrument_bndes_n_aut <- read_excel("06_bndes_naut_relational_tables.xlsx", sheet = "instrument_landscape") %>% distinct()

sector_bndes_n_aut <- read_excel("06_bndes_naut_relational_tables.xlsx", sheet = "sector_landscape") 

beneficiary_bndes_n_aut <- read_excel("06_bndes_naut_relational_tables.xlsx", sheet = "beneficiary_landscape")

climate_bndes_n_aut <- read_excel("06_bndes_naut_relational_tables.xlsx", sheet = "climate_select") %>% 
  mutate_if(is.character, ~ stri_trans_general(., "Latin-ASCII")) %>% 
  mutate(climate_original = paste0(numero_do_contrato,instrumento_financeiro)) %>% 
  mutate_if(is.character, tolower) %>% filter(!numero_do_contrato == "15208221")

# Inicio da transformacao para landscape:
df_bndes_filter_landscape <- df_bndes_filter %>%
    mutate(
        id_original = numero_do_contrato,
        data_source = "bndes_n_aut",
        year = ano,
        project_name = str_c(cliente,cnpj, sep =";"),
        project_description = descricao_do_projeto,
        source_original = fonte_de_recurso_desembolsos,
        source_finance_landscape = case_when(
            source_original == "recursos livres - proprios" |
            source_original == "recursos livres - fat" |
            source_original == "-" ~ "Federal and state governments", .default = 'Sem_Classificacao'
        ),
        origin_domestic_international = case_when(
            source_finance_landscape == "Federal and state governments" ~ "National",.default = 'Sem_Classificacao'

        ),
        origin_private_public = case_when(
            origin_domestic_international == "National" ~ "Public",.default = 'Sem_Classificacao'
        ),
        value_original_currency = valor_contratado_reais,
        original_currency = "BRL",
        channel_original = str_c(forma_de_apoio,instituicao_financeira_credenciada,sep = "_"),
        channel_landscape = if_else(forma_de_apoio=="direta","BNDES", "Financial Institution"),
        instrument_original = instrumento_financeiro
    )%>% left_join(instrument_bndes_n_aut%>%select(instrument_original,'instrument_landscape'),by="instrument_original") %>%
    mutate(
        sector_original = subsetor_cnae_nome) %>%left_join(sector_bndes_n_aut%>%select(sector_original,sector_landscape), by = "sector_original") %>%
        left_join(sector_bndes_n_aut%>%select(sector_original,subsector_original),by="sector_original") %>%
        mutate(
            rio_marker = "-",
            beneficiary_original = str_c(natureza_do_cliente,porte_do_cliente,cliente,sep= "_")) %>%
            left_join(beneficiary_bndes_n_aut%>%select(beneficiary_original,beneficiary_landscape,beneficiary_public_private), by="beneficiary_original") %>%
            mutate(localization_original=uf,
                    region = "-",
                    municipality = municipio)
                    
#Filtro para Sector LandScape
biogas <- read_csv2("Frequencia_palavras_n_grama_biogas.csv")
biofuel<- read_csv2("Frequencia_palavras_n_grama_biofuel.csv")
bioprod <- read_csv2("Frequencia_palavras_n_grama_bio_prod.csv")
sugarcane <- read_csv2("Frequencia_palavras_n_grama_bio_sugarcane.csv")
power <- read_csv2("Frequencia_palavras_n_grama_power.csv")
reforestation <- read_csv2("Frequencia_palavras_n_grama_reforestation.csv")
pulp <- read_csv2("Frequencia_palavras_n_grama_pulp.csv")

#Biogas production using sewage
df_bndes_filter_landscape_biogas <- df_bndes_filter_landscape %>%
                    filter(project_description %like% paste(biogas$value,collapse = "|")|
                                     sector_original  %like% paste(biogas$value,collapse = "|")|
                                     subsector_original %like% paste(biogas$value,collapse = "|") )
df_bndes_filter_landscape_biogas$activity_landscape <- "Biogas production using sewage"

df_bndes_filter_landscape_no_biogas <- df_bndes_filter_landscape%>%filter(!id_original %in% df_bndes_filter_landscape_biogas$id_original)

#"Production of biofuels, including biodiesel and bioethanol"
df_bndes_filter_landscape_biofuel <- df_bndes_filter_landscape_no_biogas %>%
                    filter(project_description %like% paste(biofuel$value,collapse = "|")|
                                     sector_original  %like% paste(biofuel$value,collapse = "|")|
                                     subsector_original %like% paste(biogas$value,collapse = "|"))
df_bndes_filter_landscape_biofuel$activity_landscape = "Production of biofuels, including biodiesel and bioethanol"

df_bndes_filter_landscape_no_biogas_no_biofuel <- df_bndes_filter_landscape_no_biogas%>%filter(!id_original %in% df_bndes_filter_landscape_biofuel$id_original)

#"Biological products production"
df_bndes_filter_landscape_bioprod <-df_bndes_filter_landscape_no_biogas_no_biofuel %>%
                    filter(project_description %like% paste(bioprod$value,collapse = "|")|
                                     sector_original  %like% paste(bioprod$value,collapse = "|")|
                                     subsector_original %like% paste(bioprod$value,collapse = "|"))
df_bndes_filter_landscape_bioprod$activity_landscape = "Biological products production"

df_bndes_filter_landscape_no_biogas_no_biofuel_no_bioprod <- df_bndes_filter_landscape_no_biogas_no_biofuel%>%filter(!id_original %in% df_bndes_filter_landscape_bioprod$id_original)

# "Sugarcane-energy related activities"
df_bndes_filter_landscape_sugarcane <- df_bndes_filter_landscape_no_biogas_no_biofuel_no_bioprod%>%
                    filter(project_description %like% paste(sugarcane$value,collapse = "|")|
                                     sector_original  %like% paste(sugarcane$value,collapse = "|")|
                                     subsector_original %like% paste(sugarcane$value,collapse = "|"))
df_bndes_filter_landscape_sugarcane$activity_landscape = "Sugarcane-energy related activities"

df_bndes_filter_landscape_no_biogas_no_biofuel_no_bioprod_no_sugarcane <- df_bndes_filter_landscape_no_biogas_no_biofuel_no_bioprod%>%filter(!id_original %in% df_bndes_filter_landscape_sugarcane$id_original)



# "Power cogeneration from sugarcane bagasse"
df_bndes_filter_landscape_sugarcaneBagasse <-df_bndes_filter_landscape_no_biogas_no_biofuel_no_bioprod_no_sugarcane%>%
                    filter(project_description %like% paste(power$value,collapse = "|")|
                                     sector_original  %like% paste(power$value,collapse = "|")|
                                     subsector_original %like% paste(power$value,collapse = "|"))
df_bndes_filter_landscape_sugarcaneBagasse$activity_landscape = "Power cogeneration from sugarcane bagasse"
df_bndes_filter_landscape_no_biogas_no_biofuel_no_bioprod_no_sugarcane_no_sugarcaneBagasse <- df_bndes_filter_landscape_no_biogas_no_biofuel_no_bioprod_no_sugarcane%>%filter(!id_original %in% df_bndes_filter_landscape_sugarcaneBagasse$id_original)

# "Reforestation on previously forested land"
df_bndes_filter_landscape_reforestation <- df_bndes_filter_landscape_no_biogas_no_biofuel_no_bioprod_no_sugarcane_no_sugarcaneBagasse%>%
                    filter(project_description %like% paste(reforestation$value,collapse = "|")|
                                     sector_original  %like% paste(reforestation$value,collapse = "|")|
                                     subsector_original %like% paste(reforestation$value,collapse = "|"))
df_bndes_filter_landscape_reforestation$activity_landscape = "Reforestation on previously forested land"

df_bndes_filter_landscape_no_biogas_no_biofuel_no_bioprod_no_sugarcane_no_sugarcaneBagasse_no_reforestation <- df_bndes_filter_landscape_no_biogas_no_biofuel_no_bioprod_no_sugarcane_no_sugarcaneBagasse%>%filter(!id_original %in% df_bndes_filter_landscape_reforestation$id_original)

# PulpSector
df_bndes_filter_landscape_reforestation_pulp_sector <- df_bndes_filter_landscape_no_biogas_no_biofuel_no_bioprod_no_sugarcane_no_sugarcaneBagasse_no_reforestation %>% 
                    filter( project_description %like% paste(pulp$value,collapse = "|")|
                                     sector_original  %like% paste(pulp$value,collapse = "|")|
                                     subsector_original %like% paste(pulp$value,collapse = "|"))
df_bndes_filter_landscape_reforestation_pulp_sector$activity_landscape = "PulpSector"


# Sem Classificacao
df_bndes_filter_landscape_no_biogas_no_biofuel_no_bioprod_no_sugarcane_no_sugarcaneBagasse_no_reforestation_no_pulp <- df_bndes_filter_landscape_no_biogas_no_biofuel_no_bioprod_no_sugarcane_no_sugarcaneBagasse_no_reforestation%>%filter(!id_original %in% df_bndes_filter_landscape_reforestation_pulp_sector$id_original)
df_bndes_filter_landscape_no_biogas_no_biofuel_no_bioprod_no_sugarcane_no_sugarcaneBagasse_no_reforestation_no_pulp$activity_landscape = "Sem Classificacao"

# Dando bind para obter o dataframe em conjunto:
df_bndes_filter_landscape_update <- bind_rows(df_bndes_filter_landscape_biogas,df_bndes_filter_landscape_biofuel,df_bndes_filter_landscape_bioprod,
df_bndes_filter_landscape_sugarcane,df_bndes_filter_landscape_sugarcaneBagasse,df_bndes_filter_landscape_reforestation,
df_bndes_filter_landscape_reforestation_pulp_sector,df_bndes_filter_landscape_no_biogas_no_biofuel_no_bioprod_no_sugarcane_no_sugarcaneBagasse_no_reforestation_no_pulp)




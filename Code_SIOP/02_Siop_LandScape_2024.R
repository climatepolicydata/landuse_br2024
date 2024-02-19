library(tidyverse)
library(stringi)
library(readxl)
source("C:/Users/eduar/Dropbox (CPI)/EduardoMinsky/PARAMIM/landuse_br2024/AuxFolder/Dictionary_Sectors.R")
siop_tratado <- read_rds('./brlanduse_landscape2024_dados/SIOP/Siop_Tratado_2021_2023.rds')

#Tabelas relacionais
source_landscape <- read_excel("./brlanduse_landscape2024_dados/SIOP/12_siop_relational_tables.xlsx", sheet="source_landscape")
source_landscape <- source_landscape%>%rename(source_original =`source_of finance_original` )
source_landscape <- source_landscape%>%mutate(source_original = str_trim(str_to_lower(stri_trans_general(source_original,"Latin-ASCII"))))

channel_landscapeV2 <- read_excel("./brlanduse_landscape2024_dados/SIOP/12_siop_relational_tables.xlsx", sheet="channel_landscapeV2") %>% select(channel_original,channel_landscape)%>%unique

instrument_landscape <- read_excel("./brlanduse_landscape2024_dados/SIOP/12_siop_relational_tables.xlsx", sheet="instrument_landscape")

######################################################################
siop_landscape <- siop_tratado %>% mutate(
    id_original = "-",
    data_source = 'siop_painel',
    year = Ano,
    project_name = acao,
    project_description = plano_orc,
    source_original = fonte_recursos)%>%
    left_join(source_landscape%>%select(c(source_original,source_of_finance_landscape,domestic_internacional,source_private_public)), by ="source_original") %>% #Fazendo o join para selecionar as fontes landscape 
     filter((!is.na(source_of_finance_landscape)) | (!is.na(domestic_internacional)) | (!is.na(source_private_public))) %>% #Filtrando para reter registros que não estao presentes no source origina da tabela relacional
        mutate(original_currency = "BRL",
           channel_original = str_c(modalidade,und_orc,sep=";")
           ) %>% left_join(channel_landscapeV2, by = "channel_original")%>% #Fazendo o join para selecionar os canais landscape
            filter(!is.na(channel_landscape))%>%#Filtrando apenas os canais landscape que aparecem
            mutate(sector_original = str_c(modalidade,und_orc,sep = ";"), subsector_original = programa) %>% 
            filter(Pago != 0) # Filtrando apenas contratos que pagaram algo
              
# Aplicando os filtros
siop_landscape <- siop_landscape %>% mutate(Coluna_search = str_c(project_name,project_description,sector_original,subsector_original,channel_original,source_original,sep = ";"))

bioenergia_siop <- bioenergy_search_pattern_SIOP(data_frame_SIOP = siop_landscape,Coluna_search = Coluna_search)
#bioenergia_siop%>%select(project_name,project_description,sector_original,subsector_original,channel_original,source_original)%>% view

crop_siop <- crop_search_pattern_SIOP(data_frame_SIOP = siop_landscape,Coluna_search = Coluna_search)
crop_siop%>%select(project_name,project_description,sector_original,subsector_original,channel_original,source_original,Coluna_search)%>% unique %>% view

multisector_siop <- multisector_search_pattern_SIOP(data_frame_SIOP=siop_landscape ,Coluna_search = Coluna_search) # Ha alguns investimentos que tambem estao em Crop!!!!
multisector_siop%>%select(project_name,project_description,sector_original,subsector_original,channel_original,source_original,Coluna_search)%>% unique %>% view

forest_siop <- forest_search_pattern_SIOP(data_frame_SIOP = siop_landscape,Coluna_search =Coluna_search )
forest_siop%>%select(project_name,project_description,sector_original,subsector_original,channel_original,source_original,Coluna_search)%>% unique %>% view

teste <- rbind(bioenergia_siop,crop_siop,multisector_siop,forest_siop)



siop_landscape%>%filter(!Coluna_search %in% teste$Coluna_search)%>%select(project_name,project_description,sector_original,subsector_original,channel_original,source_original,Coluna_search)%>%unique%>%view

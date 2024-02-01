library(tidyverse)
library(stringi)
library(readxl)
getwd()
siop_tratado <- read_rds('./SIOP/Siop_Tratado_2021_2023.rds')

#Tabelas relacionais
source_landscape <- read_excel("./SIOP/12_siop_relational_tables.xlsx", sheet="source_landscape")
source_landscape <- source_landscape%>%rename(source_original =`source_of finance_original` )
source_landscape <- source_landscape%>%mutate(source_original = str_trim(str_to_lower(stri_trans_general(source_original,"Latin-ASCII"))))

channel_landscape <- read_excel("./SIOP/12_siop_relational_tables.xlsx", sheet="channel_landscape")

instrument_landscape <- read_excel("./SIOP/12_siop_relational_tables.xlsx", sheet="instrument_landscape")

######################################################################
siop_tratado %>% mutate(
    id_original = "-",
    data_source = 'siop_painel',
    year = Ano,
    project_name = acao,
    project_description = plano_orc,
    source_original = fonte_recursos)%>%left_join(source_landscape%>%select(c(source_original,source_of_finance_landscape,domestic_internacional,source_private_public)), by ="source_original")%>%
    mutate(original_currency = "BRL",
           channel_original = str_c(modalidade,und_orc,sep=";")
           )%>%left_join(channel_landscape%>%select(-channel_original),by="modalidade") %>%
           mutate(instrument_original = grupo_de_despesa)%>%left_join(instrument_landscape,by="instrument_original")%>%
           mutate(sector_original = str_c(funcao,subfuncao,sep = ";"),
           sector_landscape = case_when(
            sector_original == "organizacao agraria;assistencia comunitaria "|
            sector_original == "encargos especiais;outros encargos especiais"|
            sector_original == "administracao;planejamento e orcamento"|
            sector_original == "agricultura;extensao rural"|
            sector_original == "agricultura;meteorologia"|
            sector_original == "agricultura;abastecimento"|
            sector_original == "agricultura;defesa agropecuaria"|
            sector_original == "agricultura;desenvolvimento tecnologico e engenharia"|
            sector_original == "agricultura;difusao do conhecimento cientifico e tecnologico"|
            sector_original == "agricultura;normatizacao e fiscalizacao"|
            sector_original == "agricultura;preservacao e conservacao ambiental"|
            sector_original == "agricultura;promocao da producao agropecuaria"|
            sector_original ==  "gestao ambiental;preservacao e conservacao ambiental"|
            sector_original ==  "orcamento publico;agricultura"|
            sector_original ==  "organizacao agraria;extensao rural"|
            sector_original ==  "organizacao agraria;ordenamento territorial" ~ "Agriculture",

            acao == "desenvolvimento da agroenergia"|
            sector_original == "ciencia e tecnologia;desenvolvimento cientifico"|
            sector_original ==  "ciencia e tecnologia;desenvolvimento tecnologico e engenharia"|
            sector_original ==  "direitos da cidadania;assistencia aos povos indigenas"|
            sector_original ==  "energia;biocombustiveis"|
            acao ==  "apoio a criacao, gestao e implementacao das unidades de conservacao federais"|
            sector_original ==  "gestao ambiental;administracai geral" ~ "Bioenergy and Fuels",

            sector_original == "agricultura;irrigacao" |
            acao == "regularizacao fundiaria e assistencia tecnica e extensao rural na amazonia legal e sua regiao fronteirica"|
            
           ))



siop_tratado%>%mutate(sector_original = str_c(funcao,subfuncao,sep = ";"))%>%select(sector_original)%>%filter(sector_original=='agricultura;extensao rural')
siop_tratado%>%nrow
siop_tratado%>%view


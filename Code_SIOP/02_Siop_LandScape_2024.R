library(tidyverse)
library(stringi)
library(readxl)
library(xlsx)
source("C:/Users/napcc/Dropbox (CPI)/EduardoMinsky/PARAMIM/landuse_br2024/AuxFolder/Dictionary_Sectors.R")
siop_tratado <- read_rds('./brlanduse_landscape2024_dados/SIOP/Siop_Tratado_2021_2023.rds')

#Tabelas relacionais
source_landscape <- read_excel("./brlanduse_landscape2024_dados/SIOP/12_siop_relational_tables.xlsx", sheet="source_landscape")
source_landscape <- source_landscape%>%rename(source_original =`source_of finance_original` )
source_landscape <- source_landscape%>%mutate(source_original = str_trim(str_to_lower(stri_trans_general(source_original,"Latin-ASCII"))))

channel_landscapeV2 <- read_excel("./brlanduse_landscape2024_dados/SIOP/12_siop_relational_tables.xlsx", sheet="channel_landscapeV2") %>% select(channel_original,channel_landscape)%>%unique

instrument_landscape <- read_excel("./brlanduse_landscape2024_dados/SIOP/12_siop_relational_tables.xlsx", sheet="instrument_landscape") 
climate_use <- read_excel("./brlanduse_landscape2024_dados/SIOP/12_siop_relational_tables.xlsx", sheet="climate_use") %>%select(plano_orc,acao,sector_original,sector_landscape,subsector_original,activity_landscape,subactivity_landscape,climate_component,rio_marker,beneficiary_landscape)
climate_use <- climate_use%>%mutate(
  plano_orc = str_trim(str_remove(str_remove(str_remove(str_remove(str_to_lower(stri_trans_general(plano_orc, "Latin-ASCII")),"^[[:alnum:]]{4}"),"'"),"^[[:alnum:]]{4}"),"-")),
  acao = str_trim(str_remove(str_to_lower(str_remove(stri_trans_general(acao, "Latin-ASCII"),"^[[:alnum:]]{4}")),"-")),
  sector_original = str_trim(str_to_lower(str_remove(str_remove(str_remove(stri_trans_general(sector_original,"Latin-ASCII"),"21"),"28"),"-"))),
  subsector_original = str_trim(str_remove(str_remove(str_remove(str_to_lower(stri_trans_general(subsector_original, "Latin-ASCII")),"244"),"846"),"-"))

  )


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
            filter(Pago != 0) # Filtrando apenas contratos que pagaram algum valor

# Aplicando o left join para filtro climatico que existe no excel do relational base
# Criando as colunas para esse join
siop_landscape <- siop_landscape %>% mutate(left_join_key = str_c(project_description,project_name,funcao,subfuncao,sep = ";"))
climate_use <- climate_use %>% mutate(left_join_key = str_c(plano_orc,acao,sector_original,subsector_original,sep = ";"))
climate_use <- climate_use%>%distinct(left_join_key,.keep_all = TRUE)
siop_landscape_climate_use <- siop_landscape %>% inner_join(climate_use %>% select(left_join_key,sector_landscape,activity_landscape,subactivity_landscape,climate_component,rio_marker,beneficiary_landscape),by = "left_join_key") 

# Filtrando os investimentos que nao tiveram match

siop_landscape <- siop_landscape %>% anti_join(siop_landscape_climate_use, by = "left_join_key")


# Aplicando os filtros para sectorlandscape
siop_landscape <- siop_landscape %>% mutate(Coluna_search = str_c(project_name,project_description,sector_original,subsector_original,channel_original,source_original,sep = ";"))

bioenergia_siop <- bioenergy_search_pattern_SIOP(data_frame_SIOP = siop_landscape,Coluna_search = Coluna_search)
bioenergia_siop%>%select(project_name,project_description,sector_original,subsector_original,channel_original,source_original)%>% view
bioenergia_siop_filtrado <- bioenergy_out_SIOP(data_frame_SIOP_bioenergia = bioenergia_siop,Coluna_search = Coluna_search)
bioenergia_siop_filtrado$sector_landscape = "Bioenergy and fuels"


crop_siop <- crop_search_pattern_SIOP(data_frame_SIOP = siop_landscape,Coluna_search = Coluna_search)
crop_siop%>%select(project_name,project_description,sector_original,subsector_original,channel_original,source_original,Coluna_search)%>% unique %>% view
crop_siop_filtrado<- crop_out_SIOP(data_frame_SIOP_crop = crop_siop,Coluna_search = Coluna_search)
crop_siop_filtrado$sector_landscape = "Crop"

multisector_siop <- multisector_search_pattern_SIOP(data_frame_SIOP=siop_landscape ,Coluna_search = Coluna_search) 
multisector_siop%>%select(project_name,project_description,sector_original,subsector_original,channel_original,source_original,Coluna_search)%>% unique %>% view
multisector_siop_filtrado <- multisector_out_SIOP (data_frame_SIOP_multisector = multisector_siop,Coluna_search = Coluna_search)
multisector_siop_filtrado$sector_landscape = "Multisector"

forest_siop <- forest_search_pattern_SIOP(data_frame_SIOP = siop_landscape,Coluna_search =Coluna_search )
forest_siop%>%select(project_name,project_description,sector_original,subsector_original,channel_original,source_original,Coluna_search)%>% unique %>% view
forest_siop_filtrado <- forest_out_SIOP(data_frame_SIOP_forest = forest_siop,Coluna_search = Coluna_search)
forest_siop_filtrado%>%select(project_name,project_description,sector_original,subsector_original,channel_original,source_original,Coluna_search)%>% unique %>% view
forest_siop_filtrado$sector_landscape = "Forest"

siop_sectorlandscape <- rbind(bioenergia_siop_filtrado,crop_siop_filtrado,multisector_siop_filtrado,forest_siop_filtrado)

siop_landscape %>% filter(!Coluna_search %in% siop_sectorlandscape$Coluna_search)%>%select(project_name,project_description,sector_original,subsector_original,channel_original,source_original,Coluna_search)%>% unique %>% view


# Fazendo a classificacao climática:
# Vamos começar pelo Forest_siop
# Sabemos que pagamento de folha ou qualquer outro estimulo financeiro para funcionarios do ICMBIO, FUNAI IBAMA e SFB é para ser classificado como 
# Uso climatico: Mitigacao
siop_mitigacao_folha_pagamento <- siop_sectorlandscape %>% filter(
  (grepl("\\bfunai\\b", x = Coluna_search , ignore.case = TRUE)) | grepl("\\bibama\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bchico\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bsfb", x = Coluna_search , ignore.case = TRUE) | grepl("\\bServico Florestal Brasileiro\\b", x = Coluna_search , ignore.case = TRUE)) %>%
          filter(
            (grepl("\\baposentadorias e pensoes civis da uniao\\b", x = Coluna_search , ignore.case = TRUE)) |
            (grepl("\\bbeneficios obrigatorios aos servidores civis, empregados, militares e seus dependentes\\b", x = Coluna_search , ignore.case = TRUE)) |
            (grepl("\\bajuda de custo para moradia ou auxilio-moradia a agentes publicos\\b", x = Coluna_search , ignore.case = TRUE)) |
            (grepl("\\bassistencia medica e odontologica aos servidores civis, empregados, militares e seus dependentes\\b", x = Coluna_search , ignore.case = TRUE)) |
            (grepl("\\baposentadorias e pensoes civis da uniao\\b", x = Coluna_search , ignore.case = TRUE)) |
            (grepl("\\bcontribuicao da uniao, de suas autarquias e fundacoes para o custeio do regime de previdencia dos servidores publicos federais\\b", x = Coluna_search , ignore.case = TRUE))
          )
  
siop_mitigacao_folha_pagamento <- siop_mitigacao_folha_pagamento %>% mutate(activity_landscape = "Folha de pagamento com servidores de órgãos governamentais diretamente ligados às atividades que permitirão que o Brasil alcance seus compromissos climáticos de redução de emissões de GEE",
subactivity_landscape = if_else(grepl("\\bibama\\b", x = Coluna_search , ignore.case = TRUE), true= "Ibama", false = if_else(grepl("\\bchico\\b", x = Coluna_search , ignore.case = TRUE), true = "ICMBIO",false = if_else(grepl("\\bfunai\\b", x = Coluna_search , ignore.case = TRUE), true= "Funai", false = if_else(grepl("\\bsfb", x = Coluna_search , ignore.case = TRUE),true = "SFB",false="SemClass")))),
climate_component = "Mitigação"
) 

# Gestao e manutenção dos Orgaos do FUNAI, IBAMA,ICMBIO INPA MMA e SFB são uso climatico de mitigacao e adaptacao

siop_mitigacaoAdaptacao_planejamentocapacitacao <- siop_sectorlandscape %>% anti_join(siop_mitigacao_folha_pagamento, by="Coluna_search") %>% filter(
  (grepl("\\bfunai\\b", x = Coluna_search , ignore.case = TRUE)) | grepl("\\bibama\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bchico\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bsfb", x = Coluna_search , ignore.case = TRUE) | grepl("\\bServico Florestal Brasileiro\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\binpa\\b", x = Coluna_search , ignore.case = TRUE) | 
  (grepl("\\bministerio do meio\\b", x = Coluna_search , ignore.case = TRUE))) %>% filter(
    (grepl("\\badministracao da unidade\\b", x = Coluna_search , ignore.case = TRUE))
  ) %>% mutate(climate_component = "Mitigação e Adaptação",
               activity_landscape = "Gestão e planejamento de políticas,capacitação e orientação",
                subactivity_landscape = if_else(grepl("\\bfunai\\b", x = Coluna_search , ignore.case = TRUE), true="Gestão e manutenção de estrutura administrativa:Funai",
                false = if_else(grepl("\\bibama\\b", x = Coluna_search , ignore.case = TRUE), true = "Gestão e manutenção de estrutura administrativa:Ibama",
                false = if_else(grepl("\\bchico\\b", x = Coluna_search , ignore.case = TRUE),true = "Gestão e manutenção de estrutura administrativa: ICMBIO",
                false = if_else(grepl("\\binpa\\b", x = Coluna_search , ignore.case = TRUE), true = "Gestão e manutenção de estrutura administrativa: INPA" , 
                false = if_else(grepl("\\bministerio do meio ambiente e mudanca do clima\\b", x = Coluna_search , ignore.case = TRUE), true = "Gestão e manutenção de estrutura administrativa: MMA",
                #false = if_else(grepl("\\bsfb\\b", x = Coluna_search , ignore.case = TRUE), true = "Gestão e manutenção de estrutura administrativa: SFB"),
                false = "Sem Classificacao")))))) 
                
                
  
filtro_1 <- rbind(siop_mitigacaoAdaptacao_planejamentocapacitacao,siop_mitigacao_folha_pagamento)
siop_sectorlandscape <- siop_sectorlandscape %>% anti_join(filtro_1 , by="Coluna_search") 

# Regularização ambiental e fundiária e de ordenamento territorial
siop_mitigacao_regularizacaoAmbiental <- siop_sectorlandscape %>% filter(
  (grepl("\\breforma agraria e regularizacao fundiaria\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\bregularizacao ambiental e fundiaria de projetos publicos de irrigacao\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\bregularizacao, demarcacao e fiscalizacao de terras indigenas e protecao dos povos indigenas isolados\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\bgovernanca fundiaria e gerenciamento do cadastro rural\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\borganizacao da estrutura fundiaria\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\bapoio a criacao, gestao e implementacao das unidades de conservacao federais\\b", x = Coluna_search , ignore.case = TRUE))  
) %>% mutate(activity_landscape = "Regularização ambiental e fundiária e de ordenamento territorial",
            climate_component = "Mitigação",
            subactivity_landscape =if_else(grepl("\\bunidades de conservacao federais\\b", x = Coluna_search , ignore.case = TRUE),true = "Criação, gestão, fiscalização e implementação das Unidades de Conservação (UCs). Sistema Nacional de Unidades
de Conservação (SNUC)",
                      false = if_else((grepl("\\bgeorreferenciamento da malha\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bgerenciamento dos imoveis rurais\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bfiscalizacao do cadastro rural\\b", x = Coluna_search , ignore.case = TRUE)),
                      true = "Governança fundiária e gerenciamento do Cadastro Ambiental Rural (CAR). Regularização ambiental dos imóveis rurais nas unidades da federação. Apoio a órgãos subnacionais para implementação do CAR e regularização ambiental.",
                      false = if_else((grepl("\\bcapacitacao de colaboradores em regularizacao fundiaria\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bidentificacao, arrecadacao e destinacao de areas publicas\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bregularizacao ambiental e fundiaria de projetos publicos de irrigacao\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\breforma agraria e regularizacao fundiaria - despesas diversas\\b", x = Coluna_search , ignore.case = TRUE)),
                      true = "Modernização da gestão fundiária e regularização fundiária de glebas públicas e assentamentos, estaduais e federais.",
                      false = if_else((grepl("\\bfiscalizacao de terras indigenas\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bbarreiras sanitarias\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bcoronavirus\\b", x = Coluna_search , ignore.case = TRUE)), true = "Regularização, demarcação e fiscalização de terras indígenas e proteção dos povos indígenas isolados. Reconhecimento e indenização de territórios quilombolas. Gestão ambiental e etnodesenvolvimento.",
                      false = "Sem Classificacao"))))
       )
filtro_2 <- rbind(siop_mitigacao_regularizacaoAmbiental,filtro_1)
siop_sectorlandscape <- siop_sectorlandscape %>% anti_join(filtro_2 , by="Coluna_search")

# Ações de prevenção, controle do desmatamento e de incêndios
siop_sectorlandscape %>% filter(
  (grepl("\\bfiscalizacao ambiental e prevencao e combate a incendios florestais", x = Coluna_search , ignore.case = TRUE)) | 
  (grepl("\\bformulacao e implementacao de estrategias para promover a conservacao e recuperacao e o uso sustentavel da biodiversidade, da vegetacao nativa\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\bmanutencao do sistema de protecao da amazonia - sipam\\b", x = Coluna_search , ignore.case = TRUE)) 
)%>% mutate(activity_landscape = "Ações de prevenção, controle do desmatamento e de incêndios",
            subactivity_landscape = if_else((grepl)))
# fiscalizacao ambiental e prevencao e combate a incendios florestais
#formulacao e implementacao de estrategias para promover a conservacao, a recuperacao e o uso sustentavel da biodiversidade, da vegetacao nativa e do patrimonio genetico
#manutencao do sistema de protecao da amazonia - sipam


#%>%select(project_name,project_description,sector_original,subsector_original,channel_original,source_original,sector_landscape,Coluna_search)%>% unique %>%view
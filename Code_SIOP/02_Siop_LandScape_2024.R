library(tidyverse)
library(stringi)
library(readxl)
library(xlsx)
source("C:/Users/eduar/Dropbox (CPI)/EduardoMinsky/PARAMIM/landuse_br2024/AuxFolder/Dictionary_Sectors.R")
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

sem_filtro_dicionario <- siop_landscape %>% filter(!Coluna_search %in% siop_sectorlandscape$Coluna_search)

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
subactivity_landscape = if_else(grepl("\\bibama\\b", x = Coluna_search , ignore.case = TRUE), true= "Ibama", false = if_else(grepl("\\bchico\\b", x = Coluna_search , ignore.case = TRUE), true = "ICMBIO",false = if_else(grepl("\\bfunai\\b", x = Coluna_search , ignore.case = TRUE), true= "Funai", false = if_else(grepl("\\bsfb", x = Coluna_search , ignore.case = TRUE),true = "SFB",false="SemClasse")))),
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

# Juntando os 2 primeiros filtros 
filtro_1 <- rbind(siop_mitigacaoAdaptacao_planejamentocapacitacao,siop_mitigacao_folha_pagamento)
siop_sectorlandscape <- siop_sectorlandscape %>% anti_join(filtro_1 , by="Coluna_search") 

#Gestão e planejamento de políticas, capacitação e orientação
gestao_planejamento_politicas_capacitacao_orientacao <- siop_sectorlandscape %>% filter(
  (grepl("\\bpolitica nacional sobre mudanca do clima\\b", x = Coluna_search , ignore.case = TRUE)) | (grepl("\\bpolitica nacional de recursos hidricos\\b", x = Coluna_search , ignore.case = TRUE)) | grepl("\\binpa\\b", x = Coluna_search , ignore.case = TRUE) | (grepl("\\binsa\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\bantartica\\b", x = Coluna_search , ignore.case = TRUE)) | (grepl("\\bamazonia azul\\b", x = Coluna_search , ignore.case = TRUE)) | (grepl("\\bapoio ao desenvolvimento de agricultura de baixa emissao de carbono e sistemas sustentaveis de producao\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\bpro-organico\\b", x = Coluna_search , ignore.case = TRUE)) | (grepl("\\bdesenvolvimento rural sustentavel do semiarido brasileiro\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\bpromocao e fortalecimento da comercializacao e acesso aos mercados\\b", x = Coluna_search , ignore.case = TRUE)) | (grepl("\\bapoio a agricultura digital e de precisao\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\binclusao produtiva sustentavel\\b", x = Coluna_search , ignore.case = TRUE))
  
)%>% mutate(activity_landscape="Gestão e planejamento de políticas, capacitação e orientação",
          subactivity_landscape = if_else((grepl("\\bpolitica nacional sobre mudanca do clima\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bpolitica nacional de recursos hidricos\\b", x = Coluna_search , ignore.case = TRUE)),true = "Políticas, leis, regulamentações, instrumentos econômicos, seminários e reuniões para medidas relacionadas a conservação, energia, meio ambiente e uso da água, como a Política Nacional sobre Mudança do Clima (PNMC) entre outras.",
          false = if_else((grepl("\\binpa\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\binsa\\b", x = Coluna_search , ignore.case = TRUE)),true = "Ciência, tecnologia e inovação no Instituto Nacional da Mata Atlântica (INMA), Instituto Nacional de Pesquisas da Amazônia (Inpa), Instituto Nacional de Pesquisas Espaciais (Inpe), Instituto Nacional do Semiárido (INSA).",
          false = if_else((grepl("\\bantartica\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bamazonia azul\\b", x = Coluna_search , ignore.case = TRUE)),true = "Estudos e projetos de pesquisa e desenvolvimento relacionados à mudança do clima e ao monitoramento oceanográfico e climatológico da Amazônia Azul. Apoio logístico à pesquisa científica na Antártica.",
          false = if_else((grepl("\\bapoio ao desenvolvimento de agricultura de baixa emissao de carbono e sistemas sustentaveis de producao\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bpro-organico\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bdesenvolvimento rural sustentavel do semiarido brasileiro\\b", x = Coluna_search , ignore.case = TRUE)),true = "Agricultura de baixa emissão de carbono e sistemas sustentáveis de produção, cadeias produtivas regionais, controle da agricultura orgânica (Pró-Orgânico), desenvolvimento sustentável das cadeias produtivas agrícolas e seus territórios e combate à pobreza rural no semiárido do Nordeste.",
          false = if_else((grepl("\\bpromocao e fortalecimento da comercializacao e acesso aos mercados\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\binclusao produtiva sustentavel\\b", x = Coluna_search , ignore.case = TRUE)),true = "Conservação e uso sustentável de recursos genéticos para agricultura e alimentação. Produção agroalimentar artesanal. IG de Produtos Agropecuários. Estruturação e consolidação de redes socioprodutivas da agricultura familiar.",
          false = if_else((grepl("\\bapoio a agricultura digital e de precisao\\b", x = Coluna_search , ignore.case = TRUE)),true = "Agricultura digital e de precisão.",
          false = "Sem Classificacao"))))))
          ) %>% mutate(climate_component = "Mitigação e Adaptação")

# Segundo Filtro
filtro_2 <- rbind(filtro_1,gestao_planejamento_politicas_capacitacao_orientacao)
siop_sectorlandscape <- siop_sectorlandscape %>% anti_join(filtro_2 , by="Coluna_search") 


# Regularização ambiental e fundiária e de ordenamento territorial
siop_mitigacao_regularizacaoAmbiental <- siop_sectorlandscape %>% filter(
  (grepl("\\breforma agraria e regularizacao fundiaria\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\bregularizacao ambiental e fundiaria de projetos publicos de irrigacao\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\bregularizacao, demarcacao e fiscalizacao de terras indigenas e protecao dos povos indigenas isolados\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\bgovernanca fundiaria e gerenciamento do cadastro rural\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\borganizacao da estrutura fundiaria\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\bapoio a criacao, gestao e implementacao das unidades de conservacao federais\\b", x = Coluna_search , ignore.case = TRUE)) | (grepl("\\bcadastro, recomposicao e producao florestal\\b",x = Coluna_search,ignore.case = TRUE)) | grepl("\\bconsolidacao do sistema nacional de unidades de conservacao da natureza\\b",x = Coluna_search,ignore.case = TRUE)  |
  (grepl("\\bapoio a projetos de desenvolvimento florestal sustentavel\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\bconsolidacao de assentamentos rurais\\b", x = Coluna_search , ignore.case = TRUE))
) %>% mutate(activity_landscape = "Regularização ambiental e fundiária e de ordenamento territorial",
            climate_component = "Mitigação",
            subactivity_landscape =if_else((grepl("\\bunidades de conservacao federais\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bconsolidacao do sistema nacional de unidades de conservacao da natureza\\b", x = Coluna_search , ignore.case = TRUE)),true = "Criação, gestão, fiscalização e implementação das Unidades de Conservação (UCs). Sistema Nacional de Unidades
de Conservação (SNUC)",
                      false = if_else((grepl("\\bgeorreferenciamento da malha\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bgerenciamento dos imoveis rurais\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bfiscalizacao do cadastro rural\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bgestao do sistema de cadastro ambiental rural\\b",x = Coluna_search,ignore.case = TRUE) | grepl("\\bpromocao da ampliacao da producao florestal\\b",x = Coluna_search,ignore.case = TRUE)),
                      true = "Governança fundiária e gerenciamento do Cadastro Ambiental Rural (CAR). Regularização ambiental dos imóveis rurais nas unidades da federação. Apoio a órgãos subnacionais para implementação do CAR e regularização ambiental.",
                      false = if_else((grepl("\\bcapacitacao de colaboradores em regularizacao fundiaria\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bidentificacao, arrecadacao e destinacao de areas publicas\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bregularizacao ambiental e fundiaria de projetos publicos de irrigacao\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\breforma agraria e regularizacao fundiaria - despesas diversas\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bregularizacao ambiental dos imoveis rurais nas unidades da federacao\\b", x = Coluna_search,ignore.case = TRUE) | grepl("\\bconsolidacao de assentamentos rurais\\b", x = Coluna_search,ignore.case = TRUE)),
                      true = "Modernização da gestão fundiária e regularização fundiária de glebas públicas e assentamentos, estaduais e federais.",
                      false = if_else((grepl("\\bfiscalizacao de terras indigenas\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bbarreiras sanitarias\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bcoronavirus\\b", x = Coluna_search , ignore.case = TRUE)), true = "Regularização, demarcação e fiscalização de terras indígenas e proteção dos povos indígenas isolados. Reconhecimento e indenização de territórios quilombolas. Gestão ambiental e etnodesenvolvimento.",
                      false = if_else((grepl("\\bapoio a projetos de desenvolvimento florestal sustentavel\\b", x = Coluna_search , ignore.case = TRUE)),true = "Adequação das propriedades rurais frente à legislação ambiental, inclusive recuperação da reserva legal, áreas de preservação permanente, recuperação de áreas degradadas e implantação e melhoramento de planos de manejo florestal sustentável.",false = "Sem Classificacao")))))
       ) 
# Fazendo o terceiro filtro
filtro_3 <- rbind(siop_mitigacao_regularizacaoAmbiental,filtro_2)
siop_sectorlandscape <- siop_sectorlandscape %>% anti_join(filtro_3 , by="Coluna_search")

# Ações de prevenção, controle do desmatamento e de incêndios
siop_mitigacao_prevencaoControleDesmatIncendio <- siop_sectorlandscape %>% filter(
  (grepl("\\bfiscalizacao ambiental e prevencao e combate a incendios florestais", x = Coluna_search , ignore.case = TRUE)) | 
  (grepl("\\bformulacao e implementacao de estrategias para promover a conservacao e recuperacao e o uso sustentavel da biodiversidade, da vegetacao nativa\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\bmanutencao do sistema de protecao da amazonia - sipam\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\bdesenvolvimento de politicas e acoes para a reducao do desmatamento ilegal e dos incendios florestais\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\bfortalecimento e aprimoramento das acoes relativas a avaliacao e ao controle da degradacao dos recursos naturais\\b", x = Coluna_search , ignore.case = TRUE))
)%>% mutate(activity_landscape = "Ações de prevenção, controle do desmatamento e de incêndios",
            subactivity_landscape = if_else((grepl("\\bfiscalizacao ambiental e prevencao e combate a incendios florestais\\b", x = Coluna_search , ignore.case = TRUE)),true = "Desenvolvimento e implementação de sistemas de monitoramento do desmatamento, além do controle, fiscalização, monitoramento ambiental e combate a infrações ambientais, inclusive por meio de sistemas de satélite.",
            false= if_else((grepl("\\bformulacao e implementacao de estrategias para promover a conservacao, a recuperacao e o uso sustentavel da biodiversidade, da vegetacao nativa e do patrimonio genetico\\b", x = Coluna_search , ignore.case = TRUE)),true = "Desenvolvimento e implementação de sistemas de monitoramento do desmatamento, além do controle, fiscalização, monitoramento ambiental e combate a infrações ambientais, inclusive por meio de sistemas de satélite.",
            false = if_else((grepl("\\bmanutencao do sistema de protecao da amazonia - sipam\\b", x = Coluna_search , ignore.case = TRUE)),true = "Sistema de Proteção da Amazônia (Sipam) e desenvolvimento, lançamento e operação de satélites, e a infraestrutura associada. Implantação do Sistema Amazônia (SAR).",
            false = if_else((grepl("\\bdesenvolvimento de politicas e acoes para a reducao do desmatamento ilegal e dos incendios florestais\\b", x = Coluna_search , ignore.case = TRUE)),true = "Desenvolvimento e implementação de sistemas de monitoramento do desmatamento, além do controle, fiscalização,monitoramento ambiental e combate a infrações ambientais, inclusive por meio de sistemas de satélite.",
            false = if_else((grepl("\\bfortalecimento e aprimoramento das acoes relativas a avaliacao e ao controle da degradacao dos recursos naturais\\b", x = Coluna_search , ignore.case = TRUE)),
            true = "Apoio orçamentário para autoridades federais ou subnacionais para as políticas de controle de desmatamento e gestão ambiental, além de outras atividades de assistência técnica, incluindo conscientização e capacitação",
            false = "Sem Classificação")))))
            ) %>% mutate(climate_component = "Mitigação")

# Fazendo o quarto filtro
filtro_4 <- rbind(siop_mitigacao_prevencaoControleDesmatIncendio,filtro_3)
siop_sectorlandscape <- siop_sectorlandscape %>% anti_join(filtro_4 , by="Coluna_search")

# Produção de defensivos agrícolas biológicos e orgânicos
# OU
# Desenvolvimento de zoneamentos e de matriz de riscos climáticos
siop_adaptacao_ProducaoDefensivosAgricolas <- siop_sectorlandscape %>% filter(
  (grepl("\\bfomento a participacao da agricultura familiar nas cadeias de energias renovaveis e bioinsumos\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\bcontrole fitossanitario da vassoura-de-bruxa e outras doencas e pragas do cultivo do cacau e monitoramento da moniliase para mitigar os efeitos e danos no cacaueiro\\b", x = Coluna_search , ignore.case = TRUE))
) %>% mutate(activity_landscape = "Produção de defensivos agrícolas biológicos e orgânicos",
            subactivity_landscape = if_else((grepl("\\bfomento a participacao da agricultura familiar nas cadeias de energias renovaveis e bioinsumos\\b", x = Coluna_search , ignore.case = TRUE)),true = "Desenvolvimento e inovação do setor de bioinsumos.",
            false = if_else((grepl("\\bcontrole fitossanitario da vassoura-de-bruxa e outras doencas e pragas do cultivo do cacau e monitoramento da moniliase para mitigar os efeitos e danos no cacaueiro\\b", x = Coluna_search , ignore.case = TRUE)),
            true = "Fabricação de fertilizantes orgânicos, produtos para o controle biológico de pragas e desenvolvimento de novas tecnologias.",false = "Sem Classificacao")),
            climate_component = "Adaptação"
            )


siop_adaptacao_desenvolvimentoZoneamento <- siop_sectorlandscape %>% filter(
  (grepl("\\brealizacao de zoneamento ambiental produtivo e aplicacao dos indicadores de sustentabilidade em agroecossistemas em territorios selecionados\\b", x = Coluna_search , ignore.case = TRUE))
) %>% mutate(
  activity_landscape = "Desenvolvimento de zoneamentos e de matriz de riscos climáticos",
  subactivity_landscape= if_else((grepl("\\brealizacao de zoneamento ambiental produtivo e aplicacao dos indicadores de sustentabilidade em agroecossistemas em territorios selecionados\\b", x = Coluna_search , ignore.case = TRUE)),
  true = "Realização de zoneamento ambiental produtivo e aplicação dos indicadores de sustentabilidade em agroecossistemas de territórios selecionados.", false = "Sem Classificacao"),
  climate_component = "Adaptação"
)

# Fazendo o quinto filtro
filtro_5 <- rbind(siop_adaptacao_ProducaoDefensivosAgricolas,siop_adaptacao_desenvolvimentoZoneamento,filtro_4)
siop_sectorlandscape <- siop_sectorlandscape %>% anti_join(filtro_5 , by="Coluna_search")

# Gerenciamento e monitoramento para uso de água e saneamento
siop_GerenciamentoMonitoramentosoAguaSaneamento <- siop_sectorlandscape %>% filter(
  (grepl("\\bsaneamento\\b", x = Coluna_search , ignore.case = TRUE)) | (grepl("\\birrigacao\\b", x = Coluna_search , ignore.case = TRUE)) | (grepl("\\bsubterraneas\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bapoio aos polos de agricultura irrigada\\b", x = Coluna_search , ignore.case = TRUE)|
  grepl("\\bapoio a sistemas de drenagem urbana sustentavel e de manejo de aguas pluviais em municipios criticos sujeitos a eventos recorrentes de inundacoes, enxurradas e alagamentos\\b", x = Coluna_search , ignore.case = TRUE) |
  grepl("\\bapoio a implantacao, ampliacao ou melhorias de sistemas de esgotamento sanitario em municipios com populacao superior a 50 mil habitantes ou municipios integrantes de regioes metropolitanas ou de regioes integradas de desenvolvimento\\b", x = Coluna_search , ignore.case = TRUE)|
  grepl("\\bapoio a empreendimentos de saneamento integrado em municipios com populacao superior a 50 mil habitantes ou municipios integrantes de regioes metropolitanas ou de regioes integradas de desenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) |
  grepl("\\bapoio a implementacao de acoes de desenvolvimento do setor aguas\\b", x = Coluna_search , ignore.case = TRUE) |
  grepl("\\bimplantacao, ampliacao, melhoria ou adequacao de sistemas de esgotamento sanitario na area de atuacao da codevasf\\b", x = Coluna_search , ignore.case = TRUE))
) %>% mutate(activity_landscape = "Gerenciamento e monitoramento para uso de água e saneamento") %>% mutate(
  subactivity_landscape = if_else((grepl("\\baducao de agua bruta\\b", x = project_description , ignore.case = TRUE) | grepl("\\bintervencoes emergenciais para efetivacao dos processos de alocacao de agua\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bfiscalizacao da seguranca de barragens\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bprevencao de eventos hidrologicos criticos\\b", x = Coluna_search , ignore.case = TRUE)),true = "Construção ou recuperação de barragens, tanques e sistemas de captação de água. Implementação de sistemas de armazenamento de água para proteção contra efeitos da seca sazonal.",
                                  false = if_else((grepl("\\bpisf\\b", x = Coluna_search , ignore.case = TRUE)), true = "Abastecimento público de água em comunidades ribeirinhas dos Rios São Francisco, do Parnaíba, do Itapecuru e do Mearim (Programa Água para Todos). Construção e adequação de sistemas públicos de esgotamento sanitário em comunidades ribeirinhas.",
                                  false = if_else((grepl("\\birrigacao\\b", x = project_description , ignore.case = TRUE) |grepl("\\bapoio aos polos de agricultura irrigada\\b", x = project_description , ignore.case = TRUE)), true = "Irrigação por gotejamento, outros tipos de irrigação, reservatórios e exploração de águas subterrâneas para a agricultura.",
                                  false = if_else((grepl("\\bsaneamento\\b", x = project_description , ignore.case = TRUE) | grepl("\\brnqa\\b", x = project_description , ignore.case = TRUE) | grepl("\\bapoio a implementacao de planos de recursos hidricos\\b", x = project_description , ignore.case = TRUE) | grepl("\\bsnirh", x = project_description , ignore.case = TRUE) | grepl("\\baguas subterraneas e superficiais\\b", x = project_description , ignore.case = TRUE) | grepl("\\belaboracao de planos e estudos de recursos hidricos\\b", x = project_description , ignore.case = TRUE) | grepl("\\bcapacitacao para a gestao de recursos hidricos\\b", x = project_description , ignore.case = TRUE) | grepl("\\bpromocao da conservacao e do uso sustentavel da agua\\b", x = project_description , ignore.case = TRUE) | grepl("\\bcadastro nacional de usuarios de recursos hidricos\\b", x = project_description , ignore.case = TRUE) | grepl("\\bapoio aos comites, agencias de bacia hidrografica e orgaos gestores estaduais e do distrito federal\\b", x = project_description , ignore.case = TRUE) |
                                  grepl("\\bapoio a sistemas de drenagem urbana sustentavel e de manejo de aguas pluviais em municipios criticos sujeitos a eventos recorrentes de inundacoes, enxurradas e alagamentos\\b", x = Coluna_search , ignore.case = TRUE) |
                                  grepl("\\bapoio a implantacao, ampliacao ou melhorias de sistemas de esgotamento sanitario em municipios com populacao superior a 50 mil habitantes ou municipios integrantes de regioes metropolitanas ou de regioes integradas de desenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) |
                                  grepl("\\bapoio a empreendimentos de saneamento integrado em municipios com populacao superior a 50 mil habitantes ou municipios integrantes de regioes metropolitanas ou de regioes integradas de desenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) |
                                  grepl("\\bapoio a implementacao de acoes de desenvolvimento do setor aguas\\b", x = Coluna_search , ignore.case = TRUE)|
                                  grepl("\\bimplantacao, ampliacao, melhoria ou adequacao de sistemas de esgotamento sanitario na area de atuacao da codevasf\\b", x = Coluna_search , ignore.case = TRUE)),
                                  true = "Programas de abastecimento de água, saneamento e higiene",
                                  false = if_else((grepl("\\bremocao de cargas poluidoras de bacias hidrograficas - prodes\\b", x = project_description , ignore.case = TRUE) | grepl("\\bcooperacao nacional e internacional em recursos hidricos\\b", x = project_description , ignore.case = TRUE)),true = "Projetos de infraestrutura e atividades institucionais para o manejo integrado de bacias hidrográficas.", false = "Sem Classificacao")))))
  ) %>% mutate(climate_component = "Mitigação e Adaptação")

# Fazendo o sexto filtro
filtro_6 <- rbind(siop_GerenciamentoMonitoramentosoAguaSaneamento,filtro_5)
siop_sectorlandscape <- siop_sectorlandscape %>% anti_join(filtro_6 , by="Coluna_search")

#Extensão rural para melhorar as práticas agronômicas e o acesso à tecnologia e infraestrutura
siop_ExtensaoRural <- siop_sectorlandscape %>% filter((grepl("\\bextensao rural\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bpromocao da educacao do campo\\b", x = Coluna_search , ignore.case = TRUE))) %>% mutate(
      activity_landscape = "Extensão rural para melhorar as práticas agronômicas e o acesso à tecnologia e infraestrutura",
      subactivity_landscape  = if_else((grepl("\\bfomento a producao de tecnologias e de conhecimentos\\b", x = project_description , ignore.case = TRUE) | grepl("\\bformacao e capacitacao tecnica e profissional \\b", x = project_description , ignore.case = TRUE) | grepl("\\bassistencia tecnica e extensao rural para o produtor rural\\b", x = project_description , ignore.case = TRUE) | grepl("\\bassistencia tecnica e extensao rural - despesas diversas\\b", x = project_description , ignore.case = TRUE)), true = "Assistência técnica e extensão rural, capacitação de técnicos e produtores, estruturação das entidades estaduais de assistência técnica.", 
      false = if_else((grepl("\\bpromocao da educacao do campo\\b", x = Coluna_search , ignore.case = TRUE)),true = "Treinamento agrícola não formal.", false = "Sem Classificacao"))
) %>% mutate(climate_component = "Mitigação e Adaptação")

# Fazendo o sétimo filtro
filtro_7 <- rbind(siop_ExtensaoRural,filtro_6)
siop_sectorlandscape <- siop_sectorlandscape %>% anti_join(filtro_7 , by="Coluna_search")

#P&D, sistemas de gestão do conhecimento

pd_SistemasGestaoConhecimento <- siop_sectorlandscape %>% filter(
  ((grepl("\\bbiocombustiveis\\b", x = Coluna_search , ignore.case = TRUE)) | (grepl("\\bjardim\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bjardim\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgestao das colecoes vivas\\b", x = Coluna_search , ignore.case = TRUE)) | (grepl("\\brecursos geneticos\\b", x = Coluna_search , ignore.case = TRUE)) | (grepl("\\bsaude\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpromocao da saude de animais aquaticos\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bcenso\\b", x = Coluna_search , ignore.case = TRUE)) | (grepl("\\bembrapa\\b", x = Coluna_search , ignore.case = TRUE) ) |
    (grepl("\\bproducao agropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bproducao agropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbaixa emissao\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bproducao agropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bconservacao de solo e da agua\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bproducao agropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesenvolvimento da agricultura irrigada\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bproducao agropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutos agropecuarios\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bmudancas climaticas\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\blevantamento e interpretacao de informacoes de solos\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bcacau\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bcoordenacao e gestao do abastecimento agroalimentar\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bproducao aquicola sustentavel apoio ao funcionamento de unidades de producao, a pesquisa, ao desenvolvimento tecnologico e a inovacao para a producao aquicola sustentavel\\b", x = Coluna_search , ignore.case = TRUE))|
    (grepl("\\bsuasa\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bpromocao e fortalecimento da estruturacao produtiva da agricultura familiar, pequenos e medios produtores rurais\\b", x = Coluna_search , ignore.case = TRUE)) | 
    (grepl("\\bestruturacao e inclusao produtiva dos agricultores familiares e dos pequenos e medios produtores rurais\\b", x = Coluna_search , ignore.case = TRUE)) |
    (grepl("\\bdesenvolvimento da cafeicultura\\b", x = Coluna_search , ignore.case = TRUE)))
) %>% mutate(activity_landscape = "P&D, sistemas de gestão do conhecimento",
            subactivity_landscape = if_else((grepl("\\bbiocombustiveis\\b", x = Coluna_search , ignore.case = TRUE)), true = "P&D, inovação e estudos da indústria de biocombustíveis.",
            false = if_else((grepl("\\bjardim\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE)),true = "Bancos de dados, inventários, perfis ambientais, estudos de impacto",
            false = if_else((grepl("\\bjardim\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgestao das colecoes vivas\\b", x = Coluna_search , ignore.case = TRUE)),true = "Bancos de dados, inventários, perfis ambientais, estudos de impacto",
            false = if_else((grepl("\\brecursos geneticos\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bdesenvolvimento sustentavel da economia de patrimonio genetico e conhecimentos tradicionais e reparticao de beneficios\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bsuasa\\b", x = Coluna_search , ignore.case = TRUE)),true = "Pesquisas relacionadas ao melhoramento de plantas, recursos genéticos, saúde animal e biotecnologia agrícola.",
            false = if_else((grepl("\\bsaude\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpromocao da saude de animais aquaticos\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bdesenvolvimento da cafeicultura\\b", x = Coluna_search , ignore.case = TRUE)),true = "Pesquisas relacionadas ao melhoramento de plantas, recursos genéticos, saúde animal e biotecnologia agrícola.",
            false = if_else((grepl("\\bcenso\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bcoordenacao e gestao do abastecimento agroalimentar\\b", x = Coluna_search , ignore.case = TRUE)), true = "Geração e difusão de informações da agropecuária e do abastecimento agroalimentar. Desenvolvimento de plataforma de gestão de indicadores de sustentabilidade agroambiental e de indicadores para políticas agroambientais. Censos Demográfico, Agropecuário e Geográfico.",
            false = if_else((grepl("\\bembrapa\\b", x = Coluna_search , ignore.case = TRUE) | grepl("\\bproducao aquicola sustentavel apoio ao funcionamento de unidades de producao, a pesquisa, ao desenvolvimento tecnologico e a inovacao para a producao aquicola sustentavel\\b", x = Coluna_search, ignore.case = TRUE) | grepl("\\bpromocao e fortalecimento da estruturacao produtiva da agricultura familiar, pequenos e medios produtores rurais\\b", x = Coluna_search, ignore.case = TRUE) | grepl("\\bestruturacao e inclusao produtiva dos agricultores familiares e dos pequenos e medios produtores rurais\\b", x = Coluna_search, ignore.case = TRUE)),true = "Adequação, ampliação, revitalização e modernização da infraestrutura das unidades da Embrapa. P&D para produção agropecuária sustentável e de baixo carbono, adaptação às mudanças ambientais globais, aumento da competitividade da produção de base familiar e das comunidades tradicionais.",
            false = if_else((grepl("\\bproducao agropecuaria\\b", x = Coluna_search, ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search, ignore.case = TRUE)),true = "Adequação, ampliação, revitalização e modernização da infraestrutura das unidades da Embrapa. P&D para produção agropecuária sustentável e de baixo carbono, adaptação às mudanças ambientais globais, aumento da competitividade da produção de base familiar e das comunidades tradicionais.",
            false = if_else((grepl("\\bproducao agropecuaria\\b", x = Coluna_search, ignore.case = TRUE) & grepl("\\bbaixa emissao\\b", x = Coluna_search, ignore.case = TRUE)),true = "Adequação, ampliação, revitalização e modernização da infraestrutura das unidades da Embrapa. P&D para produção agropecuária sustentável e de baixo carbono, adaptação às mudanças ambientais globais, aumento da competitividade da produção de base familiar e das comunidades tradicionais.",
            false = if_else((grepl("\\bproducao agropecuaria\\b", x = Coluna_search, ignore.case = TRUE) & grepl("\\bconservacao de solo e da agua\\b", x = Coluna_search, ignore.case = TRUE)),true= "Adequação, ampliação, revitalização e modernização da infraestrutura das unidades da Embrapa. P&D para produção agropecuária sustentável e de baixo carbono, adaptação às mudanças ambientais globais, aumento da competitividade da produção de base familiar e das comunidades tradicionais.",
            false = if_else((grepl("\\bproducao agropecuaria\\b", x = Coluna_search, ignore.case = TRUE) & grepl("\\bdesenvolvimento da agricultura irrigada\\b", x = Coluna_search, ignore.case = TRUE)),true = "Adequação, ampliação, revitalização e modernização da infraestrutura das unidades da Embrapa. P&D para produção agropecuária sustentável e de baixo carbono, adaptação às mudanças ambientais globais, aumento da competitividade da produção de base familiar e das comunidades tradicionais.",
            false = if_else((grepl("\\bproducao agropecuaria\\b", x = Coluna_search, ignore.case = TRUE) & grepl("\\bprodutos agropecuarios\\b", x = Coluna_search, ignore.case = TRUE)),true = "Adequação, ampliação, revitalização e modernização da infraestrutura das unidades da Embrapa. P&D para produção agropecuária sustentável e de baixo carbono, adaptação às mudanças ambientais globais, aumento da competitividade da produção de base familiar e das comunidades tradicionais.",
            false = if_else((grepl("\\bmudancas climaticas\\b", x = Coluna_search, ignore.case = TRUE)),true = "Adequação, ampliação, revitalização e modernização da infraestrutura das unidades da Embrapa. P&D para produção agropecuária sustentável e de baixo carbono, adaptação às mudanças ambientais globais, aumento da competitividade da produção de base familiar e das comunidades tradicionais.",
            false = if_else((grepl("\\bcacau\\b", x = Coluna_search, ignore.case = TRUE)),true = "Difusão e transferência de tecnologia para o desenvolvimento sustentável da agricultura e de SAFs nas regiõesprodutoras de cacau.",false = "Sem Classificacao"))))))))))))))) %>% mutate(subactivity_landscape = if_else((grepl("\\blevantamento e interpretacao de informacoes de solos\\b",x = project_description,ignore.case = TRUE)),true = "Unidades de Referência Tecnológica (URTs) do Plano Brasil Sem Miséria (BSM) e do Sistema Nacional de Pesquisas Agropecuárias (SNPA). Pesquisa, acompanhamento e avaliação de safras e perdas na pós-colheita. Levantamento e interpretação de informações de solos.",false = subactivity_landscape)) %>%
            mutate(climate_component = "Mitigação e Adaptação")  

# Fazendo o oitavo filtro
filtro_8 <- rbind(pd_SistemasGestaoConhecimento,filtro_7)
siop_sectorlandscape <- siop_sectorlandscape %>% anti_join(filtro_8 , by="Coluna_search")

#Atividades para redução de emissões por desmatamento e degradação
atividades_reducao_emissoes_desmatamento_Degradacao <- siop_sectorlandscape %>% filter(
  (grepl("\\bformulacao e implementacao de estrategias para promover a conservacao, a recuperacao e o uso sustentavel da biodiversidade, da vegetacao nativa e do patrimonio genetico - despesas diversas\\b", x = Coluna_search, ignore.case = TRUE)) |
  (grepl("\\bimplementacao da recuperacao ambiental da bacia carbonifera de santa catarina\\b", x = Coluna_search, ignore.case = TRUE)) |
  (grepl("\\bconservacao e uso sustentavel das especies\\b", x = Coluna_search, ignore.case = TRUE)) |
  (grepl("\\bconservacao, uso sustentavel e recuperacao de ecossistemas\\b", x = Coluna_search, ignore.case = TRUE)) |
  (grepl("\\bdesenvolvimento de instrumentos economicos e financeiros para a conservacao e recuperacao da vegetacao nativa\\b", x = Coluna_search, ignore.case = TRUE)) |
  (grepl("\\bvalorizacao de comunidades rurais, de seus produtos, servicos e processos relacionados a sociobiodiversidade\\b", x = Coluna_search, ignore.case = TRUE))
) %>% mutate(activity_landscape = "Atividades para redução de emissões por desmatamento e degradação",
            subactivity_landscape = if_else((grepl("\\bformulacao e implementacao de estrategias para promover a conservacao, a recuperacao e o uso sustentavel da biodiversidade, da vegetacao nativa e do patrimonio genetico\\b", x = Coluna_search, ignore.case = TRUE) | grepl("\\bimplementacao da recuperacao ambiental da bacia carbonifera de santa catarina\\b", x = Coluna_search, ignore.case = TRUE) | grepl("\\bconservacao e uso sustentavel das especies\\b", x = Coluna_search, ignore.case = TRUE) | grepl("\\bdesenvolvimento de instrumentos economicos e financeiros para a conservacao e recuperacao da vegetacao nativa\\b", x = Coluna_search, ignore.case = TRUE) | grepl("\\bconservacao, uso sustentavel e recuperacao de ecossistemas\\b", x = Coluna_search, ignore.case = TRUE)),
            true = "Conservação de florestas, restauração e recuperação de áreas degradadas, inclusive de vegetação nativa e áreas de preservação permanente, para melhorar o abastecimento de água. Projetos de reserva florestal privada.",
            false =if_else((grepl("\\bvalorizacao de comunidades rurais, de seus produtos, servicos e processos relacionados a sociobiodiversidade\\b", x = Coluna_search, ignore.case = TRUE)), true = "Produção extrativista, manejo florestal comunitário e a projetos socioambientais de organizações agroextrativistas, com ações de desenvolvimento de competências, suporte técnico e associativismo. Cadeias de valor de óleos vegetais, cacau silvestre e borracha e fortalecimento das cadeias produtivas florestais não madeireiras.",false = "Sem Classificacao"))) %>% 
            mutate(climate_component = "Mitigação e Adaptação")

# Fazendo o nono filtro
filtro_9 <- rbind(atividades_reducao_emissoes_desmatamento_Degradacao,filtro_8)
siop_sectorlandscape <- siop_sectorlandscape %>% anti_join(filtro_9 , by="Coluna_search")

# Sistemas de monitoramento e vigilância
sistemas_monitoramento_vigilancia <- siop_sectorlandscape %>% filter(
  (grepl("\\breducao da vulnerabilidade aos efeitos da desertificacao\\b", x = Coluna_search, ignore.case = TRUE)) |
  (grepl("\\bdesenvolvimento e aprimoramento dos modelos do sistema terrestre\\b", x = Coluna_search, ignore.case = TRUE)) |
  (grepl("\\bcemaden\\b", x = Coluna_search, ignore.case = TRUE)) |
  (grepl("\\bgeo\\b", x = Coluna_search, ignore.case = TRUE)) |
  (grepl("\\bhidro\\b", x = Coluna_search, ignore.case = TRUE)) |
  (grepl("\\bvigilancia\\b", x = Coluna_search, ignore.case = TRUE)) |
  (grepl("\\bpesquisas, estudos e levantamentos geocientificos\\b", x = Coluna_search, ignore.case = TRUE)) |
  (grepl("\\blevantamentos, estudos, previsao e alerta de eventos hidrologicos criticos\\b", x = Coluna_search, ignore.case = TRUE))|
  (grepl("\\bpesquisas e estudos estatisticos e geocientificos\\b", x = Coluna_search, ignore.case = TRUE)) |
  (grepl("\\bapoio a implantacao de infraestruturas para seguranca hidrica\\b", x = Coluna_search, ignore.case = TRUE)) |
  (grepl("\\bpesquisa, desenvolvimento e supercomputacao para previsao de tempo e clima\\b", x = Coluna_search , ignore.case = TRUE)) |
  (grepl("\\bpesquisa e desenvolvimento para estudos de tempo, clima, observacao e modelagem do sistema terrestre\\b", x = Coluna_search , ignore.case = TRUE))
) %>% mutate(activity_landscape = "Sistemas de monitoramento e vigilância",
            subactivity_landscape = if_else((grepl("\\breducao da vulnerabilidade aos efeitos da desertificacao\\b", x = Coluna_search, ignore.case = TRUE)  |
            grepl("\\blevantamentos, estudos, previsao e alerta de eventos hidrologicos criticos\\b", x = Coluna_search, ignore.case = TRUE) | grepl("\\bpesquisa, desenvolvimento e supercomputacao para previsao de tempo e clima\\b", x = Coluna_search , ignore.case = TRUE)| grepl("\\bpesquisa e desenvolvimento para estudos de tempo, clima, observacao e modelagem do sistema terrestre\\b", x = Coluna_search , ignore.case = TRUE)),
            true = "Serviços meteorológicos e climatológicos na rede nacional.",
            false = if_else((grepl("\\bdesenvolvimento e aprimoramento dos modelos do sistema terrestre\\b", x = Coluna_search, ignore.case = TRUE)),
            true = "Plataformas para coleta de dados meteorológicos e oceanográficos.",
            false = if_else((grepl("\\bcemaden\\b", x = Coluna_search, ignore.case = TRUE)),
            true = "Centro Nacional de Monitoramento e Alerta de Desastres Naturais (Cemaden)",
            false = if_else((grepl("\\bgeo\\b", x = Coluna_search, ignore.case = TRUE) | grepl("\\bhidro\\b", x = Coluna_search, ignore.case = TRUE) | grepl("\\bpesquisas, estudos e levantamentos geocientificos\\b", x = Coluna_search, ignore.case = TRUE) | grepl("\\bpesquisas e estudos estatisticos e geocientificos\\b", x = Coluna_search, ignore.case = TRUE) | grepl("\\bapoio a implantacao de infraestruturas para seguranca hidrica\\b", x = Coluna_search, ignore.case = TRUE)),
            true = "Gestão da informação geológica, mapeamento geológico-geotécnico em municípios críticos com relação a riscos geológicos. Operação da rede hidrometeorológica, levantamentos hidrogeológicos, implantação de infraestruturas para segurança hídrica. Pesquisas, estudos e levantamentos geocientíficos.",
            false = if_else((grepl("\\bvigilancia\\b", x = Coluna_search, ignore.case = TRUE) | grepl("\\bregistro genealogico, vigilancia e controle da producao e comercializacao de material genetico animal\\b", x = Coluna_search, ignore.case = TRUE)),
            true = "Vigilância e controle das atividades com organismos geneticamente modificados, controle da produção e comercialização de material genético animal, insumos destinados à alimentação animal e de produtos de uso veterinário.",
            false = "Sem Classificacao")))))) %>% mutate(climate_component = "Mitigação e Adaptação")


# Fazendo o decimo filtro
filtro_10 <- rbind(sistemas_monitoramento_vigilancia,filtro_9)
siop_sectorlandscape <- siop_sectorlandscape %>% anti_join(filtro_10 , by="Coluna_search")

#Manejo de nutrientes e controle de pragas Serviços pecuários e veterinários
manejo_nutrientes_controle_pragas_servicos_pecuarios_vet <- siop_sectorlandscape %>% filter(
  (grepl("\\bdesenvolvimento sustentavel da economia de patrimonio genetico e conhecimentos tradicionais e reparticao de beneficios\\b", x = Coluna_search, ignore.case = TRUE) |
  grepl("\\bfiscalizacao de produtos destinados a alimentacao animal\\b", x = Coluna_search, ignore.case = TRUE)|
  grepl("\\binspecao e fiscalizacao de produtos de origem animal e de produtos destinados a alimentacao animal\\b", x = Coluna_search, ignore.case = TRUE)|
  grepl("\\berradicacao\\b", x = Coluna_search, ignore.case = TRUE) | 
  grepl("\\bapoio a formulacao e implementacao de politicas e programas para protecao e defesa animal\\b", x = Coluna_search, ignore.case = TRUE))
) %>% mutate(
  activity_landscape = "Manejo de nutrientes e controle de pragas Serviços pecuários e veterinários",
  subactivity_landscape = if_else((grepl("\\bdesenvolvimento sustentavel da economia de patrimonio genetico e conhecimentos tradicionais e reparticao de beneficios\\b", x = Coluna_search, ignore.case = TRUE)),
  true = "Projetos para saúde e manejo animal, recursos genéticos, recursos alimentares.",
  false = if_else((grepl("\\bfiscalizacao de produtos destinados a alimentacao animal\\b",x = Coluna_search,ignore.case = TRUE) | grepl("\\binspecao e fiscalizacao de produtos de origem animal e de produtos destinados a alimentacao animal\\b", x = Coluna_search, ignore.case = TRUE) |
  grepl("\\berradicacao\\b",x = Coluna_search,ignore.case = TRUE) | 
  grepl("\\bapoio a formulacao e implementacao de politicas e programas para protecao e defesa animal\\b",x = Coluna_search,ignore.case = TRUE)),
  true = "Projetos para saúde e manejo animal, recursos genéticos, recursos alimentares.",
  false ="Sem Classificacao" ))
) %>%mutate(climate_component="Mitigação e Adaptação")

# Fazendo o decimo primeiro filtro
filtro_11 <- rbind(manejo_nutrientes_controle_pragas_servicos_pecuarios_vet,filtro_10)
siop_sectorlandscape <- siop_sectorlandscape %>% anti_join(filtro_11 , by="Coluna_search")

filtro_11%>%view
siop_landscape_climate_use
resto <- siop_sectorlandscape


sem_filtro_dicionario%>% filter(
  (!grepl("\\baposentadorias e pensoes civis da uniao\\b",x = Coluna_search,ignore.case = TRUE)) &
  (!grepl("\\bativos civis da uniao\\b",x = Coluna_search,ignore.case = TRUE)) &
  (!grepl("\\bbeneficios obrigatorios aos servidores civis, empregados, militares e seus dependentes\\b",x = Coluna_search,ignore.case = TRUE)) &
  (!grepl("\\badministracao da unidade\\b",x = Coluna_search,ignore.case = TRUE))
) %>%select(project_name,project_description,sector_original,subsector_original,channel_original,source_original, Coluna_search) %>% unique%>% view


resto%>%select(project_name,project_description,sector_original,subsector_original,channel_original,source_original, Coluna_search) %>% unique%>% view






library(tidyverse)
library(stringi)
library(readxl)
library(xlsx)
source("C:/Users/napcc/Dropbox (CPI)/EduardoMinsky/PARAMIM/landuse_br2024/AuxFolder/Dictionary_Sectors.R")
setwd("A:\\projects\\landuse_br2024\\SIOP\\Clean_Data")

siop_tratado <- read_rds('Siop_Tratado_2015_2023_05_24.rds') #258.437 registros

#Tabelas relacionais
grupo_despesa <- read_excel("A:/projects/landuse_br2024/SIOP/12_siop_relational_tables.xlsx", sheet="grupo_despesa")
channel_landscape <- read_excel("A:/projects/landuse_br2024/SIOP/12_siop_relational_tables.xlsx", sheet="channel_landscape")
sector_landscape <- read_excel("A:/projects/landuse_br2024/SIOP/12_siop_relational_tables.xlsx", sheet="sector_landscape")
sector_landscape <- sector_landscape %>% mutate(acao_des_limpa = str_trim(str_to_lower(stri_trans_general(acao_des_orig,"Latin-ASCII"))))
sector_landscape <- sector_landscape %>%select(acao_des_limpa,sector_landscape)%>% distinct(acao_des_limpa,.keep_all=TRUE) 
beneficiary_landscape <- read_excel("A:/projects/landuse_br2024/SIOP/12_siop_relational_tables.xlsx", sheet="beneficiary_landscape")
beneficiary_landscape <- beneficiary_landscape%>% mutate(acao_des_limpa = str_trim(str_to_lower(stri_trans_general(acao_des,"Latin-ASCII"))))
beneficiary_landscape <- beneficiary_landscape %>% select(acao_des_limpa,beneficiary_landscape)
####################################################
source_landscape <- read_excel("A:/projects/landuse_br2024/SIOP/12_siop_relational_tables.xlsx", sheet="source_landscape")

source_landscape <- source_landscape%>%mutate(source_original = str_trim(str_to_lower(stri_trans_general(source_original,"Latin-ASCII"))))

instrument_landscape <- read_excel("A:/projects/landuse_br2024/SIOP/12_siop_relational_tables.xlsx", sheet="instrument_landscape")

climate_use <- read_excel("A:/projects/landuse_br2024/SIOP/12_siop_relational_tables.xlsx", sheet="climate_use")
climate_use <- climate_use %>% mutate(acao_des_limpa = str_trim(str_to_lower(stri_trans_general(acao_des,"Latin-ASCII"))))

acao_plan_orc_count <- read_excel("A:/projects/landuse_br2024/SIOP/12_siop_relational_tables.xlsx",sheet = "acao_plan_orc_filter")
acao_plan_orc_count_unique <- acao_plan_orc_count %>% mutate(plano_orc_clean = str_trim(str_to_lower(stri_trans_general(plano_orc_clean,"Latin-ASCII")),side="both")) %>% select(plano_orc_clean)%>%unique
######################################################################
# Filtrando as observações que nao pagaram nada e filtro para ano
siop_tratado_ano <- siop_tratado %>% filter(Pago != 0)
siop_tratado_ano <- siop_tratado_ano%>%  filter(Ano >= 2021 & Ano <= 2023) 

# 2.1 Filter Grupo Despesa
grupo_despesa %>% filter(select != 1) %>% select(grupo_despesa) %>% as.vector()
siop_tratado_ano <- siop_tratado_ano %>% filter(grupo_de_despesa != "juros e encargos da divida" & grupo_de_despesa!= "amortizacao da divida" & grupo_de_despesa != "reserva de contingencia") #141.998 registros

# 2.3 Filtro de unidade orçamentária baseado na sheet channel landscape:
siop_tratado_unidade_orcamentaria <- siop_tratado_ano %>% inner_join(channel_landscape %>% filter(!is.na(und_orc)) %>% select(und_orc)%>% unique, by= "und_orc") 
channel_landscape %>% filter(!is.na(und_orc)) %>% select(und_orc) %>% anti_join(siop_tratado_ano, by= "und_orc") %>% view

# Pelo anti Join podemos ver que a unidade orçamentaria secretaria especial de agricultura familiar e do desenvolvimento agrario
# nao existe para os anos de 21 até 2023

# 2.3 Criando o primeiro Remainder:
df_remainder_SIOP1 <- siop_tratado_ano %>% anti_join(siop_tratado_unidade_orcamentaria %>% select(und_orc )%>% unique, by= "und_orc")

#df_remainder_SIOP1 %>% select(und_orc) %>% unique  %>% write.xlsx("A:\\projects\\landuse_br2024\\siop\\ExploracoesDados\\UnidadesOrcamentarias_2021_2023_Fora.xlsx")

#3.1 Seleção Setorial: É baseado nas ações
siop_tratado_unidade_orcamentaria_acao <- siop_tratado_unidade_orcamentaria %>% inner_join(sector_landscape %>% select(acao_des_limpa),by = c("acao" = "acao_des_limpa")) 
sector_landscape%>% select(acao_des_limpa) %>% anti_join(siop_tratado_unidade_orcamentaria,by =c( "acao_des_limpa" ="acao" ) )

# Tivemos ações da tabela relacional que nao tiveram match pois essas ações para os anos de 21 ate 23 tiveram valor pago de ZERO ou Acoes que nao existem
# para os anos em questao
# Esta explicacao encontra-se na pasta ExploracoesDados
#3.1 Criando o Remainder 2
df_remainder_siop2 <- siop_tratado_unidade_orcamentaria %>% anti_join(siop_tratado_unidade_orcamentaria_acao %>% select(acao),by = "acao" )
#df_remainder_siop2 %>% select(acao,plano_orc,und_orc) %>% unique %>%  write.xlsx("A:\\projects\\landuse_br2024\\siop\\ExploracoesDados\\Acoes_2021_2023_Fora.xlsx")

# 3.2: Fitro de Planos Orcamentários
siop_tratado_unidade_orcamentaria_acao_plano_orc <- siop_tratado_unidade_orcamentaria_acao  %>% inner_join(acao_plan_orc_count_unique,by = c("plano_orc"="plano_orc_clean")) 
#acao_plan_orc_count_unique %>% anti_join(siop_tratado_unidade_orcamentaria_acao,by = c("plano_orc_clean" = "plano_orc")) %>% unique %>% write.xlsx("A:\\projects\\landuse_br2024\\siop\\ExploracoesDados\\Planos_orcDeForaFiltro3.2.xlsx")

# 3.2: Criando o Remainder Tres:
df_remainder_siop3 <- siop_tratado_unidade_orcamentaria_acao%>% anti_join(siop_tratado_unidade_orcamentaria_acao_plano_orc,by = "plano_orc")

df_remainder_siop3 %>% select(plano_orc,acao,und_orc) %>% unique %>%  write.xlsx("A:\\projects\\landuse_br2024\\siop\\ExploracoesDados\\Planos_orc_2021_2023_Fora.xlsx")


siop_tratado_unidade_orcamentaria_acao_plano_orc$Pago %>% sum
52.465.588.665
######################################################################################
acao_plan_orc_count_unique %>% anti_join(siop_tratado_unidade_orcamentaria_acao_plano_orc,by = c("plano_orc_clean"="plano_orc")) %>% view 

adm_unidades_filter <- siop_tratado_unidade_orcamentaria_acao_plano_orc %>% filter(acao == "administracao da unidade")  %>% filter(und_orc =="ministerio do meio ambiente e mudanca do clima - administracao direta"|und_orc == "instituto brasileiro do meio ambiente e dos recursos naturais renovaveis - ibama" | und_orc == "fundacao nacional do indio - funai" | und_orc == "instituto chico mendes de conservacao da biodiversidade" |  und_orc == "servico florestal brasileiro - sfb" | und_orc == "instituto de pesquisas jardim botanico do rio de janeiro - jbrj")%>%
  filter(plano_orc == "administracao da sede"| plano_orc == "administracao das unidades regionais"| plano_orc=="administracao das unidades regionais - regra de ouro" |plano_orc=="administracao da unidade" |plano_orc=="administracao da unidade - despesas diversas"|plano_orc=="administracao da unidade - despesas diversas - regra de ouro" | plano_orc == "capacitacao de servidores publicos federais do ibama" | plano_orc == "capacitacao de servidores publicos federais em processo de qualificacao e requalificacao" | plano_orc == "capacitacao de servidores publicos federais em processo de qualificacao e requalificacao - area fim" |
           plano_orc == "capacitacao de servidores publicos federais em processo de qualificacao e requalificacao - area meio" | plano_orc == "capacitacao de servidores publicos federais em processo de qualificacao e requalificacao - regra de ouro" | plano_orc == "coronavirus (covid-19)" |
           plano_orc == "educacao ambiental" |plano_orc == "emenda individual"|
           plano_orc == "gestao administrativa da politica contra as mudancas do clima e de promocao da qualidade ambiental"|plano_orc == "gestao administrativa da politica da biodiversidade"|
           plano_orc == "gestao administrativa da politica das mudancas do clima e de florestas"|
           plano_orc == "gestao administrativa da politica de articulacao e cidadania ambiental"|
           plano_orc == "gestao administrativa da politica de desenvolvimento rural e extrativismo"|
           plano_orc == "gestao administrativa da politica de meio ambiente"|
           plano_orc == "gestao administrativa da politica de recursos hidricos e de qualidade ambiental"|
           plano_orc == "gestao administrativa da politica de relacoes internacionais"|
           plano_orc=="gestao administrativa da politica florestal e da biodiversidade"|
           plano_orc == "gestao administrativa da politica residuos solidos, ambientes urbanos e recursos hidricos"|
           plano_orc == "gestao de politicas de meio ambiente"|
           plano_orc == "manutencao e adaptacao de imoveis"|
           plano_orc == "manutencao e adaptacao de imoveis - regra de ouro"|
           plano_orc == "manutencao e modernizacao da infraestrutura fisica da sede e das unidades descentralizadas"|
           plano_orc == "modernizacao da estrutura de informatica do ibama"|
           plano_orc == "modernizacao da estrutura de informatica do ministerio do meio ambiente"|
           plano_orc == "modernizacao da estrutura de informatica do ministerio do meio ambiente - regra de ouro"|
           plano_orc == "outras despesas administrativas"|
           plano_orc == "outras despesas administrativas - regra de ouro"|
           plano_orc == "reuniao dos comites regionais"|
           plano_orc == "tecnologia da informacao"|
           plano_orc == "tecnologia da informacao - regra de ouro") # Esta de acordo com o Ajuste 1




resto <- siop_tratado_unidade_orcamentaria_acao_plano_orc%>% filter(acao != "administracao da unidade")

# Aplicando Ajuste 2 e 3 ao mesmo tempo
folha_pagamento <- resto %>% filter(acao == "assistencia medica e odontologica aos servidores civis, empregados, militares e seus dependentes"|
                                      acao == "assistencia pre-escolar aos dependentes dos servidores civis, empregados e militares"|
                                      acao == "auxilio-transporte aos servidores civis, empregados e militares"|
                                      acao == "auxilio-alimentacao aos servidores civis, empregados e militares"|
                                      acao == "beneficios assistenciais decorrentes do auxilio-funeral e natalidade"|
                                      acao == "contribuicao da uniao, de suas autarquias e fundacoes para o custeio do regime de previdencia dos servidores publicos federais"|
                                      acao=="ativos civis da uniao"| acao == "pagamento de pessoal ativo da uniao"|
                                      acao == "pessoal ativo da uniao"| acao == "beneficios obrigatorios aos servidores civis, empregados, militares e seus dependentes"|
                                      acao == "ajuda de custo para moradia ou auxilio-moradia a agentes publicos"|
                                      acao == "contribuicao da uniao, de suas autarquias e fundacoes para o custeio do regime de previdencia dos servidores publicos federais")
# Aqui entra o Ajuste 2 e 3
folha_pagamento_certo <- folha_pagamento %>% filter( und_orc == "instituto brasileiro do meio ambiente e dos recursos naturais renovaveis - ibama" |
                                                       und_orc == "fundacao nacional do indio - funai" | und_orc == "instituto chico mendes de conservacao da biodiversidade" |
                                                       und_orc == "servico florestal brasileiro - sfb" | und_orc=="ministerio do meio ambiente e mudanca do clima - administracao direta"|
                                                       und_orc == "instituto de pesquisas jardim botanico do rio de janeiro - jbrj") %>% filter(plano_orc == "ajuda de custo para moradia ou auxilio-moradia a agentes publicos"|
                                                                                                                                                  plano_orc == "ajuda de custo para moradia ou auxilio-moradia a agentes publicos - despesas diversas"|
                                                                                                                                                  plano_orc == "assistencia medica e odontologica aos servidores civis, empregados, militares e seus dependentes"|
                                                                                                                                                  plano_orc == "assistencia medica e odontologica de civis - complementacao da uniao"|
                                                                                                                                                  plano_orc == "assistencia medica e odontologica de civis - complementacao da uniao"|
                                                                                                                                                  plano_orc == "assistencia pre-escolar aos dependentes dos servidores civis e de empregados"|
                                                                                                                                                  plano_orc == "assistencia pre-escolar aos dependentes dos servidores civis e de empregados"|
                                                                                                                                                  plano_orc == "auxilio-alimentacao aos servidores civis, empregados e militares"|
                                                                                                                                                  plano_orc == "auxilio-transporte aos servidores civis, empregados e militares"|
                                                                                                                                                  plano_orc == "regra de ouro: assistencia medica e odontologica de civis - complementacao da uniao"|
                                                                                                                                                  plano_orc == "regra de ouro: assistencia medica e odontologica de civis - complementacao da uniao"|
                                                                                                                                                  plano_orc == "auxilio-alimentacao de civis"|plano_orc == "auxilio-transporte - civis"|
                                                                                                                                                  plano_orc== "ajuda de custo para moradia ou auxilio-moradia a agentes publicos"|
                                                                                                                                                  plano_orc == "capacitacao de servidores publicos federais em processo de qualificacao e requalificacao"|
                                                                                                                                                  plano_orc == "capacitacao de servidores publicos federais em processo de qualificacao e requalificacao - regra de ouro"|
                                                                                                                                                  plano_orc == "capacitacao de servidores publicos federais em processo de qualificacao e requalificacao - area fim"|
                                                                                                                                                  plano_orc == "capacitacao de servidores publicos federais em processo de qualificacao e requalificacao - area meio"|
                                                                                                                                                  plano_orc == "ajuda de custo para moradia ou auxilio-moradia a agentes publicos - despesas diversas - regra de ouro"|
                                                                                                                                                  plano_orc == "ativos civis da uniao"|
                                                                                                                                                  plano_orc == "auxilio-funeral e natalidade de civis"|
                                                                                                                                                  plano_orc == "assistencia pre-escolar aos dependentes de servidores civis e de empregados"|
                                                                                                                                                  plano_orc == "auxilio-funeral e natalidade de civis"|
                                                                                                                                                  plano_orc == "auxilio-transporte de civis"|
                                                                                                                                                  plano_orc == "regra de ouro: assistencia pre-escolar aos dependentes de servidores civis e de empregados"|
                                                                                                                                                  plano_orc == "regra de ouro: auxilio-alimentacao de civis"|
                                                                                                                                                  plano_orc == "regra de ouro: auxilio-funeral e natalidade de civis"|
                                                                                                                                                  plano_orc == "contribuicao da uniao, de suas autarquias e fundacoes para o custeio do regime de previdencia dos servidores publicos federais"|plano_orc == "pagamento de pessoal ativo da uniao"|plano_orc == "ativos civis da uniao"|plano_orc == "pessoal ativo da uniao")



resto2 <- resto %>% anti_join(folha_pagamento)
data_siop_final <- bind_rows(adm_unidades_filter,resto2,folha_pagamento_certo)
data_siop_final$Pago %>% sum
#Valor sem rio_marker 23.549.526.033  


ficou_fora_landscape<- siop_tratado_unidade_orcamentaria_acao_plano_orc %>% anti_join(data_siop_final)
restante <- ficou_fora_landscape %>% filter((acao == "administracao da unidade" & plano_orc == "administracao da unidade - inpa")|
                                              (acao == "administracao da unidade" & plano_orc == "administracao da unidade - inpa - regra de ouro")|
                                              (acao == "administracao da unidade" & plano_orc == "capacitacao de recursos humanos no inpa")|
                                              (acao == "administracao da unidade" & plano_orc == "capacitacao de recursos humanos no inpe")|
                                              (acao == "administracao da unidade" & plano_orc == "capacitacao de recursos humanos no inpe - regra de ouro"))

data_siop_final2 <- rbind(restante,data_siop_final)

# Inicio da Transformacao para o landscape
data_siop_final_landscape <- data_siop_final2%>%
  mutate(id_original = "-",
         data_source = "siop_painel",
         year = Ano,
         project_name = acao,
         project_description = plano_orc,
         source_original = fonte_recursos) %>% left_join(source_landscape%>%select(fonte_recursos, source_finance_landscape, origin_domestic_international,origin_private_public),
                                                         by = "fonte_recursos") %>% 
  mutate(value_original_currency = Pago,
         original_currency = "BRL",
         channel_original = str_c(modalidade,und_orc,sep=";")) %>% left_join(channel_landscape %>% select(channel_original,channel_landscape),by = "channel_original") %>%
  mutate(instrument_original = grupo_de_despesa) %>% left_join(instrument_landscape, by ="instrument_original") %>% 
  mutate(sector_original = str_c(funcao,subfuncao,sep = ";")) %>% left_join(sector_landscape, by = c("acao" = "acao_des_limpa")) %>%
  mutate(subsector_original = programa)

#Filtro Climático
data_siop_final_landscape2 <- data_siop_final_landscape %>% left_join(climate_use %>% select(acao_des_limpa,rio_marker,climate_component ,activity_landscape ),by = c("acao"="acao_des_limpa")) 


# Filtro do Ajuste 5 (FOI APLICADO A AJUSTE 5 POIES ELE ESTA CERTO)
data_siop_final_landscape2 <- data_siop_final_landscape2 %>%
  mutate(rio_marker = case_when(plano_orc == "licenca, avaliacao , registro e autorizacoes ambientais" ~ 0.4,
                                plano_orc == "licenca, avaliacao, registro e autorizacoes ambientais" ~ 0.4,
                                plano_orc == "rastreio e controle de satelites" ~ 0.4,
                                plano_orc == "recepcao, armazenamento, processamento e distribuicao de dados de satelites"~0.4,
                                plano_orc == "apoio ao desenvolvimento sustentavel das cadeias produtivas agricolas"~0.4,
                                plano_orc == "fomento a indicacao geografica de produtos agropecuarios - ig"~0.4,
                                plano_orc == "funcionamento do conselho nacional de recursos hidricos"~0.4,
                                .default = rio_marker)) 
#Filtro de Ajuste 4 (FOI APLICADO O AJUSETE 4 POIS ELE ESTA CERTO TBM)
data_siop_final_landscape2 <- data_siop_final_landscape2 %>% 
  mutate(rio_marker = case_when(plano_orc=="desenvolvimento dos satelites da serie amazonia"~1,
                                plano_orc == "desenvolvimento dos satelites da serie amazonia - regra de ouro"~1,
                                .default = rio_marker)) 


# Aplicacao Rio Marker
data_siop_final_landscape2_metodologiaPadrao <- data_siop_final_landscape2 %>% mutate(value_original_currency = value_original_currency*rio_marker)
data_siop_final_landscape2_metodologiaPadrao$value_original_currency %>% sum #23.562.480.565
data_siop_final_landscape2_metodologiaPadrao %>% select(sector_landscape) %>% unique
# Pequena alteração nas classes dos dados para padronizar e Ajustar sectors faltantes
data_siop_final_landscape2_metodologiaPadrao <- data_siop_final_landscape2_metodologiaPadrao %>% mutate(channel_landscape = case_when(channel_landscape == "Civil society organizations" ~ "Civil society organization",
                                                                                      channel_landscape == "Financial Institutions"  ~"Financial Institution",
                                                                                      .default = channel_landscape),
                                                        origin_private_public = case_when(origin_private_public == "Other"~"Others",
                                                                                          .default = origin_private_public),
                                                        origin_domestic_international = case_when(origin_domestic_international == "Domestic" ~"National",
                                                                                                  .default =origin_domestic_international )) 



data_siop_final_landscape2_metodologiaPadrao <- data_siop_final_landscape2_metodologiaPadrao %>% mutate(sector_landscape = if_else(acao == "administracao da unidade" & und_orc == "servico florestal brasileiro - sfb",
                                                                                   true = "Forest",false = sector_landscape)) %>% 
  mutate(sector_landscape = if_else(acao == "administracao da unidade" & und_orc == "ministerio da ciencia, tecnologia e inovacao - administracao direta",
                                    true = "Multi-sector",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "administracao da unidade" & und_orc == "fundacao nacional do indio - funai",
                                    true = "Multi-sector",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "administracao da unidade" & und_orc == "ministerio do meio ambiente e mudanca do clima - administracao direta",
                                    true = "Multi-sector",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "administracao da unidade" & und_orc == "instituto brasileiro do meio ambiente e dos recursos naturais renovaveis - ibama",
                                    true = "Forest",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "administracao da unidade" & und_orc == "instituto de pesquisas jardim botanico do rio de janeiro - jbrj",
                                    true = "Forest",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "administracao da unidade" & und_orc == "instituto chico mendes de conservacao da biodiversidade",
                                    true = "Forest",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "apoio ao desenvolvimento de agricultura de baixa emissao de carbono - abc" & und_orc == "ministerio da agricultura e pecuaria - administracao direta",
                                    true = "Crop",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "avaliacao das medidas de ordenamento do uso de recursos pesqueiros" & und_orc == "instituto brasileiro do meio ambiente e dos recursos naturais renovaveis - ibama",
                                    true = "Cattle",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "avaliacao de periculosidade e controle de produtos, substancias quimicas e residuos perigosos" & und_orc == "instituto brasileiro do meio ambiente e dos recursos naturais renovaveis - ibama",
                                    true = "Forest",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "avaliacao dos estoques e do potencial sustentavel dos recursos pesqueiros" & und_orc == "instituto brasileiro do meio ambiente e dos recursos naturais renovaveis - ibama",
                                    true = "Forest",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "controle ambiental de produtos, substancias, residuos e atividades potencialmente poluidoras" & und_orc == "instituto brasileiro do meio ambiente e dos recursos naturais renovaveis - ibama",
                                    true = "Forest",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "desenvolvimento de sistemas espaciais" & plano_orc == "desenvolvimento e lancamento do veiculo lancador de microssatelites vlm-1",
                                    true = "Multi-sector",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "desenvolvimento de sistemas espaciais" & plano_orc == "desenvolvimento e lancamento do satelite sino-brasileiro cbers-4a",
                                    true = "Multi-sector",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "desenvolvimento de sistemas espaciais" & plano_orc == "desenvolvimento e lancamento de satelites cientificos",
                                    true = "Multi-sector",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "desenvolvimento de sistemas espaciais" & plano_orc == "desenvolvimento e lancamento do satelite sino-brasileiro cbers-4a - regra de ouro",
                                    true = "Multi-sector",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "desenvolvimento de sistemas espaciais" & plano_orc == "desenvolvimento dos satelites da serie amazonia",
                                    true = "Forest",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "desenvolvimento de sistemas espaciais" & plano_orc == "desenvolvimento dos satelites da serie amazonia - regra de ouro",
                                    true = "Forest",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "desenvolvimento e lancamento de foguetes suborbitais e de veiculos lancadores de satelites" & und_orc == "agencia espacial brasileira",
                                    true = "Multi-sector",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "desenvolvimento e lancamento de foguetes suborbitais e de veiculos lancadores de satelites, com a infraestrutura associada" & und_orc == "agencia espacial brasileira",
                                    true = "Multi-sector",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "desenvolvimento e lancamento de satelites" & plano_orc == "desenvolvimento dos satelites da serie amazonia",
                                    true = "Forest",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "desenvolvimento e lancamento de satelites" & plano_orc == "desenvolvimento dos satelites sino-brasileiros cbers-4",
                                    true = "Multi-sector",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "monitoramento hidroambiental nos reservatorios do dnocs" & und_orc == "departamento nacional de obras contra as secas - dnocs",
                                    true = "Multi-sector",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "monitoramento, avaliacao e controle de substancias, produtos quimicos e biologicos e de atividades potencialmente poluidoras e utilizadoras de recursos ambientais" & und_orc == "instituto brasileiro do meio ambiente e dos recursos naturais renovaveis - ibama",
                                    true = "Forest",false = sector_landscape))%>% 
mutate(sector_landscape = if_else(acao == "pesquisa e desenvolvimento no setor aeroespacial" & und_orc == "comando da aeronautica",
                                  true = "Multi-sector",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "pesquisa e desenvolvimento no setor aeroespacial" & und_orc == "fundo aeronautico",
                                    true = "Multi-sector",false = sector_landscape))%>% 
  mutate(sector_landscape = if_else(acao == "regulacao e fiscalizacao dos usos de recursos hidricos, dos servicos de irrigacao e aducao de agua bruta e da seguranca de barragens" & und_orc == "agencia nacional de aguas e saneamento basico - ana",
                                    true = "Multi-sector",false = sector_landscape))  %>% 
  mutate(sector_landscape=case_when(sector_landscape=="Agriculture" ~"Crop",
                                    .default = sector_landscape))
  


data_siop_final_landscape2_metodologiaPadrao_final<- data_siop_final_landscape2_metodologiaPadrao %>% left_join(beneficiary_landscape %>% distinct(acao_des_limpa,.keep_all = T) ,by=c("acao"="acao_des_limpa")) %>% 
  mutate(beneficiary_landscape = case_when(acao == "regularizacao da estrutura fundiaria na area de abrangencia da lei 11.952, de 2009"~"Smallholders",
                                           acao == "desenvolvimento economico e social dos produtores rurais"~"Rural Producers",
                                           acao == "abastecimento publico de agua em comunidades ribeirinhas dos rios sao francisco, do parnaiba, do itapecuru e do mearim. - agua para todos"~"Indigenous Peoples, Traditional Populations and Other Local Communities",
                                           acao == "manutencao de contrato de gestao com organizacoes sociais (lei nº 9.637, de 15 de maio de 1998)"~"Not Specified",
                                           acao == "gestao ambientalmente adequada de substancias quimicas"~"Not Specified",
                                           .default = beneficiary_landscape)) %>% 
  mutate(beneficiary_original = "-",
         beneficiary_public_private="-",
         localization_original = localizador,region = regiao,uf = uf,municipality=municipio,subactivity_landscape="-")
  
  


data_siop_final_landscape2_metodologiaPadrao_final$value_original_currency %>% sum
23.562.480.565
#Fazendo Deflacao
ibge_ipca <- read_excel("A:\\macro\\IPCA\\cleanData/ipca_ibge_cl.xlsx")
ibge_ipca <- ibge_ipca %>% 
  dplyr::mutate(variacao_doze_meses = suppressWarnings(as.numeric(variacao_doze_meses)))
deflator_automatico <- function(ano_ini, ano_fim, anos, base) {
  
  # Defina o seu projeto no Google Cloud, é importante criar o projeto e colar o id no "set_billing_id". Fora isso, nao funcionarah
  # Existe um bug no datalake da Base dos Dados que não permite o download direto.
  # set_billing_id("scenic-notch-360215")
  # 
  # # criacao do data frame direto da base de dados
  # serie_basedosdados <- basedosdados::bdplyr("br_ibge_ipca.mes_brasil") %>% bd_collect()
  
  serie_basedosdados <- base
  
  # selecao e filtros de valores de interesse ao calculo, queremos sempre a variacao anual, por isso o mes == 12
  serie_filtrada <- serie_basedosdados %>% 
    select(ano, mes, variacao_doze_meses) %>% 
    dplyr::filter(mes == 12,ano >= ano_ini & ano <= ano_fim ) %>% 
    dplyr::arrange(desc(ano))
  
  indice = 1
  
  #criacao do data frame para o deflator
  for (l in anos) {
    # chamei novamente a base feita pela funcao do api, pois a base precisa ser percorrida ano a ano e
    # se nao criarmos essa tabela, a tabela a ser percorrida novamente terá sempre o ano inicial como observacao
    tabela <- serie_filtrada 
    
    tabela <- tabela %>% 
      dplyr::filter(ano == l)
    
    if (l == ano_fim) {
      tabela <- tabela %>%  dplyr::mutate(deflator = indice)
      tabela_final <- tabela
      indice_ano_anterior = indice * (1+ (tabela$variacao_doze_meses/100))
    } else {
      tabela <- tabela %>% dplyr::mutate(deflator = indice_ano_anterior)
      
      tabela_final <- rbind(tabela_final, tabela)
      indice_ano_anterior = indice_ano_anterior * (1 + (tabela$variacao_doze_meses/100))
    }
  }
  tabela_final <- tabela_final %>% 
    select(ano, deflator) %>%
    dplyr::rename(year = ano) %>% 
    dplyr::arrange(year)
  return(tabela_final)
}

ano_ini = 2015
ano_fim = 2023
anos = seq(ano_fim,ano_ini, -1)
teste <- deflator_automatico(ano_ini, ano_fim, anos,ibge_ipca)
base_select_deflator <- data_siop_final_landscape2_metodologiaPadrao_final %>% 
  left_join(teste, by= "year")%>%
  dplyr::mutate(value_brl_deflated = value_original_currency * deflator)
base_select_deflator$value_brl_deflated %>% sum
# Fazendo a dolarizacao
coleta_dados_sgs = function(series,datainicial="01/01/2012", datafinal = format(Sys.time(), "%d/%m/%Y")){
  #Argumentos: vetor de séries, datainicial que pode ser manualmente alterada e datafinal que automaticamente usa a data de hoje
  #Cria estrutura de repetição para percorrer vetor com códigos de séries e depois juntar todas em um único dataframe
  for (i in 1:length(series)){
    dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",series[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
    dados[,-1] = as.numeric(gsub(",",".",dados[,-1])) #As colunas do dataframe em objetos numéricos exceto a da data
    nome_coluna = series[i] #Nomeia cada coluna do dataframe com o código da série
    colnames(dados) = c('data', nome_coluna)
    nome_arquivo = paste("dados", i, sep = "") #Nomeia os vários arquivos intermediários que são criados com cada série
    assign(nome_arquivo, dados)
    
    if(i==1)
      base = dados1 #Primeira repetição cria o dataframe
    else
      base = merge(base, dados, by = "data", all = T) #Demais repetições agregam colunas ao dataframe criado
    print(paste(i, length(series), sep = '/')) #Printa o progresso da repetição
  }
  
  base$data = as.Date(base$data, "%d/%m/%Y")
  base$ano = as.integer(format(base$data,"%Y"))#Transforma coluna de data no formato de data
  base = base[order(base$data),] #Ordena o dataframe de acordo com a data
  
  base <- base %>% select(ano, `3694`)
  base <- base %>% dplyr::rename(c(year = ano, cambio = `3694`))
  return(base)
}
cambio_sgs = coleta_dados_sgs(3694) 
tabela_cambio <-cambio_sgs %>% 
  dplyr::filter(year >= 2015 & year <= 2023)

base_select_deflator = base_select_deflator %>% 
  left_join(tabela_cambio,by='year') %>% 
  dplyr::mutate(value_usd = value_brl_deflated/cambio)

# Finalizando e salvando
base_select_deflator<- base_select_deflator %>% select(id_original,data_source,year,project_name,project_description,source_original,
                                source_finance_landscape,origin_domestic_international,origin_private_public,
                                value_original_currency,original_currency,value_brl_deflated,value_usd,channel_original,
                                channel_landscape,instrument_original,instrument_landscape,sector_original,sector_landscape,
                                subsector_original,activity_landscape,subactivity_landscape,climate_component,rio_marker,
                                beneficiary_original,beneficiary_landscape,beneficiary_public_private,localization_original,region,
                                uf,municipality)







######### A partir daqui é aplicação de Dicionario de dados ##################################################################################

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
#crop_siop_filtrado %>% inner_join(bioenergia_siop_filtrado,by="Coluna_search")

multisector_siop <- multisector_search_pattern_SIOP(data_frame_SIOP=siop_landscape ,Coluna_search = Coluna_search) 
multisector_siop%>%select(project_name,project_description,sector_original,subsector_original,channel_original,source_original,Coluna_search)%>% unique %>% view
multisector_siop_filtrado <- multisector_out_SIOP (data_frame_SIOP_multisector = multisector_siop,Coluna_search = Coluna_search)
multisector_siop_filtrado$sector_landscape = "Multisector"
#multisector_siop_filtrado %>% inner_join(bioenergia_siop_filtrado,by="Coluna_search")

forest_siop <- forest_search_pattern_SIOP(data_frame_SIOP = siop_landscape,Coluna_search =Coluna_search )
forest_siop%>%select(project_name,project_description,sector_original,subsector_original,channel_original,source_original,Coluna_search)%>% unique %>% view
forest_siop_filtrado <- forest_out_SIOP(data_frame_SIOP_forest = forest_siop,Coluna_search = Coluna_search)
forest_siop_filtrado%>%select(project_name,project_description,sector_original,subsector_original,channel_original,source_original,Coluna_search)%>% unique %>% view
forest_siop_filtrado$sector_landscape = "Forest"
forest_siop_filtrado %>% inner_join(bioenergia_siop_filtrado,by="Coluna_search")

siop_sectorlandscape <- rbind(bioenergia_siop_filtrado,crop_siop_filtrado,multisector_siop_filtrado,forest_siop_filtrado)

siop_resto_noSector <- siop_landscape %>% filter(!Coluna_search %in% siop_sectorlandscape$Coluna_search)

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
subactivity_landscape = if_else(grepl("\\bibama\\b", x = Coluna_search , ignore.case = TRUE), true= "Ibama", false = if_else(grepl("\\bchico\\b", x = Coluna_search , ignore.case = TRUE), true = "ICMBIO",false = if_else(grepl("\\bfunai\\b", x = Coluna_search , ignore.case = TRUE), true= "Funai", false = if_else(grepl("\\bsfb", x = Coluna_search , ignore.case = TRUE),true = "SFB",false="SemClassificacao")))),
climate_component = "Mitigação"
) 
siop_mitigacao_folha_pagamento%>%filter(is.na(subactivity_landscape))%>%view
siop_mitigacao_folha_pagamento %>% select(subactivity_landscape)%>%unique%>%view
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

# Dando bind nas linhas que tiveram info de sector e climate use
siop_landscape_climate_use_bind <- bind_rows(filtro_11,siop_landscape_climate_use)
siop_landscape_climate_use_bind %>% filter(subactivity_landscape == "NA")%>%unique%>%view
siop_landscape_climate_use_bind %>% filter(subactivity_landscape == "NA")%>%select(activity_landscape)%>%unique%>%view
# Dando continuidade ao landscape
siop_landscape_climate_use_bind <- siop_landscape_climate_use_bind %>% mutate(localization_original = localizador,region = regiao,uf = uf,municipality = municipio)
siop_landscape_climate_use_bind %>% select(
  all_of(c("id_original","data_source","year","project_name","project_description","source_original","source_of_finance_landscape",
  "domestic_internacional","source_private_public","original_currency","channel_original","channel_landscape","instrument_original",
  "instrument_landscape","sector_original","sector_landscape","subsector_original","activity_landscape","subactivity_landscape","climate_component","localization_original","region",
  "uf","municipality", "Coluna_search","Pago")) 
)  %>% write.xlsx("A:/projects/landuse_br2024/SIOP/Siop_Landscape_ClimateUse_18_03_2024.xlsx")

siop_landscape_climate_use_bind %>% select(
  all_of(c("id_original","data_source","year","project_name","project_description","source_original","source_of_finance_landscape",
  "domestic_internacional","source_private_public","original_currency","channel_original","channel_landscape","instrument_original",
  "instrument_landscape","sector_original","sector_landscape","subsector_original","activity_landscape","subactivity_landscape","climate_component","localization_original","region",
  "uf","municipality", "Coluna_search","Pago")) )%>% select(project_name ,project_description,channel_original,activity_landscape,subactivity_landscape) %>% unique %>% view
  
%>% write.xlsx("PesquisaDesenvolvimentoDifusaoConhecimentoMCTI.xlsx")


#siop_landscape_climate_use_bind %>% select(beneficiary_original,beneficiary_landscape) %>% unique %>% write.xlsx("Beneficiario.xlsx")

# Unindo os que nao tiveram
siop_rest <- bind_rows(siop_resto_noSector %>% mutate(sector_landscape = "Sem Classificacao de Setor"),siop_sectorlandscape)
siop_rest %>% select(project_name,project_description,sector_original,subsector_original,channel_original,source_original,sector_landscape,Coluna_search) %>% unique  %>% write.xlsx("Siop_Resto.xlsx")
siop_rest %>% group_by(sector_landscape) %>% count()

siop_rest%>% select(
  all_of(c("id_original","data_source","year","project_name","project_description","source_original","source_of_finance_landscape",
  "domestic_internacional","source_private_public","original_currency","channel_original","channel_landscape","instrument_original",
  "instrument_landscape","sector_original","sector_landscape","subsector_original",
   "Coluna_search","Pago")) 
)%>% write.xlsx("A:/projects/landuse_br2024/SIOP/Siop_Resto_18_03_2024.xlsx")
#Criando o resumo de dados para o SIOP
create_dict(siop_tratado, write_excel = TRUE, file_name = "Descricao_Dados_SIOP_18_03_24_v2")
yaImpute
siop_tratado%>%names





# Fazendo o plot
last_landscape <- read_rds("./brlanduse_landscape2024_dados/Dict/base_landscape_final_01022024.rds")
last_landscape <- last_landscape %>% mutate(sector_landscape= case_when(
  sector_landscape == "crop" ~ "Crop",sector_landscape == "forest" ~ "Forest", sector_landscape=="cattle" ~ "Cattle",
  sector_landscape == "Bioenergy and fuels" | sector_landscape == "Bioenergy And Fuels" ~ "Bioenergy and Fuels",sector_landscape == "Agriculture" ~ "Crop",.default = sector_landscape
))
siop_antigo <- last_landscape %>% filter(data_source=="siop_painel")
climate_use <- read_excel("./brlanduse_landscape2024_dados/SIOP/12_siop_relational_tables.xlsx",sheet = "climate_use")
climate_use <- climate_use %>% mutate(plano_orc = str_trim(str_remove(str_remove(str_remove(str_to_lower(stri_trans_general(plano_orc,"Latin-ASCII")),"'"),"^[[:alnum:]]{4}"),"-")),
                      acao = str_trim(str_remove(str_remove(str_remove(str_to_lower(stri_trans_general(acao,"Latin-ASCII")),"'"),"^[[:alnum:]]{4}"),"-")))
climate_use <- climate_use%>%mutate(key_join = str_c(plano_orc,acao,sep = "_"))
siop_antigo <- siop_antigo%>%mutate(key_join = str_c(project_description,project_name,sep = "_"))%>%mutate(key_join=str_remove(key_join,"-"))

siop_antigo_climate <- siop_antigo %>% left_join(climate_use%>% select(key_join,rio_marker,beneficiary_landscape,climate_component)%>% distinct(key_join, .keep_all = TRUE),by = "key_join") 
siop_antigo_climate <- siop_antigo_climate %>% mutate(climate_component.y = if_else(is.na(climate_component.y),true = "Mitigation",
false = climate_component.y))
ano_siop_antigo_pago <- siop_antigo_climate %>% group_by(climate_component.y,year) %>% summarise(Soma_Dinheiro = sum(value_original_currency)) 
ano_siop_antigo_pago_wider <- ano_siop_antigo_pago %>% pivot_wider(names_from = year, values_from = Soma_Dinheiro) 

siop_atual <- read_excel("./brlanduse_landscape2024_dados/SIOP/Siop_Landscape_ClimateUse.xlsx")
siop_atual <- siop_atual%>% select(climate_component,year,Pago)
siop_atual %>% group_by(climate_component,year) %>% summarise(SumPago = sum(Pago))

#Fazendo Deflacao
ibge_ipca <- read_excel("./brlanduse_landscape2024_dados/ipca_ibge_cl.xlsx")
ibge_ipca <- ibge_ipca %>% 
  mutate(variacao_doze_meses = suppressWarnings(as.numeric(variacao_doze_meses)))
deflator_automatico <- function(ano_ini, ano_fim, anos, base) {
  
  # Defina o seu projeto no Google Cloud, é importante criar o projeto e colar o id no "set_billing_id". Fora isso, nao funcionarah
  # Existe um bug no datalake da Base dos Dados que não permite o download direto.
  # set_billing_id("scenic-notch-360215")
  # 
  # # criacao do data frame direto da base de dados
  # serie_basedosdados <- basedosdados::bdplyr("br_ibge_ipca.mes_brasil") %>% bd_collect()
  
  serie_basedosdados <- base
  
  # selecao e filtros de valores de interesse ao calculo, queremos sempre a variacao anual, por isso o mes == 12
  serie_filtrada <- serie_basedosdados %>% 
    select(ano, mes, variacao_doze_meses) %>% 
    filter(mes == 12,ano >= ano_ini & ano <= ano_fim ) %>% 
    arrange(desc(ano))
  
  indice = 1
  
  #criacao do data frame para o deflator
  for (l in anos) {
    # chamei novamente a base feita pela funcao do api, pois a base precisa ser percorrida ano a ano e
    # se nao criarmos essa tabela, a tabela a ser percorrida novamente terá sempre o ano inicial como observacao
    tabela <- serie_filtrada 
    
    tabela <- tabela %>% 
    filter(ano == l)
    
    if (l == ano_fim) {
      tabela <- tabela %>%  mutate(deflator = indice)
      tabela_final <- tabela
      indice_ano_anterior = indice * (1+ (tabela$variacao_doze_meses/100))
    } else {
      tabela <- tabela %>% mutate(deflator = indice_ano_anterior)
      
      tabela_final <- rbind(tabela_final, tabela)
      indice_ano_anterior = indice_ano_anterior * (1 + (tabela$variacao_doze_meses/100))
    }
  }
  tabela_final <- tabela_final %>% 
    select(ano, deflator) %>%
    dplyr::rename(year = ano) %>% 
    arrange(year)
  return(tabela_final)
}

ano_ini = 2021
ano_fim = 2023
anos = seq(ano_fim,ano_ini, -1)
teste <- deflator_automatico(ano_ini, ano_fim, anos,ibge_ipca)
base_select_deflator <- siop_atual %>% 
    left_join(teste, by= "year")%>%
    mutate(value_brl_deflated = as.numeric(Pago * deflator))
base_select_deflator%>% view
base_select_deflator <- base_select_deflator%>% group_by(climate_component,year) %>% summarise(SumPago = sum(value_brl_deflated))
base_select_deflator %>% pivot_wider(names_from = year, values_from = SumPago) 




siop_antigo_climate %>% view
last_landscape %>% names
siop_antigo_climate <- siop_antigo_climate %>% select(id_original,data_source,year,project_name,project_description,source_original,
          source_finance_landscape,origin_domestic_international,origin_private_public,value_original_currency,original_currency,value_brl_deflated,value_usd,
              channel_original,channel_landscape,instrument_original,instrument_landscape,sector_original,sector_landscape,
              subsector_original,activity_landscape,subactivity_landscape,beneficiary_original,beneficiary_public_private,localization_original,
              region,uf,municipality,value_brl_deflated_mean,value_usd_mean,rio_marker.y,beneficiary_landscape.y,climate_component.y)
siop_antigo_climate <- siop_antigo_climate %>% rename(rio_marker = rio_marker.y, beneficiary_landscape = beneficiary_landscape.y,climate_component = climate_component.y ) 

siop_antigo_climate%>% view
siop_antigo_climate <- siop_antigo_climate %>% mutate(origin_domestic_international =case_when(origin_domestic_international == "Doméstico" ~ "Domestic",
                                                                          origin_domestic_international == "Internacional" ~ "International",.default = origin_domestic_international),
                                                                          origin_private_public = case_when(origin_private_public == "Outros" ~ "Others",
                                                                          origin_private_public == "Pública" ~ "Public",
                                                                          .default = origin_private_public),
                                                                          channel_landscape = case_when(
                                                                            channel_landscape == "Órgãos Governamentais" ~ "Government agencies",
                                                                            channel_landscape == "OSCFLs" ~ "Civil society organizations",
                                                                            channel_landscape == "Instituições Financeiras" ~ "Corporations",.default = channel_landscape
                                                                          ),
                                                                          instrument_landscape = case_when(instrument_landscape=="Orçamento Público" ~ "Public Budget",
                                                                          .default ="Public Budget" ))

siop_antigo_climate %>% write_rds("Siop_Revisado_LandScape_15_20.rds")
siop_antigo_climate%>% view





channel_landscape %>% filter(is.na(und_orc))



library(tidyverse)
library(tidytext)
library(stopwords)
library(openxlsx)
library(readxl)
library(stringi)
####################################################################################################
# Script para criar um dicionário de palavras para as bases de dados do BNDES,SIOP, OCDE,NINT e FNMC#
# Vamos criar um dicionario especifico para cada base contendo as palavras de cada setor landscape  #
#####################################################################################################
# Lendo o RDS do ultimo landscape criado
last_landscape <- read_rds("./brlanduse_landscape2024_dados/Dict/base_landscape_final_01022024.rds")
last_landscape <- last_landscape %>% mutate(sector_landscape= case_when(
  sector_landscape == "crop" ~ "Crop",sector_landscape == "forest" ~ "Forest", sector_landscape=="cattle" ~ "Cattle",
  sector_landscape == "Bioenergy and fuels" | sector_landscape == "Bioenergy And Fuels" ~ "Bioenergy and Fuels",sector_landscape == "Agriculture" ~ "Crop",.default = sector_landscape
))
# Criando stopwords
pt_stop <- stopwords('pt')%>%
  as_tibble
##################################################################################################################################
########################################################## Funções ###############################################################
get_ngrama <- function(data_frame,n_value){
  ngrama <- data_frame %>% 
  unnest_tokens(output = value,
                input =Palavras,token="ngrams", n =n_value )%>% 
  anti_join( pt_stop )%>% 
  mutate(value = if_else(str_length(value)<=2,#removendo palavras pequenas
                            true=NA,
                            false=value))%>%  mutate(value = str_replace_all(string= value,#removemndo os numeros
                                    pattern="[:digit:]",
                                    replacement=''))%>%
                                    filter(!is.na(value)) %>%as_tibble
  ngrama <- ngrama%>% dplyr::count(value,sort=T)     
                             
  return(ngrama)
}

get_word_keys <- function(sector, columns_keep, data_frame_){
  data_frame_sector <- data_frame_ %>% filter(sector_landscape == sector)%>%select(all_of(columns_keep))
  data_frame_1gram <- data_frame_sector%>%select(columns_keep[1:3])
  data_frame_1gram_tibble <- tibble(Palavras = unlist(data_frame_1gram,use.names=FALSE))
  data_frame_3gram <- data_frame_sector%>%select(columns_keep[4])
  data_frame_3gram_tibble <- tibble(Palavras = unlist(data_frame_3gram,use.names=FALSE))
  data_frame_dict_1word <- get_ngrama(data_frame =data_frame_1gram_tibble,n_value = 1 )
  data_frame_dict_3word <- get_ngrama(data_frame =data_frame_3gram_tibble,n_value = 3 )
  data_frame_dict_1word$Sector = sector
  data_frame_dict_3word$Sector = sector
  data_frame_allWords <- rbind(data_frame_dict_1word,data_frame_dict_3word)
  return(data_frame_allWords)
  
}
##################################################################################################################################
##################################################################################################################################

# Para o BNDES n aut:

bndes_nauT <- last_landscape%>%filter(data_source == "bndes_naut")%>%as_tibble()

bndes_wordKeys_bioenergia <- get_word_keys(sector = "Bioenergy and Fuels", columns_keep = c("sector_original","subsector_original","project_description","source_original"),data_frame_ = bndes_nauT)
bndes_wordKeys_crop <- get_word_keys(sector = "Crop", columns_keep = c("sector_original","subsector_original","project_description","source_original"),data_frame_ = bndes_nauT)
bndes_wordKeys_forest <- get_word_keys(sector = "Forest", columns_keep = c("sector_original","subsector_original","project_description","source_original"),data_frame_ = bndes_nauT)
bndes_wordKeys_cattle <- get_word_keys(sector = "Cattle", columns_keep = c("sector_original","subsector_original","project_description","source_original"),data_frame_ = bndes_nauT)
# Unindo todo mundo e aplicando um pivot_wider:
bndes_all_words <- rbind(bndes_wordKeys_bioenergia,bndes_wordKeys_crop,bndes_wordKeys_forest,bndes_wordKeys_cattle)
bndes_all_words <- bndes_all_words%>%mutate(value = str_trim(str_to_lower(stri_trans_general(value,"Latin-ASCII"))))
bndes_all_words_landuse <- bndes_all_words%>%pivot_wider(names_from="Sector",values_from="n")
# Salvando
bndes_all_words_landuse%>%write.xlsx('Dicionario_BNDES_Naut.xlsx')
##################################################################################################################################
##################################################################################################################################
# Para o SIOP (orçamento publico)
siop <- last_landscape%>%filter(data_source=="siop_painel")%>%as_tibble

siop_wordKeys_bioenergia <- get_word_keys(sector="Bioenergy and Fuels", columns_keep =c("project_name","project_description","sector_original","subsector_original","channel_original","source_original"),data_frame_ =siop)
siop_wordKeys_crop <- get_word_keys(sector="Crop", columns_keep =c("project_name","project_description","sector_original","subsector_original","channel_original","source_original"),data_frame_ =siop)
siop_wordKeys_forest <- get_word_keys(sector="Forest", columns_keep =c("project_name","project_description","sector_original","subsector_original","channel_original","source_original"),data_frame_ =siop)
siop_wordKeys_cattle <- get_word_keys(sector="Cattle", columns_keep =c("project_name","project_description","sector_original","subsector_original","channel_original","source_original"),data_frame_ =siop)
siop_wordKeys_multisector <- get_word_keys(sector="Multi-sector", columns_keep =c("project_name","project_description","sector_original","subsector_original","channel_original","source_original"),data_frame_ =siop)
# Unindo todo mundo e aplicando um pivot_wider:
siop_all_words <- rbind(siop_wordKeys_bioenergia,siop_wordKeys_crop,siop_wordKeys_forest,siop_wordKeys_cattle,siop_wordKeys_multisector)
siop_all_words_landuse <- siop_all_words%>%pivot_wider(names_from="Sector",values_from="n")
siop_all_words_landuse%>%write.xlsx('C:/Users/napcc/Dropbox (CPI)/EduardoMinsky/PARAMIM/brlanduse_landscape2024_dados/SIOP/Dicionario_SIOP.xlsx')

siop%>%filter(sector_landscape=="Bioenergy and Fuels")%>%select("project_name","project_description","sector_original","subsector_original","channel_original","source_original","sector_landscape")%>%unique%>%view
siop%>%filter(sector_landscape=="Crop")%>%select("project_name","project_description","sector_original","subsector_original","channel_original","source_original","sector_landscape")%>%unique%>%view
siop%>%filter(sector_landscape=="Forest")%>%select("project_name","project_description","sector_original","subsector_original","channel_original","source_original","sector_landscape")%>%unique%>%view
siop%>%filter(sector_landscape=="Multi-sector")%>%select("project_name","project_description","sector_original","subsector_original","channel_original","source_original","sector_landscape")%>%unique%>%view

siop = siop%>%mutate(Coluna_search = str_c(project_name,project_description,sector_original,subsector_original,channel_original,source_original,sector_landscape, sep = ";"))
source("C:/Users/eduar/Dropbox (CPI)/EduardoMinsky/PARAMIM/landuse_br2024/AuxFolder/Dictionary_Sectors.R")
asd <- crop_search_pattern_SIOP (data_frame_SIOP = siop,Coluna_search)

asd2 <- asd %>%select(project_name,project_description,sector_original,subsector_original,channel_original,source_original,sector_landscape,channel_landscape,Coluna_search)%>%unique
asd2%>%filter(sector_landscape=="Bioenergy and Fuels") %>%view

Coluna_search="regularizacao ambiental e fundiaria de projetos publicos de irrigacao;regularizacao ambiental e fundiaria de projetos publicos de irrigacao;agricultura;irrigacao;agropecuaria sustentavel;aplicacoes diretas;companhia de desenvolvimento dos vales do sao francisco e do parnaiba - codevasf;recursos primarios de livre aplicacao;Forest"

(grepl("\\breducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\briscos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bestudos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bimplementacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmanutencao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bzoneamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricola\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brisco\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bclimatico\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpromocao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\abastecimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcomercializacao\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bassistencia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bextensao rural\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutor\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brural\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("agricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bextensao rural\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\breforma\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bampliacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\blaboratorios\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuarios\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdefesa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdivulgacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binformacoes\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmeteorologicas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bclimatologicas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmeteorologia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\babastecimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcomercializacao\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bministerio da agricultura e pecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpromocao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfamiliar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgestao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brisco\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura familiar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\borganizacao agraria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bextensao rural\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfortalecimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdinamizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura familiar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento agrario\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura familiar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdigitalizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bacervo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bhistorico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmeteorologicos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfomento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\btecnologia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprotecao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcultivares\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\buso sustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brecursos geneticos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binovacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bapoio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bterritorios rurais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) &grepl("\\bterritorial\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brural\\b" , x = Coluna_search , ignore.case = TRUE) & grepl("\\bcombate\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpobreza\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfamiliar\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagrario\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bmulheres\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brurais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura familiar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagroalimentar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpos colheita\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bavaliacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsafras\\b", x = Coluna_search , ignore.case = TRUE)) |
        
        (grepl("\\bcompanhia nacional de abastecimento\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bembrapa\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\btecnologico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bengenharia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\binovacoes\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\btransferencia\\b" , x = Coluna_search , ignore.case = TRUE) & grepl("\\btecnologias\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\btecnologias\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bunidades\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\breferencia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\btecnologica\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdifusao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bconhecimento\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcensos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdemografico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuario\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcombate\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpobreza\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsemiarido\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfomento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfamiliar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcarbono\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpreservacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bambiental\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfiscalizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\batividades\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuarias\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfiscalizacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfertilizantes\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("defesa agropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |        
        (grepl("\\bagroecologia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsociobiodiversidade\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\borganica\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brural\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brurais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfamiliar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcontrole\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\borganica\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpro-organico\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfomento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsetor\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuario\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfomento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuarios\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpromocao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcadeias\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutivas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcadeias\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutivas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricolas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagroambientais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bzoneamento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bambiental\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutivo\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagroecossistemas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsistemas produtivos rurais sustentaveis\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bp&d\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcompetitividade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentabilidade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcadeias\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpd\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcompetitividade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentabilidade\\b" , x = Coluna_search , ignore.case = TRUE) & grepl("\\bcadeias\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpesqueira\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\baquicola\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\barranjos produtivos locais\\b", x = Coluna_search , ignore.case = TRUE)) |
        
        (grepl("\\bcodevasf\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcacau\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprodutoras de cacau\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsistemas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagroflorestais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcacaueiras\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bbrasil\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmiseria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bextensao rural\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcafeicultura\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprodutores\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagronegocio\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcafe\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfundo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bdefesa\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcafeeira\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprodutoras\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcacau\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagroflorestais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bproducao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgeocientifica\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bpesquisas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuarias\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpesquisas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuarias\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcenso\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuario\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bcidadania\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmulheres\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brurais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\btrabalhadora\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brural\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagrario\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bestruturacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutiva\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bfamiliar\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bpequenos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutores rurais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bmedios\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bprodutores rurais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\brural\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsemiarido\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfundo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgarantiasafra\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfundo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsafra\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bfundo\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgarantia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsafra\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bgarantia-safra\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bmonitoramento\\b", x = Coluna_search , ignore.case = TRUE)  & grepl("\\bmeteorologico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bmonitoramento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bmeteorologico\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bquarentena vegetal\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bquilombolas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bindigenas\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\btradicionais\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bdesenvolvimento\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bcomunidades\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bindigenas\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bconservacionista\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\binterpretacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\binformacoes\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bsolos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\brecursos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgeneticos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsetor\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bbioinsumos\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bsustentavel\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgeneticos\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\balimentacao\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagricultura\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bbioeconomia\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bproagro\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bprograma\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bgarantia\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\batividade\\b", x = Coluna_search , ignore.case = TRUE) & grepl("\\bagropecuaria\\b", x = Coluna_search , ignore.case = TRUE)) |
        (grepl("\\bPROAGRO\\b", x = Coluna_search , ignore.case = TRUE))
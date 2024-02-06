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

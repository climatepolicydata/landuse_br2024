library(tidyverse)
library(tidytext)
library(stopwords)
library(openxlsx)
library(readxl)
library(stringi)
# Lendo o RDS do ultimo landscape criado
last_landscape <- read_rds("C:/Users/napcc/Dropbox (CPI)/EduardoMinsky/PARAMIM/brlanduse_landscape2024/AuxFolder/Dict/base_landscape_final_01022024.rds")

last_landscape%>%select(sector_landscape)%>%unique%>%view
last_landscape%>%filter(sector_landscape == "Livestock")%>%select(data_source)%>%unique
# instrument_original
# sector_original
# subsector_original
# project_description

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
# Criando stopwords
pt_stop <- stopwords('pt')%>%
  as_tibble
# Separando para os setores landscape
pecuaria <- last_landscape%>%filter(sector_landscape == "Pecuária")%>%select(sector_original,subsector_original,instrument_original)%>%
                        mutate(sector_original =str_trim(str_to_lower(stri_trans_general(sector_original,"Latin-ASCII"))),
                         subsector_original = str_trim(str_to_lower(stri_trans_general(subsector_original,"Latin-ASCII"))),
                         instrument_original =str_trim(str_to_lower(stri_trans_general(instrument_original,"Latin-ASCII"))) )

multisetorial <- last_landscape%>%filter(sector_landscape == "Multisetorial")%>%select(sector_original,subsector_original,instrument_original)%>%
                        mutate(sector_original =str_trim(str_to_lower(stri_trans_general(sector_original,"Latin-ASCII"))),
                         subsector_original = str_trim(str_to_lower(stri_trans_general(subsector_original,"Latin-ASCII"))),
                         instrument_original =str_trim(str_to_lower(stri_trans_general(instrument_original,"Latin-ASCII"))) )

conservacao_rest_reflora <- last_landscape%>%filter(sector_landscape == "Conservação, Restauração e Reflorestamento")%>%select(sector_original,subsector_original,instrument_original)%>%
                        mutate(sector_original =str_trim(str_to_lower(stri_trans_general(sector_original,"Latin-ASCII"))),
                         subsector_original = str_trim(str_to_lower(stri_trans_general(subsector_original,"Latin-ASCII"))),
                         instrument_original =str_trim(str_to_lower(stri_trans_general(instrument_original,"Latin-ASCII"))) )

bio_energ_comb <- last_landscape%>%filter(sector_landscape == "Bioenergia e Combustíveis")%>%select(sector_original,subsector_original,instrument_original)%>%
                        mutate(sector_original =str_trim(str_to_lower(stri_trans_general(sector_original,"Latin-ASCII"))),
                         subsector_original = str_trim(str_to_lower(stri_trans_general(subsector_original,"Latin-ASCII"))),
                         instrument_original =str_trim(str_to_lower(stri_trans_general(instrument_original,"Latin-ASCII"))) )

agricultura <- last_landscape%>%filter(sector_landscape == "Agricultura")%>%select(sector_original,subsector_original,instrument_original)%>%
                        mutate(sector_original =str_trim(str_to_lower(stri_trans_general(sector_original,"Latin-ASCII"))),
                         subsector_original = str_trim(str_to_lower(stri_trans_general(subsector_original,"Latin-ASCII"))),
                         instrument_original =str_trim(str_to_lower(stri_trans_general(instrument_original,"Latin-ASCII"))) )

#Criando o dicionario individual para cada setor landscape
# Pecuaria
Palavras_pecuaria<-tibble(Palavras = c(pecuaria$sector_original,pecuaria$subsector_original))
Palavras_pecuaria <- get_ngrama(Palavras_pecuaria,n_value = 1)
Palavras_pecuaria_inst_fin <- tibble(Palavras = c(pecuaria$instrument_original))
Palavras_pecuaria_inst_fin <- get_ngrama(Palavras_pecuaria_inst_fin,n_value = 3)
Palavras_pecuaria_all <- rbind(Palavras_pecuaria,Palavras_pecuaria_inst_fin)
Palavras_pecuaria_all$Sector_Landscape = "Pecuaria"

# multisetorial
Palavras_multisetorial<-tibble(Palavras = c(multisetorial$sector_original,multisetorial$subsector_original))
Palavras_multisetorial <- get_ngrama(Palavras_multisetorial,n_value = 1)
Palavras_multisetorial_inst_fin <- tibble(Palavras = c(multisetorial$instrument_original))
Palavras_multisetorial_inst_fin <- get_ngrama(Palavras_multisetorial_inst_fin,n_value = 3)
Palavras_multisetorial_all <- rbind(Palavras_multisetorial,Palavras_multisetorial_inst_fin)
Palavras_multisetorial_all$Sector_Landscape = "Multisetorial"

# conservacao_rest_reflora
Palavras_conservacao_rest_reflora<-tibble(Palavras = c(conservacao_rest_reflora$sector_original,conservacao_rest_reflora$subsector_original))
Palavras_conservacao_rest_reflora <- get_ngrama(Palavras_conservacao_rest_reflora,n_value = 1)
Palavras_conservacao_rest_reflora_inst_fin <- tibble(Palavras = c(conservacao_rest_reflora$instrument_original))
Palavras_conservacao_rest_reflora_inst_fin <- get_ngrama(Palavras_conservacao_rest_reflora_inst_fin,n_value = 3)
Palavras_conservacao_rest_reflora_all <- rbind(Palavras_conservacao_rest_reflora,Palavras_conservacao_rest_reflora_inst_fin)
Palavras_conservacao_rest_reflora_all$Sector_Landscape = "conservacao_rest_reflora"

# bio_energ_comb
Palavras_bio_energ_comb<-tibble(Palavras = c(bio_energ_comb$sector_original, bio_energ_comb$subsector_original))
Palavras_bio_energ_comb <- get_ngrama(Palavras_bio_energ_comb,n_value = 1)
Palavras_bio_energ_comb_inst_fin <- tibble(Palavras = c(bio_energ_comb$instrument_original))
Palavras_bio_energ_comb_inst_fin <- get_ngrama(Palavras_bio_energ_comb_inst_fin,n_value = 3)
Palavras_bio_energ_comb_all <- rbind(Palavras_bio_energ_comb,Palavras_bio_energ_comb_inst_fin)
Palavras_bio_energ_comb_all$Sector_Landscape = "bio_energ_comb"

# agricultura
Palavras_agricultura<-tibble(Palavras = c(agricultura$sector_original, agricultura$subsector_original))
Palavras_agricultura <- get_ngrama(Palavras_agricultura,n_value = 1)
Palavras_agricultura_inst_fin <- tibble(Palavras = c(agricultura$instrument_original))
Palavras_agricultura_inst_fin <- get_ngrama(Palavras_agricultura_inst_fin,n_value = 3)
Palavras_agricultura_all <- rbind(Palavras_agricultura,Palavras_agricultura_inst_fin)
Palavras_agricultura_all$Sector_Landscape = "agricultura"

# Juntando todas as tibbles em um pivot wider
dicionario_palavras_SectorLandscape<- rbind(Palavras_pecuaria_all,Palavras_multisetorial_all,
                                            Palavras_conservacao_rest_reflora_all,Palavras_bio_energ_comb_all,Palavras_agricultura_all)%>%pivot_wider(names_from = "Sector_Landscape", values_from ="n" )

dicionario_palavras_SectorLandscape%>%write.xlsx('Dicionarios_palavras_Sector_landscape.xlsx')
getwd()

 # Criando a mesma contagem de palavras mas por TF-IDF
tf_idf <- rbind(Palavras_pecuaria_all,Palavras_multisetorial_all,
                                            Palavras_conservacao_rest_reflora_all,Palavras_bio_energ_comb_all,Palavras_agricultura_all)

tf_idf_total_words <- tf_idf%>% 
  group_by(Sector_Landscape) %>% 
  summarize(total = sum(n))
tf_idf <- tf_idf %>% left_join(tf_idf_total_words)

freq_by_rank <- tf_idf %>% 
  group_by(Sector_Landscape) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total) %>%
  ungroup()

  
sector_tf_idf <- tf_idf %>%
  bind_tf_idf(value, Sector_Landscape, n)

library(forcats)

sector_tf_idf %>%
  group_by(Sector_Landscape) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(value, tf_idf), fill = Sector_Landscape)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Sector_Landscape, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)
  
sector_tf_idf%>%select(value,Sector_Landscape,tf_idf)%>%pivot_wider(names_from="Sector_Landscape", values_from="tf_idf")%>%write.xlsx("Dicionarios_palavras_TFIDF_Sector_landscape.xlsx")




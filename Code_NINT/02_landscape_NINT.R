library(tidyverse)
library(stringi)
library(readxl)
library(xlsx)
library(pdftools)
source("C:/Users/napcc/Dropbox (CPI)/EduardoMinsky/PARAMIM/landuse_br2024/AuxFolder/Dictionary_Sectors.R")
#Importando tabela relacional
source_finance_landscape <- read_excel("./brlanduse_landscape2024_dados/NINT/11_nint_relational_tables.xlsx",sheet = "source_finance_landscape") %>% select(-emissor_trade_name)
source_finance_landscape <- source_finance_landscape %>% mutate(source_original = str_to_lower(stri_trans_general(source_original,"Latin-ASCII")))

channel_landscape <- read_excel("./brlanduse_landscape2024_dados/NINT/11_nint_relational_tables.xlsx",sheet = "channel_landscape") %>% select(-tipo_de_emissor)
channel_landscape <- channel_landscape %>% mutate(source_original = str_to_lower(stri_trans_general(source_original,"Latin-ASCII")),
                                channel_original = str_to_lower(stri_trans_general(channel_original,"Latin-ASCII")))
channel_landscape <- channel_landscape %>% mutate(left_link = str_c(source_original,channel_original,sep = ";")) %>% select(left_link,channel_landscape)

instrument_landscape <- read_excel("./brlanduse_landscape2024_dados/NINT/11_nint_relational_tables.xlsx",sheet = "instrument_landscape") %>% select(-tipo,-categoria,-instrumento_financeiro)
instrument_landscape <- instrument_landscape %>% mutate(instrument_original = str_to_lower(stri_trans_general(instrument_original,"Latin-ASCII")))
#####################################################################################################################
nint_clear <- read_csv2("./brlanduse_landscape2024_dados/NINT/nint_clear.csv") %>% filter((data >=2021) & (data<= 2023))
nint_clear[nint_clear == "N/D"] <- NA
#####################################################################################################################
# Criando flag de observações que não possuem nenhum hiperlink para os pareceres
sem_info_nint <-NULL
com_info_nint <- NULL
for( i in 1:nrow(nint_clear)){
    if((nint_clear$verificador_externo[[i]]%>%is.na == TRUE) & (nint_clear$verificador_externo2[[i]]%>%is.na == TRUE) & (nint_clear$cbi[[i]]%>%is.na == TRUE)){
        print(paste0("Sem info: ", i))       
        sem_info_nint [[i]]<-nint_clear%>%slice(i)

    } 
    else if((nint_clear$verificador_externo[[i]]%>%is.na == FALSE) | (nint_clear$verificador_externo2[[i]]%>%is.na == FALSE) | (nint_clear$cbi[[i]]%>%is.na == FALSE)){
        com_info_nint[[i]]<-nint_clear%>%slice(i)
        print(paste0("Com info: ", i))
    }
}
sem_info_nint <- Filter(function(x) length(x) > 0, sem_info_nint)
sem_info_nint<- do.call(rbind,sem_info_nint)
sem_info_nint$Info.Project = "No"
com_info_nint <- Filter(function(x) length(x) > 0, com_info_nint)
com_info_nint <- do.call(rbind,com_info_nint)
com_info_nint$Info.Project = "Yes"

nint_clear <- bind_rows(com_info_nint,sem_info_nint)
#####################################################################################################################
#Criando Coluna para as os titulos dos projetos que tem URL em uma das 3 colunas

# Loop sobre as URLs dos PDFs
#Primeiro Para Verificador Externo
pdf_titulo_verificador_externo <- NULL
# Loop sobre as URLs dos PDFs
pdf_urls = nint_clear$verificador_externo
for (i in 1:length(pdf_urls)) {
  # Utilizando tryCatch para capturar erros e continuar o loop
  pdf_titulo_verificador_externo[[i]] <- tryCatch({
    # Chamada para pdf_text() dentro do bloco try
    pdf_text(pdf_urls[[i]])[[1]]%>%substr(1,900)
  }, error = function(e) {
    # Lidando com o erro no bloco catch
    message(paste("Erro ao processar:", pdf_urls[[i]]))
    return("Error")  # Retornando NULL ou outro valor especial
  })  
}
nint_clear$Titulo_Verificador_Externo = pdf_titulo_verificador_externo


#Para Verificador Externo2
pdf_titulo_verificador_externo2 <- NULL
# Loop sobre as URLs dos PDFs
pdf_urls2 = nint_clear$verificador_externo2
for (i in 1:length(pdf_urls2)) {
  # Utilizando tryCatch para capturar erros e continuar o loop
  pdf_titulo_verificador_externo2[[i]] <- tryCatch({
    # Chamada para pdf_text() dentro do bloco try
    pdf_text(pdf_urls2[[i]])[[1]]%>%substr(1,900)
  }, error = function(e) {
    # Lidando com o erro no bloco catch
    message(paste("Erro ao processar:", pdf_urls2[[i]]))
    return("Error")  # Retornando NULL ou outro valor especial
  })  
}
nint_clear$Titulo_Verificador_Externo2 = pdf_titulo_verificador_externo2

# Para cbi
pdf_titulo_cbi <- NULL
# Loop sobre as URLs dos PDFs
pdf_cbi = nint_clear$cbi
for (i in 1:length(pdf_cbi)) {
  # Utilizando tryCatch para capturar erros e continuar o loop
  pdf_titulo_cbi[[i]] <- tryCatch({
    # Chamada para pdf_text() dentro do bloco try
    pdf_text(pdf_cbi[[i]])[[1]]%>%substr(1,900)
  }, error = function(e) {
    # Lidando com o erro no bloco catch
    message(paste("Erro ao processar:", pdf_cbi[[i]]))
    return("Error")  # Retornando NULL ou outro valor especial
  })  
}
nint_clear$Titulo_cbi = pdf_titulo_cbi



# Criando coluna para descrição dos projetos

#Primeiro Para Verificador Externo
pdf_verificador_externo <- NULL
# Loop sobre as URLs dos PDFs
pdf_urls = nint_clear$verificador_externo
for (i in 1:length(pdf_urls)) {
  # Utilizando tryCatch para capturar erros e continuar o loop
  pdf_verificador_externo[[i]] <- tryCatch({
    # Chamada para pdf_text() dentro do bloco try
    paste(pdf_text(pdf_urls[[i]]), collapse = " ")
    
  }, error = function(e) {
    # Lidando com o erro no bloco catch
    message(paste("Erro ao processar:", pdf_urls[[i]]))
    return("Error")  # Retornando NULL ou outro valor especial
  })  
}
nint_clear$Texto_Verificador_Externo = pdf_verificador_externo

# Para verificador externo 2
pdf_verificador_externo2 <- NULL
# Loop sobre as URLs dos PDFs
pdf_urls2 = nint_clear$verificador_externo2
for (i in 1:length(pdf_urls2)) {
  # Utilizando tryCatch para capturar erros e continuar o loop
  pdf_verificador_externo2[[i]] <- tryCatch({
    # Chamada para pdf_text() dentro do bloco try
    paste(pdf_text(pdf_urls2[[i]]), collapse = " ")
    
  }, error = function(e) {
    # Lidando com o erro no bloco catch
    message(paste("Erro ao processar:", pdf_urls2[[i]]))
    return("Error")  # Retornando NULL ou outro valor especial
  })
}
nint_clear$Texto_Verificador_Externo2 = pdf_verificador_externo2

# Para cbi
pdf_cbi <- NULL
# Loop sobre as URLs dos PDFs
pdf_urls3 = nint_clear$cbi
for (i in 1:length(pdf_urls3)) {
  # Utilizando tryCatch para capturar erros e continuar o loop
  pdf_cbi[[i]] <- tryCatch({
    # Chamada para pdf_text() dentro do bloco try
    
    paste(pdf_text(pdf_urls3[[i]]), collapse = " ")
  }, error = function(e) {
    # Lidando com o erro no bloco catch
    message(paste("Erro ao processar:", pdf_urls3[[i]]))
    return("Error")  # Retornando NULL ou outro valor especial
  })
}

nint_clear$Texto_cbi = pdf_cbi


# Iniciando o landscape transform
nint_clear_landscape <- nint_clear %>% mutate(
    id_original = number,
    data_source = "nint",
    year = data,
    project_name = str_c(mercado,tipo,Texto_Verificador_Externo,Texto_Verificador_Externo2,Texto_cbi,sep = "&"),
    project_description = str_c(Texto_Verificador_Externo,Texto_Verificador_Externo2,Texto_cbi,sep = "&"),
    source_original = emissor_trade_name
) %>% left_join(source_finance_landscape,by = "source_original") %>%
                mutate(value_original_currency = valor *1000000,
                original_currency = moeda,
                channel_original =tipo_de_emissor,left_link = str_c(source_original,channel_original,sep = ";")) %>% left_join(channel_landscape,by="left_link") %>% select(-left_link) %>%
                mutate(instrument_original= str_c(instrumento_financeiro,categoria,sep = "_")) %>% left_join(instrument_landscape,by = "instrument_original") %>% mutate(sector_original = str_c(emissor_trade_name,uso_de_recursos,sep = "_"))


#  Fazendo o filtro sector landscape

nint_clear_landscape_sector_semKlabin <- nint_clear_landscape %>% filter(Info.Project=="Yes") %>% filter((grepl("\\bcra\\b",x = instrumento_financeiro,ignore.case = TRUE)) | (grepl("\\bagropecuaria\\b",x = uso_de_recursos, ignore.case = TRUE)) | (grepl("\\bagricultura\\b",x = uso_de_recursos, ignore.case = TRUE)) |
                                                              (grepl("\\bbrf\\b",x = emissor_trade_name,ignore.case = TRUE)) | (grepl("\\bmarfrig\\b",x = emissor_trade_name,ignore.case = TRUE)) | (grepl("\\bsuzano\\b",x = emissor_trade_name,ignore.case = TRUE)) |
                                                              (grepl("\\bcelulose irani\\b",x = emissor_trade_name,ignore.case = TRUE)) |
                                                              (grepl("\\bthe forest company\\b",x = emissor_trade_name,ignore.case = TRUE)) | (grepl("\\bfs bioenergia\\b",x = emissor_trade_name,ignore.case = TRUE)) |
                                                              (grepl("\\bbioenergetica aroeira\\b",x = emissor_trade_name,ignore.case = TRUE)) | (grepl("\\brizoma agro\\b",x = emissor_trade_name,ignore.case = TRUE)) | (grepl("\\bkatayama alimentos\\b",x = emissor_trade_name,ignore.case = TRUE)) |
                                                              (grepl("\\btaboa\\b",x = emissor_trade_name,ignore.case = TRUE)) | (grepl("\\bslc agricola\\b",x = emissor_trade_name,ignore.case = TRUE)) | (grepl("\\bproteina animal\\b",x=project_description,ignore.case = TRUE))|
                                                              (grepl("\\bsoja\\b",x = project_description,ignore.case = TRUE)) | (grepl("\\bfertilizantes\\b",x = project_description,ignore.case = TRUE)) |
                                                              (grepl("\\bpapel\\b",x = project_description,ignore.case = TRUE) & grepl("\\bcelulose\\b",x = project_description,ignore.case = TRUE)) |
                                                              (grepl("\\bcoproducao\\b",x = project_description,ignore.case = TRUE) & grepl("\\benergia\\b",x = project_description,ignore.case = TRUE) & grepl("\\bbagaço da cana\\b",x = project_description,ignore.case = TRUE)) |
                                                              (grepl("\\bcooperativas\\b",x = project_description,ignore.case = TRUE) & grepl("\\bagro\\b",x = project_description,ignore.case = TRUE)) |
                                                              (grepl("\\bprotein\\b",x = project_description,ignore.case = TRUE)) | 
                                                              (grepl("\\bsoy\\b",x = project_description,ignore.case = TRUE)) | (grepl("\\bsoybean\\b",x = project_description,ignore.case = TRUE)) |
                                                              (grepl("\\bfertilize\\b",x = project_description,ignore.case = TRUE)) | (grepl("\\bpulp\\b",x = project_description,ignore.case = TRUE) & grepl("\\bpaper\\b",x = project_description,ignore.case = TRUE))|
                                                              (grepl("\\bco-generation\\b",x = project_description,ignore.case = TRUE) & grepl("\\bsugar\\b",x = project_description,ignore.case = TRUE) & grepl("\\bbagasse\\b",x = project_description,ignore.case = TRUE)) |
                                                              (grepl("\\bcooperative\\b",x = project_description,ignore.case = TRUE)))  

klabin <- nint_clear_landscape %>% filter(grepl("\\bklabin\\b",x =emissor_trade_name,ignore.case = T ))
nint_clear_landscape_sector <- bind_rows(nint_clear_landscape_sector_semKlabin,klabin)
nint_clear_landscape_sector

# Iniciando a classificação setorial
#Para bioenergia
bioenergia_nint <- bioenergia_search_pattern_NINT(data_frame_NINT = nint_clear_landscape_sector %>% mutate(Coluna_search = project_description),Coluna_search = Coluna_search)
bioenergia_nint_filter <- bioenergia_NINT_out(data_frame_NINT = bioenergia_nint)
bioenergia_nint_filter$Sector_landscape = "Bioenergy and Fuels"
# Para Crop
crop_nint <- crop_search_pattern_NINT(data_frame_NINT = nint_clear_landscape_sector %>% mutate(Coluna_search = project_description),Coluna_search = Coluna_search)
crop_nint_filter <- crop_NINT_out(data_frame_NINT = crop_nint)
crop_nint_filter$Sector_landscape = "Crop"

# Para Forest
forest_nint <- forest_search_pattern_NINT(data_frame_NINT = nint_clear_landscape_sector %>% mutate(Coluna_search = project_description),Coluna_search = Coluna_search)
forest_nint_filter <- forest_NINT_out(data_frame_NINT = forest_nint)
forest_nint_filter$Sector_landscape = "Forest"
# Para Cattle
cattle_nint <- cattle_search_pattern_NINT(data_frame_NINT = nint_clear_landscape_sector %>% mutate(Coluna_search = project_description),Coluna_search = Coluna_search)
cattle_NINT_filter <- cattle_NINT_out(data_frame_NINT = cattle_nint)
cattle_NINT_filter$Sector_landscape = "Cattle"

#Para MultiSector
multisector_nint <- multisector_search_pattern_NINT(data_frame_NINT = nint_clear_landscape_sector %>% mutate(Coluna_search = project_description),Coluna_search = Coluna_search)

a = multisector_nint %>% inner_join(crop_nint_filter,by = "project_description") %>% unique
a %>% select(verificador_externo.x,verificador_externo2.x,Texto_cbi.x,verificador_externo.y,verificador_externo2.y,Texto_cbi.y) %>% view


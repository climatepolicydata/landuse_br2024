library(tidyverse)
library(stringi)
library(readxl)
library(xlsx)
library(pdftools)
library(lexRankr)
source("C:/Users/napcc/Dropbox (CPI)/EduardoMinsky/PARAMIM/landuse_br2024/AuxFolder/Dictionary_Sectors.R")
#Importando tabela relacional
source_finance_landscape <- read_excel("A:\\projects\\landuse_br2024\\NINT/11_nint_relational_tables.xlsx",sheet = "source_finance_landscape") %>% select(-emissor_trade_name)
source_finance_landscape <- source_finance_landscape %>% mutate(source_original = str_to_lower(stri_trans_general(source_original,"Latin-ASCII")))

channel_landscape <- read_excel("A:\\projects\\landuse_br2024\\NINT/11_nint_relational_tables.xlsx",sheet = "channel_landscape") %>% select(-tipo_de_emissor)
channel_landscape <- channel_landscape %>% mutate(source_original = str_to_lower(stri_trans_general(source_original,"Latin-ASCII")),
                                channel_original = str_to_lower(stri_trans_general(channel_original,"Latin-ASCII")))
channel_landscape <- channel_landscape %>% mutate(left_link = str_c(source_original,channel_original,sep = ";")) %>% select(left_link,channel_landscape)

instrument_landscape <- read_excel("A:\\projects\\landuse_br2024\\NINT/11_nint_relational_tables.xlsx",sheet = "instrument_landscape") %>% select(-tipo,-categoria,-instrumento_financeiro)
instrument_landscape <- instrument_landscape %>% mutate(instrument_original = str_to_lower(stri_trans_general(instrument_original,"Latin-ASCII")))
#####################################################################################################################
nint_clear <- read_csv2("A:\\projects\\landuse_br2024\\NINT\\Clean_data\\nint_clear_19_03_2024.csv") %>% filter((data >=2021) & (data<= 2023))
nint_clear[nint_clear == "N/D"] <- NA
nint_clear%>%view
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
nint_clear%>%view

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
nint_clear_landscape %>% view
# Iniciando a classificação setorial

#Para bioenergia
bioenergia_nint <- bioenergia_search_pattern_NINT(data_frame_NINT = nint_clear_landscape %>% mutate(Coluna_search = project_description),Coluna_search = Coluna_search)
bioenergia_nint_filter <- bioenergia_NINT_out(data_frame_NINT = bioenergia_nint)
bioenergia_nint_filter$sector_landscape = "Bioenergy and Fuels"
# Para Crop
crop_nint <- crop_search_pattern_NINT(data_frame_NINT = nint_clear_landscape %>% mutate(Coluna_search = project_description),Coluna_search = Coluna_search)
crop_nint_filter <- crop_NINT_out(data_frame_NINT = crop_nint)
crop_nint_filter$sector_landscape = "Crop"

# Para Forest
forest_nint <- forest_search_pattern_NINT(data_frame_NINT = nint_clear_landscape %>% mutate(Coluna_search = project_description),Coluna_search = Coluna_search)
forest_nint_filter <- forest_NINT_out(data_frame_NINT = forest_nint)
forest_nint_filter$sector_landscape = "Forest"
# Para Cattle
cattle_nint <- cattle_search_pattern_NINT(data_frame_NINT = nint_clear_landscape %>% mutate(Coluna_search = project_description),Coluna_search = Coluna_search)
cattle_NINT_filter <- cattle_NINT_out(data_frame_NINT = cattle_nint)
cattle_NINT_filter$sector_landscape = "Cattle"

#Para MultiSector
multisector_nint <- multisector_search_pattern_NINT(data_frame_NINT = nint_clear_landscape %>% mutate(Coluna_search = project_description),Coluna_search = Coluna_search)
multisector_nint_filter <- multisector_NINT_out(multisector_nint)
multisector_nint_filter$sector_landscape = "MultiSector"

nint_sectorLandscape <- bind_rows(bioenergia_nint_filter,crop_nint_filter,forest_nint_filter,cattle_NINT_filter,multisector_nint_filter)
nint_clear_SemSector <- nint_clear_landscape %>% anti_join(nint_sectorLandscape,by = "project_description") 
# Dando continuidade ao Landscape transform

nint_sectorLandscape <- nint_sectorLandscape %>% mutate(
  subsector_original = "-",
  rio_marker = '-',
  localization_original = "-",
  region = "-",
  uf = "-",
  municipality = "-"
)
# Fazendo o filtro de atividade e componente climático

#  Produção de cana-de-açúcar, inclusive para geração de energia
nint_Mitigacao_ProducaoCanaAcucar <- nint_sectorLandscape%>%filter((sector_landscape== "Bioenergy and Fuels") | (sector_landscape== "Crop")) %>% 
filter(
  (grepl("\\bplantio e trato de canavial\\b",x = project_description,ignore.case = TRUE)) |
  (grepl("\\brenovação de canavial\\b",x = project_description,ignore.case = TRUE)) |
  (grepl("\\bplantio e tratos culturais\\b",x = project_description,ignore.case = TRUE))|
  (grepl("\\bcultivo de cana-de-açúcar\\b",x = project_description,ignore.case = TRUE))|
  (grepl("\\bexpansão de canavial\\b",x = project_description,ignore.case = TRUE))|
  (grepl("\\bexpansão de canaviais\\b",x = project_description,ignore.case = TRUE)) |
  (grepl("\\bsugarcane harvesting\\b",x = project_description,ignore.case = TRUE)))%>%mutate(
    activity_landscape = "Produção de cana-de-açúcar, inclusive para geração de energia",
    subactivity_landscape = "Expansão e renovação de canaviais, otimização da colheita e ampliação da capacidade de moagem de cana. Inclui aquisição de máquinas, equipamentos e construção de unidades de armazenamento para etanol e açúcar.",
    climate_use = "Mitigação"
  )

#Primeiro Filtro

nint_sectorLandscape <- nint_sectorLandscape %>% anti_join(nint_Mitigacao_ProducaoCanaAcucar,by = "project_description") 

# Produção de biocombustíveis, incluindo biodiesel e bioetanol
Producao_biocomb_biodiesel_etanol <- nint_sectorLandscape %>% filter(
 ( grepl("\\bcorn ethanol\\b",x =project_description , ignore.case = TRUE )) |
 (grepl("\\bprodução\\b",x =project_description , ignore.case = TRUE) & grepl("\\bbiocombustíveis\\b",x =project_description , ignore.case = TRUE)) |
 (grepl("\\bprodução\\b",x =project_description , ignore.case = TRUE) & grepl("\\bbiocombustível\\b",x =project_description , ignore.case = TRUE)) |
 (grepl("\\bprodução\\b",x =project_description , ignore.case = TRUE) & grepl("\\bbiodiesel\\b",x =project_description , ignore.case = TRUE)) |
 (grepl("\\bprodução\\b",x =project_description , ignore.case = TRUE) & grepl("\\bbioetanol\\b",x =project_description , ignore.case = TRUE))  
) %>% filter((!verificador_externo2 %in% "https://api.mziq.com/mzfilemanager/v2/d/a608601a-f940-4251-9b9d-7feaf98e8e69/e1549723-2cf6-ce41-48f4-e1287b47b03b?origin=1") &
(!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20211020+JF+Citrus.pdf") & (!verificador_externo %in% "fora https://nintspo.s3.sa-east-1.amazonaws.com/20221212+Agrogalaxy.pdf") &
(!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210602+Integral+Group+BREI+-+Noah.pdf")) %>% mutate(activity_landscape = "Produção de biocombustíveis, incluindo biodiesel e bioetanol",
                                                                                                                            subactivity_landscape = "Modernização de equipamentos, processos e instalações industriais e agrícolas. Construção de tanques de armazenamento de etanol. Investimentos para mitigar riscos ambientais, legais, trabalhistas e operacionais.",
                                                                                                                            climate_use = "Mitigação")

# Segundo filtro
nint_sectorLandscape <- nint_sectorLandscape %>% anti_join(Producao_biocomb_biodiesel_etanol,by = "project_description") 

# Geração de energia renovável e medidas para eficiência energética
GeracaoEnergiaRenovavelMEdidasEficiencia <- nint_sectorLandscape %>% filter(
  (grepl("\\bsolar\\b",x =project_description , ignore.case = TRUE)) |
  (grepl("\\bbiogás\\b",x =project_description , ignore.case = TRUE)) |
  (grepl("\\bbiomassa\\b",x =project_description , ignore.case = TRUE)) |
  (grepl("\\bfoto\\b",x =project_description , ignore.case = TRUE)) |
  (grepl("\\bcogeração\\b",x =project_description , ignore.case = TRUE)) |
  (grepl("\\bCentral Geradora Hidrelétrica\\b",x =project_description , ignore.case = TRUE)) |
  (grepl("\\blinhas de transmissão\\b",x =project_description , ignore.case = TRUE))
) %>% filter((!verificador_externo %in% "https://www.iss-corporate.com/file/documents/spo/spo-20210119-movida.pdf")  &
            (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210908+Green+Yellow.pdf") &
            (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20211020+JF+Citrus.pdf") &
            (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210329+Solinftec.pdf") &
            (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20221212+Agrogalaxy.pdf") &
            (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20231129+Capal.pdf") &
            (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210602+Integral+Group+BREI+-+Noah.pdf") &
            (!verificador_externo %in% "https://www.tanac.com.br/novo/wp-content/uploads/2021/08/SPO_TANAC_IBBA_BVC-POS-LAST_Ok.pdf")) %>% mutate(
              activity_landscape = "Geração de energia renovável e medidas para eficiência energética",
              subactivity_landscape = if_else((grepl("\\bsolar\\b",x =project_description , ignore.case = TRUE)),true = "Energia solar para redes centralizadas, incluindo células fotovoltaicas e sistemas de energia solar concentrada, e para redes isoladas e sistemas autônomos, incluindo minirredes e sistemas solares residenciais.",
              false = if_else((grepl("\\bbiogás\\b",x =project_description , ignore.case = TRUE)),true = "Tratamento de água e resíduos para produção de energia a partir do biogás.",
              false = if_else((grepl("\\bcogeração\\b",x =project_description , ignore.case = TRUE)),true = "Produção de vapor e cogeração de energia a partir da cana-de-açúcar.",
              false = if_else((grepl("\\bCentral Geradora Hidrelétrica\\b",x =project_description , ignore.case = TRUE) | (grepl("\\blinhas de transmissão\\b",x =project_description , ignore.case = TRUE))),
              true = "Construção de subestações e linhas de transmissão para conexão à rede elétrica nacional.", false = "Sem Classificacao")))),climate_use = "Mitigação"
            )

#Terceiro Filtro
nint_sectorLandscape <- nint_sectorLandscape %>% anti_join(GeracaoEnergiaRenovavelMEdidasEficiencia,by = "project_description") 


# Gerenciamento e monitoramento para uso  de água e saneamento
Gerenciamento_Monitoramento_AguaSaneamento <- nint_sectorLandscape %>% filter(
  (grepl("\\bsaneamento\\b",x =project_description , ignore.case = TRUE))
) %>% filter(
  (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210723+Adami.pdf") &
  (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210330+Allonda.pdf") &
  (!verificador_externo %in% "https://api.mziq.com/mzfilemanager/v2/d/50b51302-4c48-4351-b296-bfcbe65fd70a/df5ce540-aa94-f520-78c4-16e970b45dc5?origin=1") &
  (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20220406+Allonda.pdf") &
  (!verificador_externo %in% "https://www.pge.eco.br/_files/ugd/409741_2987a3355e2f4c509f7f8a9cc592a2ad.pdf") &
  (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20230119+Pisani.pdf")
) %>% mutate(
  activity_landscape = "Gerenciamento e monitoramento para uso  de água e saneamento",
  subactivity_landscape = "Programas de abastecimento de água, saneamento e higiene.",
  climate_use = "Mitigação e Adaptação"
)

#Quarto Filtro
nint_sectorLandscape <- nint_sectorLandscape %>% anti_join(Gerenciamento_Monitoramento_AguaSaneamento,by = "project_description") 


# Para Infraestrutura e tecnologias agrícolas
InfraTecAgricola <- nint_sectorLandscape %>% filter(
  (grepl("\\bagricultura de precisão\\b",x =project_description , ignore.case = TRUE))
) %>% mutate(activity_landscape = "Infraestrutura e tecnologias agrícolas",
            subactivity_landscape = if_else((grepl("\\bagricultura de precisão\\b",x =project_description , ignore.case = TRUE)),true = "Equipamentos e utensílios para agricultura de precisão",
            false = "Sem Classificacao"))%>%mutate(climate_use = "Mitigação e Adaptação")

#Quinto Filtro
nint_sectorLandscape <- nint_sectorLandscape %>% anti_join(InfraTecAgricola,by = "project_description") 


#Para Atividades relacionadas à indústria de florestas plantadas, celulose e papel
AtividadesIndustriaFlorestasCelulose <- nint_sectorLandscape %>% filter(
  (grepl("\\bpinus\\b",x =project_description , ignore.case = TRUE)) |
  (grepl("\\bSuzano\\b",x =project_description , ignore.case = TRUE))) %>% filter(
    (!verificador_externo %in% "https://nintspo.s3.sa-east-1.amazonaws.com/20210602+Integral+Group+BREI+-+Noah.pdf")
  )%>%mutate(activity_landscape = "Para Atividades relacionadas à indústria de florestas plantadas, celulose e papel",
subactivity_landscape = "Investimentos em modernização industrial e manutenção da capacidade produtiva da indústria de celulose e papel alinhados ao meio ambiente.",
climate_use = "Mitigação e Adaptação")

#Sexto Filtro
nint_sectorLandscape <- nint_sectorLandscape %>% anti_join(AtividadesIndustriaFlorestasCelulose,by = "project_description") 

# Nova Classse ainda sem classificacao landscape
novos_setores <- nint_sectorLandscape %>% filter(
  (grepl("\\bagricultura de baixo carbono\\b",x = project_description,ignore.case = TRUE) |
  (grepl("\\blow-carbon agricultural \\b",x = project_description,ignore.case = TRUE)) |
  (grepl("\\bimplantação\\b",x = project_description,ignore.case = TRUE) & grepl("\\bcomplexo\\b",x = project_description,ignore.case = TRUE) & grepl("\\beólico\\b",x = project_description,ignore.case = TRUE)))
) %>%mutate(activity_landscape = "Nova Atividade (possivelmente)",
            subactivity_landscape = if_else((grepl("\\bagricultura de baixo carbono\\b",x = project_description,ignore.case = TRUE) |grepl("\\blow-carbon agricultural \\b",x = project_description,ignore.case = TRUE)),
            true = "Nova Atividade - Agricultura de Baixo Carbono",
            false = if_else((grepl("\\bimplantação\\b",x = project_description,ignore.case = TRUE) & grepl("\\bcomplexo\\b",x = project_description,ignore.case = TRUE) & grepl("\\beólico\\b",x = project_description,ignore.case = TRUE)),
            true = "Nova Atividade - Implantação de Usina Eólica", false = "Sem Classificacao"))
            )%>%mutate(climate_use = "Novas Atividades")

#Sétimo Filtro
nint_sectorLandscape <- nint_sectorLandscape %>% anti_join(novos_setores,by = "project_description") 

# Unindo
nint_sectorlandscape_climateUse <- rbind(nint_Mitigacao_ProducaoCanaAcucar,Producao_biocomb_biodiesel_etanol,GeracaoEnergiaRenovavelMEdidasEficiencia,
Gerenciamento_Monitoramento_AguaSaneamento,InfraTecAgricola,AtividadesIndustriaFlorestasCelulose,novos_setores)
nint_sectorlandscape_climateUse%>%view
# Sem filtro de clima OU setor
nint_sectorLandscape_NoClimate <- nint_sectorLandscape
nint_sectorLandscape_NoClimate %>%unique%>% view
nint_clear_SemSector %>% unique %>% view
# Criação do resumo da descricao dos projetos
nint_sectorlandscape_climateUse$VetorProcura <- str_c(nint_sectorlandscape_climateUse$Texto_Verificador_Externo,nint_sectorlandscape_climateUse$Texto_Verificador_Externo2,nint_sectorlandscape_climateUse$Texto_cbi)
nint_sectorlandscape_climateUse <- nint_sectorlandscape_climateUse %>% mutate(VetorProcura = gsub("\n","",VetorProcura))

top_3_words <- NULL
for(i in 1:nrow(nint_sectorlandscape_climateUse)){
  top_3_words[[i]] <- lexRankr::lexRank(nint_sectorlandscape_climateUse$VetorProcura[[i]],
                                        docId = 1,
                                        n = 3,
                                        continuous = TRUE)
}
resumo_colapse <- NULL
for(i in 1:length(top_3_words)){
  resumo_colapse[[i]] <- paste(top_3_words[[i]][[3]],collapse = "  ||||  ")
}
df_unico <- NULL
for(i in 1:length(resumo_colapse)){
  df_unico[[i]] <- nint_sectorlandscape_climateUse %>% slice(i) %>% mutate(project_description_3 =resumo_colapse[[i]]) %>% mutate(project_description_3 = str_to_lower(stri_trans_general(project_description_3,"Latin-ASCII")))
}

nint_sectorlandscape_climateUse_V2 <- do.call(rbind,df_unico)
nint_sectorlandscape_climateUse_V2 <- nint_sectorlandscape_climateUse_V2 %>% mutate(verificador_externo = if_else(is.na(verificador_externo),"SEM VERIFICADOR",false = verificador_externo),
verificador_externo2 = if_else(is.na(verificador_externo2),true = "SEM VERIFICADOR",false = verificador_externo2),
cbi = if_else(is.na(cbi),true = "SEM VERIFICADOR",false = cbi))

nint_sectorlandscape_climateUse_V2 <- nint_sectorlandscape_climateUse_V2 %>% mutate(project_name2 = str_c(verificador_externo,verificador_externo2,cbi,sep = " /// "))
nint_sectorlandscape_climateUse_V2 <- nint_sectorlandscape_climateUse_V2 %>% mutate(beneficiary_original = "-",beneficiary_landscape = "-",beneficiary_public_private = "-") %>% select(id_original,data_source,year,project_name2,project_description_3,source_original,source_finance_landscape,origin_domestic_international,
origin_private_public,value_original_currency,original_currency,channel_original,channel_landscape,instrument_original,instrument_landscape,sector_original,sector_landscape,
subsector_original,activity_landscape,subactivity_landscape,climate_use,beneficiary_original,beneficiary_landscape,beneficiary_public_private,rio_marker,localization_original,region,uf,municipality)
nint_sectorlandscape_climateUse_V2 <- nint_sectorlandscape_climateUse_V2 %>% rename(project_name = project_name2 , project_description = project_description_3)
nint_sectorlandscape_climateUse_V2 %>% view
# Realizando a deflação
ibge_ipca <- read_excel("A:\\macro\\IPCA\\cleanData\\ipca_ibge_cl.xlsx")
ibge_ipca <- ibge_ipca %>% 
  mutate(variacao_doze_meses = suppressWarnings(as.numeric(variacao_doze_meses)))

#criando a funcao

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

#aplicando a funcao na base 
#essa funcao eh arbitraria, devido as diferentes variáveis que queremos multiplicar e diferentes bases

calculo_deflator <- function(tabela_deflator, base_select_deflator) {
  
  base_select_deflator <- base_select_deflator %>% 
    left_join(tabela_deflator, by= "year")%>%
    mutate(value_brl_deflated = as.numeric(value_original_currency * deflator))
  
  
  return(base_select_deflator)
  
}
# eh necessario a escolha dos anos nos quais quer criar os indices. Assim, a funcao toma como base/indice = 1 o ano final

ano_ini = 2021
ano_fim = 2023

#a variavel anos completa os anos no intervalo de anos escolhidos acima.
anos = seq(ano_fim,ano_ini, -1)

teste <- deflator_automatico(ano_ini, ano_fim, anos,ibge_ipca)
nint_sectorlandscape_climateUse_V2_deflated <- calculo_deflator(tabela_deflator = teste, base_select_deflator = nint_sectorlandscape_climateUse_V2)
# Dolarizando

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
  filter(year >= 2021 & year <= 2023)
tabela_cambio
nint_sectorlandscape_climateUse_V2_deflated_dolar <- nint_sectorlandscape_climateUse_V2_deflated %>% left_join(tabela_cambio,by="year")%>% mutate(value_usd = value_brl_deflated / cambio)

nint_sectorlandscape_climateUse_V2_deflated_dolar%>% select(-deflator,-cambio)%>% write_csv2("A:\\projects\\landuse_br2024\\NINT\\NINT_landscape_04_04_2024.csv")

nint_sectorlandscape_climateUse_V2_deflated_dolar%>% select(-deflator,-cambio)%>% write_rds("A:\\projects\\landuse_br2024\\NINT\\NINT_landscape_04_04_2024.rds")



passado <- read_rds("A:\\projects\\brlanduse_landscape102023\\nint\\output\\df_nint_landscape_final_replication.rds")
ano_ini = 2015
ano_fim = 2023
anos = seq(ano_fim,ano_ini, -1)
passado <- passado %>% select(-value_brl_deflated)
teste <- deflator_automatico(ano_ini, ano_fim, anos,ibge_ipca)
passado_deflated <- calculo_deflator(tabela_deflator = teste, base_select_deflator = passado)
passado_agrupado <- passado_deflated %>% group_by(year) %>% summarise(SomaAno_deflacionado = sum(value_brl_deflated),SomaAno = sum(value_original_currency))

nint_sectorlandscape_climateUse_V2_deflated_dolar %>% group_by(year) %>% summarise(SomaAno_deflacionado = sum(value_brl_deflated),SomaAno = sum(value_original_currency))
bind_rows(passado_agrupado,nint_sectorlandscape_climateUse_V2_deflated_dolar %>% group_by(year) %>% summarise(SomaAno_deflacionado = sum(value_brl_deflated),SomaAno = sum(value_original_currency))) %>% write.csv2("asd.csv")
#Agricultura de baixo carbono

#https://nintspo.s3.sa-east-1.amazonaws.com/20211020+JF+Citrus.pdf

#Compra soja baixo carbono
#https://nintspo.s3.sa-east-1.amazonaws.com/20231129+Capal.pdf

#Implantacao complexo eólico
#https://nintspo.s3.sa-east-1.amazonaws.com/20210806++Alian%C3%A7a+Energia.pdf
#https://nintspo.s3.sa-east-1.amazonaws.com/20211015+Tucano+II.pdf




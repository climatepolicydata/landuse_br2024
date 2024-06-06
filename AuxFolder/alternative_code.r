library(tidyverse)
library(stringi)
library(readxl)
library(readr)
library(dplyr)
github <- "Documents"
root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
source(paste0(root,github,"/GitHub/landuse_br2024/AuxFolder/Dictionary_Sectors.R"))


siop_tratado <- read_rds("A:\\projects\\landuse_br2024\\siop\\Clean_Data\\Siop_Tratado_2015_2023_05_24.rds") #258.437
# siop_tratado%>% filter(plano_orc == "censo demografico 2020")   %>% select(Pago) %>% view
grupo_despesa <- read_excel("A:\\projects\\landuse_br2024\\siop\\12_siop_relational_tables - ATUALIZACAO.xlsx", sheet="grupo_despesa")

channel_landscape <- read_excel("A:\\projects\\landuse_br2024\\siop\\12_siop_relational_tables - ATUALIZACAO.xlsx", sheet="channel_landscape")

sector_landscape <- read_excel("A:\\projects\\landuse_br2024\\siop\\12_siop_relational_tables - ATUALIZACAO.xlsx", sheet="sector_landscape")
sector_landscape <- sector_landscape %>% mutate(acao_des_limpa = str_trim(str_to_lower(stri_trans_general(acao_des_orig,"Latin-ASCII"))))
sector_landscape <- sector_landscape%>%select(acao_des_limpa,sector_landscape)
sector_landscape <- sector_landscape %>% distinct(acao_des_limpa,.keep_all = TRUE) 


beneficiary_landscape <- read_excel("A:\\projects\\landuse_br2024\\siop\\12_siop_relational_tables - ATUALIZACAO.xlsx", sheet="beneficiary_landscape")
beneficiary_landscape <- beneficiary_landscape%>% mutate(acao_des_limpa = str_trim(str_to_lower(stri_trans_general(acao_des,"Latin-ASCII"))))
# beneficiary_landscape[duplicated(beneficiary_landscape$acao_des_limpa),] %>% view
beneficiary_landscape <- beneficiary_landscape %>% distinct(acao_des_limpa,.keep_all = TRUE)
beneficiary_landscape <- beneficiary_landscape %>% select(acao_des_limpa,beneficiary_landscape,Fonte)

####################################################
source_landscape <- read_excel("A:\\projects\\landuse_br2024\\siop\\12_siop_relational_tables - ATUALIZACAO.xlsx", sheet="source_landscape")

source_landscape <- source_landscape%>%mutate(source_original = str_trim(str_to_lower(stri_trans_general(source_original,"Latin-ASCII"))))

instrument_landscape <- read_excel("A:\\projects\\landuse_br2024\\siop\\12_siop_relational_tables - ATUALIZACAO.xlsx", sheet="instrument_landscape")

climate_use <- read_excel("A:\\projects\\landuse_br2024\\siop\\12_siop_relational_tables - ATUALIZACAO.xlsx", sheet="climate_use")
climate_use <- climate_use %>% mutate(acao_des_limpa = str_trim(str_to_lower(stri_trans_general(acao_des,"Latin-ASCII"))))
climate_use<-climate_use %>% distinct(acao_des_limpa,.keep_all = TRUE)

######## leitura tabela relacional do plano orçamentário$$$$$$$$$$$

acao_plan_orc_count <- read_excel("A:\\projects\\landuse_br2024\\siop\\12_siop_relational_tables - ATUALIZACAO.xlsx",sheet = "acao_plan_orc_filter")
acao_plan_orc_count_unique <- acao_plan_orc_count %>% mutate(plano_orc_clean = str_trim(str_to_lower(stri_trans_general(plano_orc_clean,"Latin-ASCII")),side="both")) 
# acao_plan_orc_count_unique[duplicated(acao_plan_orc_count_unique$plano_orc_clean),] %>% view
acao_plan_orc_count_unique<- acao_plan_orc_count_unique %>% distinct(plano_orc_clean,.keep_all = TRUE)
# Iniciando as analises

##### filtros iniciais #####
siop_tratado_new = siop_tratado
siop_tratado_new_ano <- siop_tratado_new %>% filter(Pago != 0)
siop_tratado_new_ano <- siop_tratado_new_ano%>%  filter(Ano >= 2021 & Ano <= 2023) 
siop_tratado_new_ano$Pago %>% sum #1.228545e+13
siop_tratado_new_ano<- siop_tratado_new_ano %>% filter(grupo_de_despesa != "juros e encargos da divida" & grupo_de_despesa!= "amortizacao da divida" & grupo_de_despesa != "reserva de contingencia") #141.998 registros
siop_tratado_new_ano$Pago %>% sum #6.558356e+12
siop_tratado_unidade_orcamentaria_new <- siop_tratado_new_ano %>% inner_join(channel_landscape %>% filter(!is.na(und_orc)) %>% select(und_orc)%>% unique, by= "und_orc")
siop_tratado_unidade_orcamentaria_new$Pago %>% sum #289.978.075.877
#Criacao do Reminder. É nele que vamos aplicar o dicionario
df_remainder_SIOP1 <- siop_tratado_new_ano %>% anti_join(siop_tratado_unidade_orcamentaria_new %>% select(und_orc )%>% unique, by= "und_orc")

crop_search_pattern_SIOP <- function(data_frame_SIOP, Coluna_search) {
  data_frame_crop <- data_frame_SIOP %>% filter(
    (
      grepl("\\breducao\\b", Coluna_search, ignore.case = TRUE) &
        grepl("\\briscos\\b", Coluna_search, ignore.case = TRUE) &
        grepl("\\bagropecuaria", Coluna_search, ignore.case = TRUE)
    ) |
      (
        grepl("\\bestudos\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bimplementacao\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bmanutencao\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bzoneamento\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bagricola\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\brisco\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bclimatico\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\abastecimento\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bcomercializacao\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bassistencia\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bextensao rural\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bprodutor\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\brural\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bagricultura\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bextensao rural\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\breforma\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bampliacao\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\blaboratorios\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bagropecuarios\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bagricultura\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bdefesa\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bagropecuaria\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bproducao\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bdivulgacao\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\binformacoes\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bmeteorologicas\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bclimatologicas\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bagricultura\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bmeteorologia\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bpromocao\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bagricultura\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bfamiliar\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bgestao\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\brisco\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bagricultura familiar\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\borganizacao agraria\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bextensao rural\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bfortalecimento\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bdinamizacao\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bagricultura familiar\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bdesenvolvimento agrario\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bagricultura familiar\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bdigitalizacao\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bacervo\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bhistorico\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bmeteorologicos\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bprotecao\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bcultivares\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\buso sustentavel\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\brecursos geneticos\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bagricultura\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bapoio\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bdesenvolvimento\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bsustentavel\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bterritorios rurais\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bdesenvolvimento\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bterritorial\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\brural\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bcombate\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bpobreza\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bagricultura\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bfamiliar\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bdesenvolvimento\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bagrario\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bmulheres\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\brurais\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bagricultura familiar\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bagroalimentar\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bpos colheita\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bavaliacao\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bsafras\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bcompanhia nacional de abastecimento\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bembrapa\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bagricultura\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bdesenvolvimento\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\btecnologico\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bengenharia\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\binovacoes\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bagropecuaria\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\btransferencia\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\btecnologias\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bagropecuaria\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\btecnologias\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bagropecuaria\\b", Coluna_search, ignore.case = TRUE)
      ) |
      (
        grepl("\\bunidades\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\breferencia\\b", Coluna_search, ignore.case = TRUE) &
          grepl("\\bmetereologica\\b", Coluna_search, ignore.case = TRUE)
      )
  )
  return(data_frame_crop)
}

result <- crop_search_pattern_SIOP(data_frame_SIOP = df_remainder_SIOP1,Coluna_search = Coluna_search)

crop_search_pattern_SIOP <- function(data_frame_SIOP, Coluna_search) {
  patterns <- list(
    c("reducao", "riscos", "agropecuaria"),
    c("estudos", "implementacao", "manutencao", "zoneamento", "agricola", "risco", "climatico"),
    c("abastecimento", "comercializacao"),
    c("assistencia", "extensao rural", "produtor", "rural"),
    c("agricultura", "extensao rural"),
    c("reforma", "ampliacao", "laboratorios", "agropecuarios"),
    c("agricultura", "defesa", "agropecuaria"),
    c("producao", "divulgacao", "informacoes", "meteorologicas", "climatologicas"),
    c("agricultura", "meteorologia"),
    c("promocao", "agricultura", "familiar"),
    c("gestao", "risco", "agricultura familiar"),
    c("organizacao agraria", "extensao rural"),
    c("fortalecimento", "dinamizacao", "agricultura familiar"),
    c("desenvolvimento agrario", "agricultura familiar"),
    c("digitalizacao", "acervo", "historico", "meteorologicos"),
    c("protecao", "cultivares"),
    c("uso sustentavel", "recursos geneticos", "agricultura"),
    c("apoio", "desenvolvimento", "sustentavel", "territorios rurais"),
    c("desenvolvimento", "territorial", "rural", "combate", "pobreza"),
    c("agricultura", "familiar", "desenvolvimento", "agrario"),
    c("mulheres", "rurais"),
    c("agricultura familiar"),
    c("agroalimentar"),
    c("pos colheita"),
    c("avaliacao", "safras"),
    c("companhia nacional de abastecimento"),
    c("embrapa"),
    c("agricultura", "desenvolvimento", "tecnologico", "engenharia"),
    c("inovacoes", "agropecuaria"),
    c("transferencia", "tecnologias", "agropecuaria"),
    c("tecnologias", "agropecuaria"),
    c("unidades", "referencia", "metereologica")
  )
  
  data_frame_crop <- data_frame_SIOP %>% filter(
    any(sapply(patterns, function(pat) all(grepl(paste("\\b", pat, "\\b", sep=""), Coluna_search, ignore.case = TRUE))))
  )
  
  return(data_frame_crop)
}

df_remainder_SIOP1 <- df_remainder_SIOP1 %>% mutate(Coluna_search = str_c(acao,plano_orc,funcao,subfuncao,programa,modalidade,
                                                                          und_orc,Fonte,sep = ";"))

result2 <- crop_search_pattern_SIOP(data_frame_SIOP = df_remainder_SIOP1,Coluna_search = Coluna_search)

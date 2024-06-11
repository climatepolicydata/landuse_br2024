tempo <- system.time({

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
  data_frame_crop <- data_frame_SIOP %>% 
    filter(
      grepl(
        pattern = "\\breducao\\b&\\briscos\\b&\\bagropecuaria",
        x = Coluna_search,
        ignore.case = TRUE
      ) |
        grepl(
          pattern = "\\bestudos\\b&\\bimplementacao\\b&\\bmanutencao\\b&\\bzoneamento\\b&\\bagricola\\b&\\brisco\\b&\\bclimatico\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\abastecimento\\b|\\bcomercializacao\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bassistencia\\b|\\bextensao rural\\b|\\bprodutor\\b|\\brural\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "agricultura\\b|\\bextensao rural\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\breforma\\b|\\bampliacao\\b|\\blaboratorios\\b|\\bagropecuarios\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bagricultura\\b|\\bdefesa\\b|\\bagropecuaria\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bproducao\\b|\\bdivulgacao\\b|\\binformacoes\\b|\\bmeteorologicas\\b|\\bclimatologicas\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bagricultura\\b|\\bmeteorologia\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bpromocao\\b|\\bagricultura\\b|\\bfamiliar\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bgestao\\b|\\brisco\\b|\\bagricultura familiar\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\borganizacao agraria\\b|\\bextensao rural\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bfortalecimento\\b|\\bdinamizacao\\b|\\bagricultura familiar\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bdesenvolvimento agrario\\b|\\bagricultura familiar\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bdigitalizacao\\b|\\bacervo\\b|\\bhistorico\\b|\\bmeteorologicos\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bprotecao\\b|\\bcultivares\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\buso sustentavel\\b|\\brecursos geneticos\\b|\\bagricultura\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bapoio\\b|\\bdesenvolvimento\\b|\\bsustentavel\\b|\\bterritorios rurais\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bdesenvolvimento\\b|\\bterritorial\\b|\\brural\\b|\\bcombate\\b|\\bpobreza\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bagricultura\\b|\\bfamiliar\\b|\\bdesenvolvimento\\b|\\bagrario\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bmulheres\\b|\\brurais\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bagricultura familiar\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bagroalimentar\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bpos colheita\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bavaliacao\\b|\\bsafras\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bcompanhia nacional de abastecimento\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bembrapa\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bagricultura\\b|\\bdesenvolvimento\\b|\\btecnologico\\b|\\bengenharia\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\binovacoes\\b|\\bagropecuaria\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\binovacao\\b|\\bservicos\\b|\\btecnologicos\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) | grepl(
          pattern = "\\bsistemas\\b|\\bproducao\\b|\\bagricola\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bcadeia produtiva\\b|\\btecnologias\\b|\\bagricolas\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bsistemas\\b|\\bintegrados\\b|\\bproducao\\b|\\bagronegocio\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bprojeto\\b|\\bintegração lavoura-pecuária\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bapoio\\b|\\bcadeias produtivas\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bagricultura\\b|\\borganica\\b|\\bmeio ambiente\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bassistencia tecnica\\b|\\bemater\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bsafras\\b|\\bplantio\\b|\\bcolheita\\b|\\bmonitoramento\\b|\\bdefesa agropecuaria\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bmonitoramento\\b|\\bculturas\\b|\\blavouras\\b|\\bsafras\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bplanejamento\\b|\\bmonitoramento\\b|\\bcolheita\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bclassificacao\\b|\\bprodutos agricolas\\b|\\bclassificacao vegetal\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\btecnologia\\b|\\bcomunicacao\\b|\\btecnologia rural\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bmonitoramento\\b|\\bproducao\\b|\\bagricola\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\btransferencia tecnologica\\b|\\bagricultura\\b|\\binovacao\\b|\\btecnologia social\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bagricultura de precisao\\b|\\bmonitoramento\\b|\\bproducao\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bmercados\\b|\\btecnologia\\b|\\bprocessamento\\b|\\balimentos\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bsistemas\\b|\\bprodutivos\\b|\\bproducao\\b|\\bagronegocios\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bagricultura\\b|\\bsustentabilidade\\b|\\bseguranca\\b|\\balimentar\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bdistribuicao\\b|\\brenda\\b|\\bprodutiva\\b|\\bagricultura familiar\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bsafras\\b|\\bprecos\\b|\\bprodutos agricolas\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bmonitoramento\\b|\\bdados\\b|\\binformacao\\b|\\bagricola\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bmercados\\b|\\bcomercializacao\\b|\\bprodutos agricolas\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bpesquisa\\b|\\bdesenvolvimento\\b|\\bagricultura\\b|\\btecnologico\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bassistencia\\b|\\bemater\\b|\\bprodutores\\b|\\brurais\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bagricultura\\b|\\bprecos\\b|\\bmercados\\b|\\bprodutos agricolas\\b",
          x = Coluna_search,
          ignore.case = TRUE
        ) |
        grepl(
          pattern = "\\bagricultura\\b|\\bfamiliar\\b|\\bsustentabilidade\\b|\\balimentar\\b",
          x = Coluna_search,
          ignore.case = TRUE
        )
    )
  return(data_frame_crop)
}

df_remainder_SIOP1 <- df_remainder_SIOP1 %>% mutate(Coluna_search = str_c(acao,plano_orc,funcao,subfuncao,programa,modalidade,
                                                                          und_orc,Fonte,sep = ";"))

result2 <- crop_search_pattern_SIOP(data_frame_SIOP = df_remainder_SIOP1,Coluna_search = Coluna_search)

})

# Exibir o tempo de execução
print(tempo)

##########################################################################
# Codigo para tratar a base de dados SIOP para os anos de 2021 ate 2023  #
#  O tratamento será a limpeza das colunas para retirada de caracteres   #
# Como também a padronização dos dados                                   #
##########################################################################

#Importanto pacotes:
library(tidyverse)
library(stringi)
library(readxl)
siop = read_csv2('./SIOP/SIOP_2021_2023.csv')
siop <- siop %>% slice(-1)
siop%>%glimpse

siop_tratado = siop %>%
  select(-c(`Resultado Primário`, `Projeto de Lei`, `Dotação Inicial`, `Dotação Atual`)) %>%
  mutate(
    Ano = as.numeric(siop$Ano),
    orgao_orc = str_trim(str_replace(str_replace(str_to_lower(stri_trans_general(
        `Órgão Orçamentário`,'Latin-ASCII')), "^[[:alnum:]]{5}", ""), "-", "")),

    funcao = str_trim(str_replace(str_replace(str_to_lower(stri_trans_general(
        Função, "Latin-ASCII")), "^[[:alnum:]]{2}", ""), "-", "")),

    und_orc = str_trim(str_replace(str_replace(str_to_lower(stri_trans_general(
        `Unidade Orçamentária`, "Latin-ASCII")), "^[[:alnum:]]{5}", ""), "-", "")),
    
    programa = str_trim(str_replace(str_replace(str_to_lower(stri_trans_general(
        Programa, "Latin-ASCII")), "^[[:alnum:]]{4}", ""), "-", "")),

    acao = str_trim(str_replace(str_replace(str_to_lower(stri_trans_general(
        Ação, "Latin-ASCII")), "^[[:alnum:]]{4}", ""), "-", "")),

    localizador = str_trim(str_replace(str_replace(str_to_lower(stri_trans_general(
        Localizador, "Latin-ASCII")), "^[[:alnum:]]{4}", ""), "-", "")),
    
    regiao = str_trim(str_replace(str_replace(str_to_lower(stri_trans_general(
        Região, "LAtin-ASCII")), "^[[:alnum:]]{2}", ""), "-", "")),
    
    uf = str_trim(str_replace(str_replace(str_to_lower(stri_trans_general(
        UF, "Latin-ASCII")), "^[[:alnum:]]{2}", ""), "-","")),
    
    municipio_codigo = str_trim(str_extract((stri_trans_general(
        Município, "Latin-ASCII")),"\\d+")),
    
    municipio = str_trim(str_replace(str_replace((str_to_lower(stri_trans_general(
        Município, "Latin-ASCII"))),"^[[:alnum:]]{7}", ""), "-", "")),
    
    plano_orc = str_trim(str_remove(str_remove(str_remove(str_replace(str_to_lower(
        stri_trans_general(`Plano Orçamentário`, "Latin-ASCII")), "^[[:alnum:]]{4}", ""),"-"),"0002 - "),"00se - ")),
    
    grupo_de_despesa = str_trim(str_remove(str_remove(str_to_lower(stri_trans_general(
        `Grupo de Despesa`, "Latin-ASCII")),"^[[:alnum:]]{1}"),"-")),
    
    modalidade = str_trim(str_remove(str_remove(str_to_lower(stri_trans_general(
        `Modalidade de Aplicação`, "Latin-ASCII")), "^[[:alnum:]]{2}"),"-")),
    
    fonte_recursos = str_trim(str_remove(str_remove(str_remove(str_to_lower(stri_trans_general(
        `Fonte`,"Latin-ASCII")),"^[[:alnum:]]{3}"),"^[[:alnum:]]{1}"),"-")),
    
    subfuncao = str_trim(str_remove(str_remove(str_to_lower(stri_trans_general(
        `Subfunção`, "Latin-ASCII")),"^[[:alnum:]]{3}"),"-")),
    
    origem_do_credito = str_trim(str_remove(str_remove(str_to_lower(stri_trans_general(
        `Origem do Crédito`, "Latin-ASCII")), "^[[:alnum:]]{1}"), "-")),
    
    objetivo = str_trim(str_remove(str_remove(str_to_lower(stri_trans_general(
        Objetivo, "Latin-ASCII")),"^[[:alnum:]]{4}"),"-")),
    
    empenhado = as.numeric(Empenhado),

    liquidado = as.numeric(Liquidado)
  )


siop_tratado%>%write_rds('./Siop_Tratado_2021_2023.rds')



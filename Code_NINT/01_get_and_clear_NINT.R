library(tidyverse)
library(stringi)
library(readxl)
library(xlsx)
nint <- read_excel("./brlanduse_landscape2024_dados/NINT/NINT_2015_2024.xlsx")
# Limpando colunas
nint <- nint %>% mutate(
    number = `#` ,
    emissor_trade_name = str_to_lower(stri_trans_general(Emissor, "Latin-ASCII")),
    mercado = str_to_lower(stri_trans_general(Mercado,"Latin-ASCII")),
    instrumento_financeiro = str_to_lower(stri_trans_general(Instrumento,"Latin-ASCII")),
    tipo = str_to_lower(stri_trans_general(Tipo,"Latin-ASCII")),
    categoria = str_to_lower(stri_trans_general(Tema,"Latin-ASCII")),
    tipo_de_emissor = str_to_lower(stri_trans_general(`Tipo de Emissor/Tomador`,"Latin-ASCII")),
    uso_de_recursos = str_to_lower(stri_trans_general(`Uso de Recursos`,"Latin-ASCII")),
    data = year(`Data de emissão`)
) %>%view


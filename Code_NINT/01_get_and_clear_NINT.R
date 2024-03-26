library(tidyverse)
library(stringi)
library(readxl)
library(xlsx)
nint <- read_excel("A:\\finance\\nint\\rawData\\NINT_2015_2024_29_02_2024.xlsx")
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
    data = year(`Data de emissão`),
    moeda = str_to_lower(`Moeda ($)`) ,
    valor = `Valor (M)`,
    prazo_anos = `Prazo (anos)`,
    verificador_externo = `Parecer Independente (SPO)`,
    verificador_externo2 = `Outros Documentos (SPO Framework)`,
    cbi = `Link CBI`,verificador_cbi = `Verificador CBI`
) %>% select(number,emissor_trade_name,mercado,instrumento_financeiro,tipo,categoria,tipo_de_emissor,uso_de_recursos,data,moeda,valor,prazo_anos,verificador_externo,verificador_externo2,cbi,verificador_cbi)


nint %>% write_csv2("A:\\finance\\nint\\cleanData\\nint_clear_19_03_2024.csv")

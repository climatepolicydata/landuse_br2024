##################

########################### Libraries ######################################


library(tidyverse)
library(janitor)
library(stringi)
##### directory #########

dir_fnmc_raw <- ("A:/finance/fnmc_Mma/rawData")

dir_fnmc_clear <- ("A:/finance/fnmc_Mma/cleanData")


##### import datasets #########
setwd(dir_fnmc_raw)

df_fnmc <- read_csv2("projetos-fnmc-2011-a-2023-dados-abertos_02_04_2024_UTF8.csv") %>% as_tibble


df_fnmc_clear <- df_fnmc %>% 
  janitor::clean_names() %>% mutate(no_do_instrumento_de_repasse =str_trim(str_to_lower(stri_trans_general(no_do_instrumento_de_repasse,"Latin-ASCII"))),
    no_processo = str_trim(no_processo),
    nome_do_projeto =str_trim(str_to_lower(stri_trans_general(nome_do_projeto,"Latin-ASCII"))),
    instituicao_executora = str_trim(str_to_lower(stri_trans_general(instituicao_executora,"Latin-ASCII"))),
    uf = str_trim(str_to_lower(uf)),
    tipo_de_instituicao  = str_trim(str_to_lower(stri_trans_general(tipo_de_instituicao,"Latin-ASCII"))))
df_fnmc_clear <- df_fnmc_clear %>% mutate(valor_fnmc = str_remove_all(valor_fnmc, "\\D") %>% as.numeric()/100,
                          valor_contrapartida = str_remove_all(valor_contrapartida, "\\D") %>% as.numeric()/100,
                          valor_total = str_remove_all(valor_total, "\\D") %>% as.numeric()/100,
                          valor_repassado = str_remove_all(valor_repassado, "\\D") %>% as.numeric()/100,
                          valor_nao_repassado = str_remove_all(valor_nao_repassado, "\\D") %>% as.numeric()/100)
df_fnmc_clear %>% glimpse
setwd(dir_fnmc_clear)
df_fnmc_clear%>%view
write_rds(df_fnmc_clear, "fnmc_dados_abertos_Abril_02_04_2024_clear.rds")

# Author : Julia Niemeyer 
# Date:  20/05/2025
# Goal: Rotina para captação dos dados anuais da taxa de cambio

pacman::p_load(readxl,
               openxlsx,
               data.table,
               writexl,
               rbcb,
               stringi,
               tidyr)


## change the initial and final dates according to analysis
date_init <- paste0(ano_ini-1,"-12-31")
if(ano_ini == ano_fim){
date_final <- paste0(ano_fim+1, "-12-31")
} else {
  date_final <- paste0(ano_fim+1, "-12-31")
}

d <- get_currency("USD", date_init, date_final) %>%
  separate(date, into = c("ano", "mes", "dia"), sep = "-") %>%
  filter(mes == "12") %>%
  group_by(ano) %>%
  slice_max(order_by = dia, n = 1, with_ties = FALSE) %>%
  select(ano, bid) %>%
  dplyr::rename("year" = "ano") %>%
  filter(year >= ano_ini & year <= ano_fim)


##export as tabela_cambio.csv to be used in script
write_csv(d, paste0("A:\\projects\\landuse_br2024\\macro_databases\\tabela_cambio_", ano_ini, "-", ano_fim, ".csv"))
cat("nova tabela de cambio escrita para ", ano_ini, "a", ano_fim, "\n")

##################

# Authora : Julia Niemeyer
# Date: 28.05.2025
# Email: julia.niemeyer@cpiglobal.org
# Goal: baixar a base atualizada do SES automaticamente

########################## ACTION NEEDED ######################################
ano_ini = 2024 #the initial year to start analysis
ano_fim = 2024 #the final year to end your analysis


########################### Directories ########################################
root = 'A:\\finance\\ses\\'

dir_susep_dt_clean = paste0(root,'cleanData\\')

dir_susep_raw = paste0(root,'rawData\\')

des_file = paste0(root,'rawData\\', ano_ini, "-", ano_fim)
if(!dir.exists(des_file)){
  dir.create(des_file)
}


options(timeout = 600)

download.file(
  url = "https://www2.susep.gov.br/redarq.asp?arq=BaseCompleta%2ezip",
  destfile = paste0(des_file, "\\Ses_seguros_", ano_ini, "-", ano_fim, ".zip"),
  method = "libcurl", 
  mode = "wb",
  quiet = FALSE
)

# Salvar planilhas em rawData
unzip(paste0(des_file, "\\Ses_seguros_", ano_ini, "-", ano_fim, ".zip"), exdir = dir_susep_raw)

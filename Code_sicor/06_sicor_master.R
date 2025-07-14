##############################################################################
# Author : Renan Florias
# Date: 02.10.2023
# Email: renanflorias@hotmail.com
# Goal: Masterfile that runs all landuse analysis

# Modified by Julia Niemeyer
# Date 22/05/2025

########################### ACTION NEEDED ######################################

# Fill the information to run your analysis
ano_ini = 2019 #the initial year to star analysis
ano_fim = 2024 #the final year to end your analysis (included)
ano_base = 2024 #the year to base inflation


## set the path to your github clone
github <- "Documents"

############## ATUALIZAR SEMPRE #############################
## a linha abaixo está no script 02 dessa forma. Caso for mudar, mudar lá (linha 24)
#arquivo_sicor <- paste0("A:/finance/sicor/cleanData/sicor_main_2013_", ano_fim+1, "_empreendimento.Rds")


"all outputs will be written in a folder called A:/projects/landuse_br2024/sicor/output/ano_ini-ano_fim"


########################### Libraries ######################################
#install.packages("pacman")

pacman::p_load(tidyverse,here, tictoc)

######################### Directories and databases #########################

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

codes_sicor <- paste0(root,github,"/GitHub/landuse_br2024/Code_sicor/")

tic()
# sourcing SICOR codes
source(paste0(codes_sicor,"02_get_filter_sicor.R"))
source(paste0(codes_sicor,"03_sicor_op_basica_dummies.R"))
source(paste0(codes_sicor,"04_sicor_op_basica_filter_dummies.R"))
source(paste0(codes_sicor,"05_sicor_op_basica_transform_landscape.R"))


toc()


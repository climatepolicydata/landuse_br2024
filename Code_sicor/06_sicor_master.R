##############################################################################
# Author : Renan Florias
# Date: 02.10.2023
# Email: renanflorias@hotmail.com
# Goal: Masterfile that runs all landuse analysis

########################### Libraries ######################################
install.packages("pacman")

pacman::p_load(tidyverse,here)
######################### Directories and databases #########################

root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")

github <- readline("digite a pasta do seu repositório clone: ")

codes_sicor <- paste0(root,github,"/GitHub/brlanduse_landscape102023/Code_sicor/")

# sourcing SICOR codes
source(paste0(codes_sicor,"02_get_filter_sicor.R"))
source(paste0(codes_sicor,"03_sicor_op_basica_dummies.R"))
source(paste0(codes_sicor,"04_sicor_op_basica_filter_dummies.R"))
source(paste0(codes_sicor,"05_sicor_op_basica_transform_landscape.R"))


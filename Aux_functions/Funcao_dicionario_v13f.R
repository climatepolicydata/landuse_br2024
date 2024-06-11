###############################################################################
# AUTHORS: Renata Canini, Rafael Gabbay
# E-MAIL : renata.canini@cpiglobal.org, rafael.lopes.gabbay@gmail.com
# CODE OBJECTIVE: automated function to data dictionary generation based o on a
# dataframe
###############################################################################

# AUTOMATED DATA DICTIONARY GENERATION


# function setup

	# directory setup

	dir_teste <- "C:/Users/rgabbay/Dropbox (CPI)/Climate Finance Brazil/01.DATA/variable_data_documentation/tests"
	
	root <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/")
	wd_workf<- paste0(root,"Dropbox (CPI)/Climate Finance Brazil/01.DATA/BCB/3.Temp files/")

	# library setup

	pacman::p_load(tidyverse,
              	writexl,
              	palmerpenguins,
              	janitor)

# The main_sheet function creates a tibble with exploratory information for data cleaning

	f_summarize <- function(df, excel = TRUE){
	  
	 if (excel){ 
	   
	  # Get variable info and NAs
	  var_info <- data.frame(variable = names(df),
	                         variable_type = sapply(df, class),
	                         missing_values_pct = sapply(df, function(y)(sum(length(which(is.na(y)))))/(length(y))),
	                         row.names = NULL)
	  
	  }else{
	    
	   var_info <- data.frame(variable = names(df),
	                          variable_description = "",
	                          variable_type = sapply(df, class),
	                          missing_values_pct = sapply(df, function(y)(sum(length(which(is.na(y)))))/(length(y))),
	                          row.names = NULL)
	   print("Main sheet created")
	  }
	  
	  # Get list of unique variables in each column
	  desc_variables <- data.frame(variable = names(df),
	                               n_unique_opts = sapply(df, function(x)length(unique(x))),
	                               max = sapply(df,function(x)ifelse(is.numeric(x), max(x, na.rm = T), NA)),
	                               min = sapply(df, function(x)ifelse(is.numeric(x), min(x, na.rm = T), NA)),
	                               avg = sapply(df, function(x)ifelse(is.numeric(x), mean(x, na.rm = T), NA)),
	                               sum = sapply(df, function(x)ifelse(is.numeric(x), sum(x, na.rm = T), NA)),
	                               sd  = sapply(df, function(x)ifelse(is.numeric(x), sd(x, na.rm = T), NA)),
	                               var = sapply(df, function(x)ifelse(is.numeric(x), var(x, na.rm = T), NA)),
	                               var_coefficient = sapply(df, function(x)ifelse(is.numeric(x), var(x, na.rm = T)/mean(x, na.rm = T), NA)))
	  
	  # Create the data dictionary
	  d_dict <- as_tibble(merge(var_info, desc_variables, by = "variable", sort = T))
	  return(d_dict)
	}
# The create_dict function is designed to create an excel file with a data dictionary structure, allowing for
# the manual input of descriptions for variables and options of each variable in different worksheets of a single
# excel file
	
	
	create_dict <- function(df, opts_sheets = NULL, write_excel = FALSE, file_name = "") {
	  df <- clean_names(df)
	  # Creates the main sheet (old data dict)
	  main_sheet <- lst("main_sheet" = f_summarize(df, FALSE))
	  # opts_sheet: vector of boolean values (T or F) that defines in order if each variable
	  # will have an opts sheet. F: without opts, T: with opts
	  
	  # Prompting options sheets for each variable
	  
	  if (is.null(opts_sheets)){
	    opts_sheets <- map_chr(colnames(df), function(var)
	      readline(prompt= paste("Do you want an options sheet for variable", var,"? (y|n)")))
	    opts_sheets <- ifelse(opts_sheets == "y", TRUE, FALSE)
	  }
	  print("Prompting done")
	  
	  ## Iteration that for each pair (variable, sheet_ops), creates either an empty
	  ## dataframe (for F sheet_ops) or a correct dataframe (for T sheet_ops). All 
	  ## dataframes are returned in the object all_sheets as a list
	  
	  opts_sheets <- df %>%
	    map2(opts_sheets, function(.x, .y)if (.y == TRUE) {
	      tabyl(.x) %>%
	        mutate(option_description = "")
	    } %>%
	      clean_names())
	  
	 
	  print("Option sheets created")
	  
	  ## Appends main sheet to opts_sheets
	  all_sheets <- c(main_sheet, opts_sheets)
	  
	  # Removes empty opts sheets
	  all_sheets[sapply(all_sheets, is.null)] <- NULL
	  
	  print("Main sheets and option sheets consolidated")
	  
	  ### How to properly add main sheet as first sheet and named "main_sheet"
	  
	  # Creates excel file in current working directory
	  if (write_excel){
	    file_name <- paste0(file_name, ".xlsx")
	    write_xlsx(all_sheets, file_name) # file will be written on current working directory
	  }
	 print("Excel workbook file created")
	 
	 print("Done!")
	 
	}
	

# Tests ########################################################################

setwd(wd_workf)
main_sheet(penguins)



teste <- read.dbf('Ferrovias.dbf', as.is= FALSE)

setwd("A:/projects/landuse_br2024/siop/preview_data")

setwd(dir_bcb)
setwd(dir_susep_dt_clean)

create_dict(siop_resto_2023, write_excel = TRUE, file_name = "dictionary_siop_landscape_2021_2023")


	create_main_sheet(penguins)
	create_dict(penguins, write_excel = TRUE, file_name = "penguins_dictv0")
penguins
	View(main_sheet(dataset))
	
	

require(XLConnect)
require(readxl)
require(gtools)

#Get and set current directory
setwd(getwd())

################################################################################
##function: quantifluor_import(extraction_log)                                ##
##input: the qc/extraction log from which to generate sample sheet for        ##
##       the quantfluor assay                                                 ##
##output: .xlsx spreadsheet that can be used as input into quantifluor assay  ##
##        additionally a warning flag is printed to stdout indicating < 90    ##
##        samples are in the generated samples sheet                          ##
################################################################################
quantifluor_import <- function(extraction_log){
  run_time <- gsub(" ", "_", Sys.time())
  print("Generating Sample Sheet for Quantifluor assay...")
  batch_page <- read_excel(extraction_log, sheet = "batch info", skip=3)
  rack_id <- batch_page$RackID[2] #equivalent to L6 cell
  batch_page['rack barcode'] <- rack_id
  select_samples <- subset(batch_page, !(batch_page$cell %in% c("A1", "C8", "C9", "F4", "F5", "H12")), select=c("rack barcode", "Biobank ID", "Position")) # ignore control wells
  select_samples <- select_samples[mixedorder(select_samples$Position),] # reoder position by A2, A3...etc...
  select_samples['sample #'] <- paste("SPL", row.names(select_samples), sep="") # add incremental sample number prefix SPL
  save_excel <- loadWorkbook(paste("sample_sheet_import_quantifluor_", run_time, ".xlsx", sep=""), create=TRUE) # creates Excel workboot
  createSheet(save_excel, "Sheet 1") # creates sheet in above workbook
  writeWorksheet(object = save_excel, data = select_samples, sheet = "Sheet 1") # write to worksheet
  saveWorkbook(save_excel) # save worksheet
  print ("Successfully generated import sample sheet.  Exiting Program.")
  if (nrow(na.omit(select_samples)) < 90){
    print("WARNING!  Less than 90 samples are in the sample sheet!")
  }
}



################################################################################
##function: quantifluor_export(extaction_log, results_file)                   ##
##input:  extraction/qc log and the results output from quantifluor assay     ##
##output: extraction/qc log populated with results from quantifluor assay and ##
##        flags for mean, CV, counts threshold populated in notes column      ##
################################################################################
quantifluor_export <- function(extraction_log, results_file){
  run_time <- gsub(" ", "_", Sys.time())
  print(paste("Export works", extraction_log, results_file, sep=" "))
  qc_ext_log <- loadWorkbook(extraction_log, create=FALSE)
  setStyleAction(qc_ext_log,XLC$"STYLE_ACTION.NONE")
  qc_ext_log_data <- readWorksheet(qc_ext_log, sheet= "batch info")
  qc_ext_log_subset <- readWorksheet(qc_ext_log, sheet="batch info", startRow =4, endCol = 28 )
  barcode_in_ext_log <- qc_ext_log_subset$RackID[1] 
  quant_results <- loadWorkbook(results_file, create=FALSE)
  fieldGroup_table <- readWorksheet(quant_results, sheet = "Sheet1", startRow=1, startCol = 1, endCol = 2, endRow = 11) # read in table to get barcode info
  barcode_in_results <- fieldGroup_table[which(fieldGroup_table$Field.Group == "Barcode (Plate 1):"), ]$Col2 # barcode info extraction
  if(barcode_in_ext_log == unlist(strsplit(barcode_in_results, split = "-", fixed = T))[1]){
    print("SUCCESS! Barcode in extraction log matches barcode in Quantifluor results file. Continuing to next step...")
  }else{
    stop("FAILED. Barcode mismatch between extraction log and in Quantifluor results file. Exiting program.")
  }
  data_table <- readWorksheet(quant_results, sheet = "Sheet1", startRow = 45, startCol = 1, endCol = 11, endRow = 237) # read in table for actual data
  plate_one_only <- subset(data_table, Plate==1) # subset table above to only include rows where plate == 1; equivalent to even rows of full excel spreadsheet
  
  # TESTING CONDITIONALS
  plate_one_only$flags <- NA
  index_flag_mean_not_exact <- grep(">", plate_one_only$Mean)
  for (i in index_flag_mean_not_exact){
    if (is.na(plate_one_only$flags[i]) == TRUE){
      plate_one_only$flags[i] <- "OOR high"
    }
    else{
      plate_one_only$flags[i] <- paste(plate_one_only$flags[i], "ORR", sep="; ")
    }
  }
  index_flag_mean_threshold <- which(as.numeric(as.character(gsub("<|>", "", plate_one_only$Mean)))<10.0)
  for (i in index_flag_mean_threshold){
    if (is.na(plate_one_only$flags[i]) == TRUE){
      plate_one_only$flags[i] <- "below limit for Infinium"
    }
    else{
      plate_one_only$flags[i] <- paste(plate_one_only$flags[i], "low mean threshold", sep="; ")
    }
  }
  index_flag_count <- which(((plate_one_only$Count=="0") | (plate_one_only$Count=="1"))) # gives indices of where count is 0 or 1 in Count column
  for (i in index_flag_count){
    if (is.na(plate_one_only$flags[i]) == TRUE){
      plate_one_only$flags[i] <- "count is 0 or 1"
    }
    else{
      plate_one_only$flags[i] <- paste(plate_one_only$flags[i], "count is 0 or 1", sep="; ")
    }
  }
  index_flag_cv <- which(grepl("!", plate_one_only$CV....)) # gives indices of where a ! exists in CV column
  for (i in index_flag_cv){
    if (is.na(plate_one_only$flags[i]) == TRUE){
      plate_one_only$flags[i] <- "CV error"
    }
    else{
      plate_one_only$flags[i] <- paste(plate_one_only$flags[i], "CV error", sep="; ") 
    }
  }
  
  index_flag_ext_fail <- which(as.numeric(as.character(gsub("<|>", "", plate_one_only$Mean)))<=0.000) # where mean is literally <0.000 flag as extraction failed
  for (i in index_flag_ext_fail){
    if (is.na(plate_one_only$flags[i]) == TRUE){
      plate_one_only$flags[i] <- "failed extraction"
    }
    else{
      plate_one_only$flags[i] <- paste(plate_one_only$flags[i], "failed extraction", sep="; ") 
    }
  }
  # END TESTING CONDITIONALS
  
  # rename columns so can perform a merge
  names(plate_one_only)[which(names(plate_one_only) == "Name")] <- "Biobank.ID"
  names(plate_one_only)[which(names(plate_one_only) == "Well")] <- "cell"
  plate_one_only$Mean <- gsub(">", "", plate_one_only$Mean) # strip the > symbol
  
  # have place holders for missing wells in quantifluor results file
  missing_wells <- setdiff(qc_ext_log_subset$cell, plate_one_only$cell)
  for (i in missing_wells){
    biobank_id <- qc_ext_log_subset[which(qc_ext_log_subset$cell == i),"Biobank.ID"]
    hold_cell <- data.frame(NA, biobank_id, NA, i, NA, NA, NA, NA, NA, NA, NA, "Missing")
    names(hold_cell) <- names(plate_one_only)
    plate_one_only <- rbind(plate_one_only, hold_cell)
  }
  
  if (length(missing_wells) != 0){
    print("WARNING!  Missing wells/samples from quantiFluor results.  NOT A FULL PLATE!")
  }
  
  # merge data together
  merged_df <- merge(qc_ext_log_subset, plate_one_only, by=c("Biobank.ID", "cell"), all.y = T) # all.y=T to get spaces for controls
  merged_df <- merged_df[mixedorder(merged_df$cell),] # reoder position by A2, A3...etc...
  reorder_all_fields <- subset(merged_df, select=c("run.date.1","user.1", "Mean", "CV....", "flags"))
  writeWorksheet(qc_ext_log, reorder_all_fields, sheet="batch info", startRow = 5, startCol =19, header=FALSE)
  saveWorkbook(qc_ext_log)
  print("Data Transfer Successfully Completed!")
}


################################################################################
##function: export_info(extraction_log)                                       ##
##input:  name of extraction/qc log and then prompts user for more info about ##
##        export method results file                                          ##
##output: returns function call to                                            ##
##        quantifluor_export(extraction_log, results_file) OR                 ##
##        recursively calls export_info(extraction_log) unless user chooses   ##
##        to quit                                                             ##
################################################################################
export_info <- function(extraction_log){
  cat("Name of Quantifluor results file: ")
  results_file <- trimws(readLines("stdin", n=1), which="both");
  cat("You entered the following: ", results_file)
  cat("\n")
  cat("Is this correct? (Y/N/Q) ")
  results_verification <- trimws(readLines("stdin", n=1), which="both")
  if ((toupper(results_verification) == "YES") | (toupper(results_verification) == "Y")){
    return(quantifluor_export(extraction_log = extraction_log, results_file = results_file))
  } else if ((toupper(results_verification) == "NO") | (toupper(results_verification) == "N")){
    return(export_info(extraction_log = extraction_log))
  } else{
    stop("Exiting Program.  Goodbye!")
  }
}



################################################################################
##function: get_user_specifics(extraction_log)                                ##
##input:  name of extraction/qc log and then prompts user for more info       ##
##output: returns function call to export_info(extraction_log) OR             ##
##        quantifluor_import(extraction_log) OR                               ##
##        recursively calls get_user_specifics() unless user chooses to quit  ##
################################################################################
get_user_specifics <- function(extraction_log){
  cat("import into plate reader OR export from plate reader? (select I/i/import/IMPORT or E/e/export/EXPORT):")
  method <- trimws(readLines("stdin", n=1), which="both");
  cat("You entered the following: ", method)
  cat("\n")
  cat("Is this correct? (Y/N/Q)");
  method_verification <- trimws(readLines("stdin", n=1), which="both")
  if (((toupper(method_verification)=="Y") | (toupper(method_verification)=="YES")) & 
    ((toupper(method) == "I") | (toupper(method) == "IMPORT"))){
    return(quantifluor_import(extraction_log))
  } else if (((toupper(method_verification)=="Y") | (toupper(method_verification)=="YES")) & 
             ((toupper(method) == "E") | (toupper(method) == "EXPORT"))){
    return(export_info(extraction_log = extraction_log))
  }else if ((toupper(method_verification)=="N") | (toupper(method_verification)=="NO")){
    return(get_user_specifics(extraction_log = extraction_log))
  }else{
    stop("Exiting Program.  Goodbye!")
  }
}


################################################################################
##function: get_user_input()                                                  ##
##input:  None -- prompts user for extraction/QC log location and name        ##
##output: returns function call get_user_specifics(extraction_log) OR         ##
##        recursively calls get_user_input() unless user chooses to quit      ##
################################################################################
get_user_input <- function(){
  cat("Please enter the name of your extraction and QC batch log file: ");
  input <- trimws(readLines("stdin",n=1), which="both");
  cat("You entered the following: ", input)
  cat("\n")
  cat("Is this correct? (Y/N/Q)");
  verification <- trimws(readLines("stdin", n=1), which="both")
  if (toupper(verification)=='Y' | toupper(verification)=="YES"){
    return(get_user_specifics(extraction_log=input))
  } else if (toupper(verification)=='N' | toupper(verification)=="NO"){
    return(get_user_input())
  } else{
    stop("Exiting program--no files have been modified.  GoodBye!")
  }
}


#####-----------------------------Call and Running the Script----------------------------#####

# calls to functions
get_user_input()

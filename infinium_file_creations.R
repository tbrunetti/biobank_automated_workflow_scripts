require(XLConnect)
require(readxl)
require(gtools)
require(stringr)

#Get and set current directory
setwd(getwd())


################################################################################
##function: create_manifest(batch_log)                                        ##
##input:  called in info_manifest() requires name of infinium batch log xlsx  ##
##        file                                                                ##
##output: outputs a xlsx file that is a formatted infinium sample sheet       ##
################################################################################

create_manifest <- function(batch_log){
  run_time <- gsub(" ", "_", Sys.time())
  batch_log_page <- read_excel(batch_log, sheet = "batch info", skip=3)
  batch_log_page <- batch_log_page[mixedorder(batch_log_page$`row*`, decreasing = FALSE),] # sort by infinum row in ascending order
  
  # rename to manifest output header labels
  names(batch_log_page)[which(names(batch_log_page) == "row*")] <- "Row"
  names(batch_log_page)[which(names(batch_log_page) == "Biobank ID")] <- "Institute Sample Label"
  names(batch_log_page)[which(names(batch_log_page) == "avg [ng/ul]")] <- "Conc (ng/ul)"
  names(batch_log_page)[which(names(batch_log_page) == "Position")] <- "Well"
  # end of rename to manifest output header labels
  
  # create new column headers in final manifest file
  batch_log_page$`Institute Plate Label` <- NA
  batch_log_page$`Is Control` <- NA
  batch_log_page$Species <- "Homo Sapiens"
  batch_log_page$Comments <- NA
  batch_log_page$`Volume (ul)` <- 10
  batch_log_page$`Tissue Source` <- "Blood"
  batch_log_page$`Extraction Method` <- NA
  batch_log_page$`Parent 1` <- NA
  batch_log_page$`Parent 2`<- NA
  batch_log_page$`Replicate(s)`<- NA
  batch_log_page$`WGA Method (if Applicable)` <- NA
  batch_log_page$`Mass of DNA used in WGA` <- NA
  # end of new column headers in final manifest file
  
  new_workbook <- loadWorkbook(paste("infinium_sample_manifest_", run_time, ".xlsx", sep=""), create=TRUE) # create infinium output
  createSheet(new_workbook, "Sample import") # create and name sheet in workbook
 
  batch_log_page$`Is Control`[which(batch_log_page$Well %in% c("A1", "C8", "C9", "F4", "F5", "H12"))] <- 1 # put 1 in is control is match one of control well positions
  
  # change sex to abbreviations
  batch_log_page$Sex[which(toupper(batch_log_page$Sex) == "FEMALE")] <- "F"
  batch_log_page$Sex[which(toupper(batch_log_page$Sex) == "MALE")] <- "M"
  batch_log_page$Sex[which((toupper(batch_log_page$Sex) != "FEMALE") & (toupper(batch_log_page$Sex) != "MALE"))] <- "U"
  
  # print warning message to screen is number of samples does not equal 96 or 192
  if ((nrow(batch_log_page) != 96) | (nrow(batch_log_page) != 192)){
    print("WARNING!!!  Total samples does equal 96 or 192")
  }
  
  heading <- data.frame(c("Institute", "Date Received", "Comments"), c("CCPM Biobank", NA, NA))
  writeWorksheet(new_workbook, heading, sheet="Sample import", startRow = 1, startCol = 1, header=FALSE)
  
  final_dataframe <- subset(batch_log_page, select=c("Row", "Institute Plate Label", "Well", "Is Control","Institute Sample Label", 
                                  "Species", "Sex", "Comments","Race", "Ethnicity", "Volume (ul)", "Conc (ng/ul)", 
                                  "Tissue Source", "Extraction Method", "Parent 1", "Parent 2", "Replicate(s)", 
                                  "WGA Method (if Applicable)", "Mass of DNA used in WGA"))

  writeWorksheet(new_workbook, final_dataframe, sheet= "Sample import", startRow = 5, startCol = 1, header=TRUE)
  saveWorkbook(new_workbook)
} 



################################################################################
##function: batch_log(extraction_log, redcap)                                 ##
##input:  called in info_batch_log(); input name of extraction_qc log xlsx    ##
##        file (comma separated if list of them) and a redcap export xlsx     ##
##        file (comma separated if a list of them in the same order as the    ##
##        extraction_qc log file list)                                        ##
##output: outputs xlsx that follows format of Infinium Batch Log file; one    ##
##        file is generated no matter how batches are input -- all inputs     ##
##        are concatenated together in one batch file                         ##
################################################################################

batch_log <- function(extraction_log, redcap){
  run_time <- gsub(" ", "_", Sys.time())
  # reads in the template and makes a copy and renames it to become final Infinium batch log output file
  file.copy("/home/brunettt/Desktop/Biobank_workflow_automation/Infinium/R-7_Infinium_batch_log_template.xlsx", paste("/home/brunettt/Desktop/Biobank_workflow_automation/Infinium/", "Infinium_Batch_Log_", run_time, ".xlsx", sep=""))
  final_batch_log <- loadWorkbook(paste("/home/brunettt/Desktop/Biobank_workflow_automation/Infinium/", "Infinium_Batch_Log_", run_time, ".xlsx", sep=""), create=FALSE)
  setStyleAction(final_batch_log,XLC$"STYLE_ACTION.NONE")
  final_batch_log_subset <- readWorksheet(final_batch_log, sheet="batch info", startRow = 4, startCol = 1, header=TRUE)
  batch_plates <- unlist(strsplit(extraction_log, split=",", fixed=TRUE))
  redcap_file <- loadWorkbook(redcap, create=FALSE)
  redcap_file_subset <- readWorksheet(redcap_file, sheet="CCPMBiobankSamples_DATA_2017-10")
  names(redcap_file_subset)[which(names(redcap_file_subset) == "btid")] <- "Biobank.ID" # rename redcap btid column name
  total_batches_remaining <- length(batch_plates)
  index_to_maintain_batch_order = total_batches_remaining + 1
  more_than_one = index_to_maintain_batch_order - total_batches_remaining
  
  final_concatenated_batches <- data.frame()
  # iterate through as many batches as provided by user
  while (total_batches_remaining != 0){
    load_batch_ext_log <- loadWorkbook(batch_plates[(index_to_maintain_batch_order - total_batches_remaining)], create=FALSE)
    read_batch_ext_log_subset <- readWorksheet(load_batch_ext_log, sheet = "batch info", startRow = 4, startCol = 1, header = TRUE)
    read_batch_ext_log_subset$num_well <- as.numeric(as.character(str_match(read_batch_ext_log_subset$cell, pattern = ("[0-9]{1,2}"))))
    read_batch_ext_log_subset$letter_well <-  str_match(read_batch_ext_log_subset$cell, pattern = ("[A-Za-z]"))
    sorted_by_infinium_row <- read_batch_ext_log_subset[order(read_batch_ext_log_subset$num_well, read_batch_ext_log_subset$letter_well),] # sort cells so can add infinium row
    sorted_by_infinium_row$inf_row <- seq(1,nrow(sorted_by_infinium_row)) # adds infinium row numbers 1-96 
    
    # recodes row numbers if more than one batch exists
    if (more_than_one > 1){
      sorted_by_infinium_row$inf_row <- ((index_to_maintain_batch_order - total_batches_remaining)*96) + as.numeric(as.character(sorted_by_infinium_row$inf_row))
      add_redcap <- merge(x=sorted_by_infinium_row, y=redcap_file_subset, all.x=TRUE, by="Biobank.ID") # left merge on Biobank ID column
      add_redcap <- add_redcap[mixedorder(add_redcap$Position, decreasing = FALSE),]
      add_redcap_subset <- subset(add_redcap, c("avg..ng.ul.", "notes"))
      add_redcap_subset2 <- subset(add_redcap, c("sex", "race", "ethnicity"))
      writeWorksheet(final_batch_log, add_redcap$inf_row, startCol = 1, header=F, startRow = (((index_to_maintain_batch_order - total_batches_remaining)*96) +6))
      writeWorksheet(final_batch_log, add_redcap$Biobank.ID, startCol = 3, header=F, startRow = (((index_to_maintain_batch_order - total_batches_remaining)*96) +6))
      writeWorksheet(final_batch_log, add_redcap_subset, startCol = 12, header = F, startRow = (((index_to_maintain_batch_order - total_batches_remaining)*96) +6))
      writeWorksheet(final_batch_log, add_redcap_subset2, startCol = 15, header = F, startRow = (((index_to_maintain_batch_order - total_batches_remaining)*96) +6))
    }else{
    add_redcap <- merge(x=sorted_by_infinium_row, y=redcap_file_subset, all.x=TRUE, by="Biobank.ID") # left merge on Biobank ID column
    add_redcap <- add_redcap[mixedorder(add_redcap$Position, decreasing = FALSE),]
    add_redcap_subset <- subset(add_redcap, c("avg..ng.ul.", "notes"))
    add_redcap_subset2 <- subset(add_redcap, c("sex", "race", "ethnicity"))
    writeWorksheet(final_batch_log, add_redcap$inf_row, startCol = 1, header=F, startRow = 5)
    writeWorksheet(final_batch_log, add_redcap$Biobank.ID, startCol = 3, header=F, startRow = 5)
    writeWorksheet(final_batch_log, add_redcap_subset, startCol = 12, header=F, startRow = 5)
    writeWorksheet(final_batch_log, add_redcap_subset2, startCol = 15, header = F, startRow = 5)
    }
    
    total_batches_remaining <- total_batches_remaining - 1 # after finished with batch, subtract from total number of batches remaining
  }
  
  saveWorkbook(final_batch_log)
  print("Successfully finshed creating Infinium batch log!")
}



################################################################################
##function: info_manifest()                                                   ##
##input:  None -- prompts user for information regarding name of infinium     ##
##        batch log xlsx file                                                 ##
##output: if verified batch log file is correct, calls function               ##                
##        create_manifest(batch_log); if no is selected, recursively calls    ##
##        info_manifest() unless user quits which results in                  ##
##        termination of program                                              ##
################################################################################

info_manifest <- function(){
  cat("Name of Infinium batch log: ");
  batch_log <- trimws(readLines("stdin", n=1), which="both")
  cat("You entered the following: ", batch_log);
  cat('\n')
  cat("Is this correct? (Y/N/Q) ")
  verification <- trimws(readLines("stdin", n=1), which="both")
  if ((toupper(verification) == "Y") | (toupper(verification)=="YES")){
    return(create_manifest(batch_log = batch_log))
  }else if ((toupper(verification) == "N") | (toupper(verification)=="NO")){
    return(info_manifest())
  }else{
    stop("Exiting program.  Good Bye!")
  }
}


################################################################################
##function: info_batch_log()                                                  ##
##input:  None -- prompts user for method i.e. create infinium batch log OR   ##
##        create infinium sample manifest                                     ##
##output: returns function call to batch_log(extraction_log, redcap) OR       ##
##        recurvisely calls info_batch_log() unless user requests to exit     ##
################################################################################

info_batch_log <- function(){
  cat("Name of extraction/qc log(s); if more than one separate names with comma: ")
  file_names <- trimws(readLines("stdin", n=1), which="both")
  cat("You entered the following: ", file_names)
  cat('\n')
  cat("Is this correct? (Y/N/Q)")
  verification_1 <- trimws(readLines("stdin", n=1), which="both")
  if ((toupper(verification_1) == "YES") | (toupper(verification_1) == "Y")){
    check_redcap <- function(){
    cat("Name of REDCap data export file: ");
    redcap <- trimws(readLines("stdin", n=1), which = "both")
    cat("You entered the following: ", redcap)
    cat('\n')
    cat("Is this correct? (Y/N/Q) ")
    verification_2 <- trimws(readLines("stdin", n=1), which="both")
    return(list(redcap, verification_2))
    }
    output <- check_redcap()
    if ((toupper(output[2]) == "YES") | (toupper(output[2]) == "Y")) {
      return(batch_log(extraction_log = file_names, redcap = output[1]))
    }else if ((toupper(output[2]) == "NO") | (toupper(output[2]) == "N")){
      return(output <- check_redcap())
    }else{
      stop("Exiting Program. Good Bye!")
    }
  }
}


################################################################################
##function: get_user_input()                                                  ##
##input:  None -- prompts user for method i.e. create infinium batch log OR   ##
##        create infinium sample manifest                                     ##
##output:                                                                     ##
################################################################################

get_user_input <- function(){
  cat("Create infinium batch log (LOG/log/l/L) or create infinium sample manifest (manifest/MANIFEST/M/m):  ");
  method <- trimws(readLines("stdin", n=1), which="both");
  cat("You entered the following:  ", method)
  cat("\n")
  cat("Is this correct? (Y/N/Q) ");
  verification <- trimws(readLines("stdin", n=1), which="both");
  if (((toupper(verification) == "Y") | (toupper(verification) == "YES")) & 
    ((toupper(method) == "LOG") | (toupper(method) == "L"))){
      return(info_batch_log())
    }else if (((toupper(verification) == "Y") | (toupper(verification) == "YES")) & 
  ((toupper(method) == "MANIFEST") | (toupper(method) == "M"))){
      return(info_manifest())
    }else if ((toupper(verification) == "NO") | (toupper(verification) =="N")){
      return(get_user_input())
    }else{
      stop("Exiting Program.  Good Bye!")
    }
}

#####-----------------------------Call and Running the Script----------------------------#####

# calls to funtions
get_user_input()

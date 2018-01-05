# UPDATED FROM BIOBANK
## BEFORE USING:
##1.  Make sure BTID/BIOBANK IDs are unique to each sample
##2.  Make sure the qc/batch log infinium template is in the same location as this script
options(java.parameters = "-Xmx4g")
require(XLConnect)
require(readxl)
require(gtools)
require(stringr)
xlcFreeMemory()

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
  run_time <- gsub("_([0-9]{1,2}):", "_\\1h", run_time)
  run_time <- gsub("([0-9]{1,2}):", "\\1m", run_time)
  run_time <- paste(run_time, "s", sep="")
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
  
  file_name <- unlist(strsplit(batch_log, split=".", fixed=TRUE))[1]
  new_workbook <- loadWorkbook(paste(file_name, "_infinium_sample_manifest_", run_time, ".xlsx", sep=""), create=TRUE) # create infinium output
  createSheet(new_workbook, "Sample import") # create and name sheet in workbook
 
  batch_log_page$`Is Control`[which(batch_log_page$Well %in% c("A1", "C8", "C9", "F4", "F5", "H12"))] <- 1 # put 1 in is control is match one of control well positions
  
  # start of loop to check if Wells need to be padded with 0
  for (i in 1:length(batch_log_page$Well)){
    if (grepl("[0-9]{2}", batch_log_page$Well[i])){
      next
    }else{
      padded <- paste(substr(x=batch_log_page$Well[i], start=1, stop=1), "0", substr(x=batch_log_page$Well[i], start=2, stop=2),sep="")
      batch_log_page$Well[i] <- padded
    }
  }
  # end of loop to check if Wells need to be padded with 0
  
  # change sex to abbreviations
  batch_log_page$Sex[which(toupper(batch_log_page$Sex) == "FEMALE")] <- "F"
  batch_log_page$Sex[which(toupper(batch_log_page$Sex) == "MALE")] <- "M"
  batch_log_page$Sex[which((toupper(batch_log_page$Sex) != "F") & (toupper(batch_log_page$Sex) != "M") | (is.na(batch_log_page$Sex) == TRUE))] <- "U"
  
  # print warning message to screen is number of samples does not equal 96 or 192
  if (((length(batch_log_page$Row) == 96) | (length(batch_log_page$Row) == 192)) & (("Missing" %in% batch_log_page$notes) == FALSE)){
    print("Checking for missing samples...PASS!")
  }else{
    print("WARNING!!!  Total samples does not equal 96 or 192")
    print(paste("Total lines in manifest file (not including headers):", length(batch_log_page$Row), sep=" "))
    print("Please check the following wells:")
    print(as.list(batch_log_page[grep("Missing", batch_log_page$notes), "Well"]))
  }
  
  heading <- data.frame(c("Institute", "Date Received", "Comments"), c("CCPM Biobank", NA, NA))
  writeWorksheet(new_workbook, heading, sheet="Sample import", startRow = 1, startCol = 1, header=FALSE)
  
  final_dataframe <- subset(batch_log_page, select=c("Row", "Institute Plate Label", "Well", "Is Control","Institute Sample Label", 
                                  "Species", "Sex", "Comments","Race", "Ethnicity", "Volume (ul)", "Conc (ng/ul)", 
                                  "Tissue Source", "Extraction Method", "Parent 1", "Parent 2", "Replicate(s)", 
                                  "WGA Method (if Applicable)", "Mass of DNA used in WGA"))

  writeWorksheet(new_workbook, final_dataframe, sheet= "Sample import", startRow = 5, startCol = 1, header=TRUE)
  saveWorkbook(new_workbook)
  print("Successfully generated manifest file.  Goodbye!")
} 



################################################################################
##function: batch_log(extraction_log, redcap)                                 ##
##input:  called in info_batch_log(); input name of extraction_qc log xlsx    ##
##        file (comma separated if list of them) and a redcap export xlsx     ##
##        file (comma separated if a list of them in the same order as the    ##
##        extraction_qc log file list)                                        ##
##output: outputs xlsx that follows format of Infinium Batch Log file; one    ##
##        file is generated no matter how batches are input -- all inputs     ##
##        are concatenated together into one batch file                       ##
################################################################################

batch_log <- function(extraction_log, redcap){
  run_time <- gsub(" ", "_", Sys.time())
  run_time <- gsub("_([0-9]{1,2}):", "_\\1h", run_time)
  run_time <- gsub("([0-9]{1,2}):", "\\1m", run_time)
  run_time <- paste(run_time, "s", sep="")
  if (length(unlist(strsplit(extraction_log, split=",", fixed=TRUE))) > 1){
    file_name <- ""
    batch_names <- unlist(strsplit(extraction_log, split=",", fixed=TRUE))
    for (i in 1:length(batch_names)){
      each_file <- unlist(strsplit(batch_names[i], split = ".", fixed=TRUE))[1]
      if (i > 1){
        file_name <- paste(file_name, "_", each_file, sep="")
      }else{
        file_name <- each_file
      }
    }
    }else{
      file_name <- unlist(strsplit(extraction_log, split=".", fixed=TRUE))[1]
    }

  # reads in the template and makes a copy and renames it to become final Infinium batch log output file
  file.copy("R-7_Infinium_batch_log_template.xlsx", paste(file_name, "_Infinium_Batch_Log_", run_time, ".xlsx", sep=""))
  final_batch_log <- loadWorkbook(paste(file_name, "_Infinium_Batch_Log_", run_time, ".xlsx", sep=""), create=FALSE)
  setStyleAction(final_batch_log,XLC$"STYLE_ACTION.NONE")
  final_batch_log_subset <- readWorksheet(final_batch_log, sheet="batch info", startRow = 4, startCol = 1, header=TRUE)
  batch_plates <- unlist(strsplit(extraction_log, split=",", fixed=TRUE))
  redcap_file <- loadWorkbook(redcap, create=FALSE)
  print(paste("Reading REDCap Data from Sheet: ", getSheets(redcap_file)[1], sep=""))
  redcap_file_subset <- readWorksheet(redcap_file, sheet=getSheets(redcap_file)[1])
  names(redcap_file_subset)[which(names(redcap_file_subset) == "gender")] <- "sex" # rename redcap gender column name
  names(redcap_file_subset)[which(names(redcap_file_subset) == "btid")] <- "Biobank.ID" # rename redcap btid column name
  
  # check for duplicate Biobank IDs in REDCap file
  # if a BTID is duplicated in REDCap, batch log will not be generated
  if (any(duplicated(redcap_file_subset$Biobank.ID)) == TRUE){
    file.remove(paste(file_name, "_Infinium_Batch_Log_", run_time, ".xlsx", sep=""))
    stop(paste("WARNING!  Duplicate BTIDs exist in the REDCap file that may be contradictory.  Please check the following BTIDs and remove the duplicate entries.",
               redcap_file_subset$Biobank.ID[which(duplicated(redcap_file_subset$Biobank.ID))], "Batch log will not be generated.  Exiting program. Goodbye!", sep = "\n"))
  }
  
  total_batches_remaining <- length(batch_plates)
  index_to_maintain_batch_order = total_batches_remaining + 1
  more_than_one = total_batches_remaining
  final_concatenated_batches <- data.frame()
  
  # iterate through as many batches as provided by user
  while (total_batches_remaining != 0){
    load_batch_ext_log <- loadWorkbook(batch_plates[(index_to_maintain_batch_order - total_batches_remaining)], create=FALSE)
    setStyleAction(load_batch_ext_log,XLC$"STYLE_ACTION.NONE")
    read_batch_ext_log_subset <- readWorksheet(load_batch_ext_log, sheet = "batch info", startRow = 4, startCol = 1, header = TRUE)
    read_batch_ext_log_subset$Time <- sub(".* ", "", as.character(read_batch_ext_log_subset$Time))
    read_batch_ext_log_subset$num_well <- as.numeric(as.character(str_match(read_batch_ext_log_subset$cell, pattern = ("[0-9]{1,2}"))))
    read_batch_ext_log_subset$letter_well <-  str_match(read_batch_ext_log_subset$cell, pattern = ("[A-Za-z]"))
    sorted_by_infinium_row <- read_batch_ext_log_subset[order(read_batch_ext_log_subset$num_well, read_batch_ext_log_subset$letter_well),] # sort cells so can add infinium row
    sorted_by_infinium_row$inf_row <- seq(1,nrow(sorted_by_infinium_row)) # adds infinium row numbers 1-96 
    
    # Create exception: no batch log created if BTID is missing (checks this per batch/file)
    # missing means multiple of 90 does not exist; defined as cell that does not start with a number 0-9)
    if ((length(grep("^[0-9]", read_batch_ext_log_subset$Biobank.ID, perl=TRUE, value=FALSE)) %% 90) != 0){
      file.remove(paste(file_name, "_Infinium_Batch_Log_", run_time, ".xlsx", sep=""))
      stop("Missing BTIDs present.  Batch log will not be created until this issue is resolved.  Exiting program.  Goodbye!")
    }
    # end of exception #
    
    # recodes row numbers if more than one batch exists
    if (more_than_one > 1){
      sorted_by_infinium_row$inf_row <- ((index_to_maintain_batch_order - total_batches_remaining-1)*96) + as.numeric(as.character(sorted_by_infinium_row$inf_row))
      add_redcap <- merge(x=sorted_by_infinium_row, y=redcap_file_subset, all.x=TRUE, by="Biobank.ID") # left merge on Biobank ID column
      add_redcap <- add_redcap[mixedorder(add_redcap$Position, decreasing = FALSE),]
      add_redcap_subset <- subset(add_redcap, select = c("avg..ng.ul.", "notes"))
      add_redcap_subset2 <- subset(add_redcap, select = c("sex", "race", "ethnicity"))
      add_redcap_subset3 <- subset(add_redcap, select = c("Barcode", "Date", "Time", "User"))
      writeWorksheet(final_batch_log, add_redcap$inf_row, sheet = "batch info", startCol = 1, header=F, startRow = (((index_to_maintain_batch_order - total_batches_remaining-1)*96) +5))
      writeWorksheet(final_batch_log, add_redcap$Biobank.ID, sheet = "batch info", startCol = 3, header=F, startRow = (((index_to_maintain_batch_order - total_batches_remaining-1)*96) +5))
      writeWorksheet(final_batch_log, add_redcap$Position, sheet = "batch info", startCol = 6, header=F, startRow = (((index_to_maintain_batch_order - total_batches_remaining-1)*96) +5))
      writeWorksheet(final_batch_log, add_redcap_subset, startCol = 12, sheet = "batch info", header = F, startRow = (((index_to_maintain_batch_order - total_batches_remaining-1)*96) +5))
      writeWorksheet(final_batch_log, add_redcap_subset2, startCol = 15, sheet = "batch info", header = F, startRow = (((index_to_maintain_batch_order - total_batches_remaining-1)*96) +5))
      writeWorksheet(final_batch_log, add_redcap$instrument.id, startCol = 2, sheet = "batch info", header = F, startRow = (((index_to_maintain_batch_order - total_batches_remaining-1)*96) +5))
      writeWorksheet(final_batch_log, add_redcap$RackID, startCol = 5, sheet = "batch info", header = F, startRow = (((index_to_maintain_batch_order - total_batches_remaining-1)*96) +5))
      writeWorksheet(final_batch_log, add_redcap_subset3, startCol = 7, sheet = "batch info", header = F, startRow = (((index_to_maintain_batch_order - total_batches_remaining-1)*96) +5))
    }else{
    add_redcap <- merge(x=sorted_by_infinium_row, y=redcap_file_subset, all.x=TRUE, by="Biobank.ID") # left merge on Biobank ID column
    add_redcap <- add_redcap[mixedorder(add_redcap$Position, decreasing = FALSE),]
    add_redcap_subset <- subset(add_redcap, select = c("avg..ng.ul.", "notes"))
    add_redcap_subset2 <- subset(add_redcap, select = c("sex", "race", "ethnicity"))
    add_redcap_subset3 <- subset(add_redcap, select = c("Barcode", "Date", "Time", "User"))
    writeWorksheet(final_batch_log, add_redcap$inf_row, sheet = "batch info", startCol = 1, header=F, startRow = 5)
    writeWorksheet(final_batch_log, add_redcap$Biobank.ID, sheet = "batch info", startCol = 3, header=F, startRow = 5)
    writeWorksheet(final_batch_log, add_redcap$Position, sheet = "batch info", startCol = 6, header= F, startRow = 5)
    writeWorksheet(final_batch_log, add_redcap_subset, startCol = 12, sheet = "batch info", header=F, startRow = 5)
    writeWorksheet(final_batch_log, add_redcap_subset2, startCol = 15, sheet = "batch info", header = F, startRow = 5)
    writeWorksheet(final_batch_log, add_redcap$instrument.id, startCol = 2, sheet = "batch info", header = F, startRow = 5)
    writeWorksheet(final_batch_log, add_redcap$RackID, startCol = 5, sheet = "batch info", header = F, startRow =5)
    writeWorksheet(final_batch_log, add_redcap_subset3, startCol = 7, sheet = "batch info", header = F, startRow = 5)
    }
    
    total_batches_remaining <- total_batches_remaining - 1 # after finished with batch, subtract from total number of batches remaining
  }
  
  saveWorkbook(final_batch_log)
  print("Successfully finshed creating Infinium batch log!")
}

################################################################################
##function: update_log(batch_log, redcap)                                     ##
##input:  called in info_update_log();name of already created infinium        ## 
##        batch log (.xlsx) and a redcap export xlsx file                     ##
##output: outputs xlsx that follows format of Infinium Batch Log file with    ##
##        updated REDCap Information for select wells                         ##
################################################################################
update_log <- function(batch_log, redcap){
  # get_ids is a function prompting user to input BTIDs that need updating
  get_ids <- function(){
    cat("comma separted list (no spaces) of Biobank IDs that need to be updated: ")
    ids <- trimws(readLines("stdin", n=1), which="both")
    cat("Is this correct? (Y/N/Q) ")
    verification <- trimws(readLines("stdin", n=1), which="both")
    if ((toupper(verification) =="Y") | (toupper(verification) == "YES")){
      return(update(batch_log = batch_log, redcap = redcap, ids = ids))
    }else if((toupper(verification) =="N") | (toupper(verification) == "NO")){
      return(get_ids())
    }else{
      stop("Exiting program.  Goodbye!")
    }
  }
  
  # update is a function that actually performs the physical update to the batch log spreadsheet
  update <- function(batch_log, redcap, ids){
    batch_log_to_update <- loadWorkbook(batch_log, create=FALSE)
    setStyleAction(batch_log_to_update,XLC$"STYLE_ACTION.NONE")
    read_batch_info_sheet <- readWorksheet(batch_log_to_update, sheet = "batch info", header=FALSE)
    ids_to_update = trimws(strsplit(ids, ",")[[1]], which="both") # parse user input of BTIDs to update and trim any trailing whitespace between commas from user input
      
    load_redcap <- loadWorkbook(redcap, create=FALSE)
    redcap_sheet <- readWorksheet(load_redcap, sheet = getSheets(load_redcap)[1])
    
    # for each BTID input by user, search in redcap file and update batch log file accordingly
    # exceptions caught:  multiple BTIDs exist or BTID does not exist in either batch log file or REDCap file
    for (btid in ids_to_update){
      if(nrow(redcap_sheet[which(redcap_sheet$btid %in% btid),]) == 1){
        writeWorksheet(batch_log_to_update, data = redcap_sheet[which(redcap_sheet$btid %in% btid),"gender"], sheet = "batch info", 
                       startRow = which(read_batch_info_sheet$Col3 %in% btid), startCol = 15, header=FALSE)
        writeWorksheet(batch_log_to_update, data = redcap_sheet[which(redcap_sheet$btid %in% btid),"race"], sheet = "batch info", 
                       startRow = which(read_batch_info_sheet$Col3 %in% btid), startCol = 16, header=FALSE)
        writeWorksheet(batch_log_to_update, data = redcap_sheet[which(redcap_sheet$btid %in% btid),"ethnicity"], sheet = "batch info", 
                       startRow = which(read_batch_info_sheet$Col3 %in% btid), startCol = 17, header=FALSE)
      }else if (nrow(redcap_sheet[which(redcap_sheet$btid %in% btid),]) > 1){
        stop(paste("Multiple BTIDs exist in REDCap for the following BTID:", btid, "\n",
                 "Please check the REDCap file to determine which entry is correct and delete the duplicate(s) and rerun this script", sep = " "))
      }else{
        stop(paste("The following BTID does not exist in either the batch log file or within the REDCap file:", btid, "\n",
                   "Please ascertain this BTID is present in both files", sep = " "))
      }
    }
    saveWorkbook(batch_log_to_update) #overwrite existing
    print("Successfully finished updating batch log file!")
  }
  # initiate function call
  get_ids()
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
    stop("Exiting program.  Goodbye!")
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
      return(batch_log(extraction_log = file_names, redcap = unlist(output[1])))
    }else if ((toupper(output[2]) == "NO") | (toupper(output[2]) == "N")){
      return(output <- check_redcap())
    }else{
      stop("Exiting Program. Goodbye!")
    }
  }
}


################################################################################
##function: info_update_log()                                                 ##
##input:  None -- prompts user for more information for updating an already   ##
##        existing infinium batch log (.xlsx)                                 ##
##output: returns function call to update_log(batch_log, redcap) OR           ##
##        recurvisely calls info_update_log() unless user requests to exit    ##
################################################################################
info_update_log <- function(){
  cat("Name of infinium batch log to update: ")
  log_file <- trimws(readLines("stdin", n=1), which="both")
  cat("You entered the following: ", log_file)
  cat("\n")
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
      return(update_log(batch_log = log_file, redcap = unlist(output[1])))
    }
    else if((toupper(output[2]) == "NO") | (toupper(output[2]) == "N")){
      return(output <- check_redcap())
    }else{
      stop("Exiting program.  Goodbye!")
    }
  }else if((toupper(verification_1) == "NO") | (toupper(verification_1) =="N")){
    return(info_update_log())
  }else{
    stop("Exiting program.  Goodbye!")
  }
}



################################################################################
##function: get_user_input()                                                  ##
##input:  None -- prompts user for method i.e. create infinium batch log OR   ##
##        create infinium sample manifest                                     ##
##output: If method selected is infinium batch log and all info is correctly  ##
##        entered, calls function info_batch_log(); if method selected is     ##
##        infinium sample manifest and all info is correctly entered, calls   ##
##        function info_manifest(); else continues to prompt user for info    ##
##        until user selects quit or exits program                            ##
################################################################################

get_user_input <- function(){
  cat("Create infinium batch log (LOG/log/l/L) or create infinium sample manifest (manifest/MANIFEST/M/m) or update an existing infinium batch log (UPDATE/update/U/u):  ");
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
    }else if (((toupper(verification) == "Y") | (toupper(verification) == "YES")) & 
      ((toupper(method) == "UPDATE") | (toupper(method) == "U"))){
      return(info_update_log())
    }else if ((toupper(verification) == "NO") | (toupper(verification) =="N")){
      return(get_user_input())
    }else{
      stop("Exiting Program.  Goodbye!")
    }
}

#####-----------------------------Call and Running the Script----------------------------#####

# calls to funtions
get_user_input()

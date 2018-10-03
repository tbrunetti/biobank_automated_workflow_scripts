require(stringr)
require(plyr)
require(tidyr)
################################################################################
##function: generate_manifest(manifestPath, detailedPath, redcapPath)         ##
##input: passed variables from func get_redcap_input()                        ##
##output: if successful, generates a manifest for BCP with file name          ##
##        projectName_BCPmanifest.csv in current working directory; if not    ##
##        manifest will not be created and an error message will print to     ##
##        the screen                                                          ##        
################################################################################
setwd(getwd())

generate_manifest <- function(manifestPath, detailedPath, redcapPath){
  manifestFile <- read.csv(manifestPath, header=TRUE, skip = 4)
  detailedFile <- read.csv(detailedPath, header=TRUE)
  redcapFile <- read.csv(redcapPath, header = TRUE)
  
  # copy sample id columns so sample matching is consistent 
  manifestFile$Sample_ID <- as.character(manifestFile$Institute.Sample.Label)
  detailedFile$Sample_ID <- as.character(detailedFile$Sample)
  redcapFile$Sample_ID <- as.character(redcapFile$btid)
  
  # copy sample plate columns so sample matching is consistent (important if running more than 96 samples)
  manifestFile$Sample_Plate <- as.character(manifestFile$Institute.Plate.Label)
  detailedFile$Sample_Plate <- as.character(detailedFile$Sample.Plate)
  
  combined_set <- merge(detailedFile, redcapFile, by="Sample_ID", all.x = TRUE)
  all <- merge(manifestFile, combined_set, by = c("Sample_ID", "Sample_Plate"), all.x = TRUE)
  all$SentrixBarcode_A <- as.character(all$BeadChip)
  all$SentrixPosition_A <- paste(all$SSR, all$SSC, sep="")
  all$Sample_Well <- as.character(all$Well.x)
  all$Sample_Name <- as.character(all$Sample_ID)
  all$`Tissue Source` <- as.character(all$Tissue.Source)
  all$Concentration <- as.character(all$Conc..ng.ul.)
  all$`Extraction Method` <- as.character(all$Extraction.Method)
  all$`Wga Method` <- as.character(all$WGA.Method..if.Applicable.)
  all$Comment <- as.character(all$Comments)
  all$Instrument_ID <- as.character(all$instrumentid1)
  all$Race_intermediate <- all$Race
  all$Race <- paste(all$Race_intermediate, all$ethnicity, sep = ' ')
  all$MRN <- as.character(all$mrn)
  all$Replicate <- as.character(all$Replicate.s.)
  all$Parent1 <- as.character(all$Parent.1)
  all$Parent2 <- as.character(all$Parent.2)
  all$exclude <- "0"
  all$Species<- as.character(all$Species)
  all$Name<-as.character(all$name)
  
  # date of birth manipulations
  all$dobString <- as.character(all$dob)
  all<-separate(all, col = "dobString", sep = "/", into = c("month", "day", "year"))
  for (i in seq(1, length(all$month))){
    if (is.na(nchar(all$month[i]))){
      all[i, "monthPad"] <- "00"
      }else if (nchar(all$month[i]) < 2){
      all[i, "monthPad"] <- paste("0", all$month[i], sep="")
    }else{
      all[i, "monthPad"] <- all$month[i]
    }
  }
  
  for (i in seq(1, length(all$day))){
    if (is.na(nchar(all$day[i]))){
      all[i, "dayPad"] <- "00"
      all[i, "yearPad"] <- "0000"
    }
    else if (nchar(all$day[i]) != 2){
      all[i, "dayPad"] <- paste("0", all$day[i], sep="")
      all[i, "yearPad"] <- all$year[i]
    }else{
      all[i, "dayPad"] <- all$day[i]
      all[i, "yearPad"] <- all$year[i]
    }
  }
  
  all$DOB <-paste(all$monthPad, all$dayPad, all$yearPad, sep='-')
  
  
  # gender/sex manipulations
  all$GenderSampleSheet <-as.character(all$Sex)
  for (i in seq(1, length(all$GenderSampleSheet))){
    if (toupper(as.character(all$GenderSampleSheet[i])) == "M"){
      all[i, "Gender"] <- "Male"
    }else if (toupper(as.character(all$GenderSampleSheet[i])) == "F"){
      all[i, "Gender"] <- "Female"
    }else{
      all[i, "Gender"] <- "Unknown"
    }
  }
  
  # name manipulations
  all[which(is.na(all$Name)), "Name"] <- all$Sample_ID[which(is.na(all$Name))]
  all$Name <- paste0("\"", all$Name, "\"")
  
  
  # get subset of all new column names
  final_set <- subset(all, select = c("Sample_ID", "SentrixBarcode_A", "SentrixPosition_A", "Sample_Plate", "Sample_Well", 
                         "Gender", "Sample_Name", "Replicate", "Parent1", "Parent2", "Species", 
                         "Tissue Source", "Concentration", "Extraction Method", "Wga Method", "Comment",
                         "Instrument_ID", "Race", "MRN", "Name", "DOB", "exclude"))
  
  plates <- unique(final_set$Sample_Plate)
  for (j in seq(1, length(plates))){
    temp_subset <- final_set[which(final_set$Sample_Plate == plates[j]),]
    # sort by plate and position
    final_set_sorted <- temp_subset[order(temp_subset$Sample_Plate, temp_subset$Sample_Well),]
    temp<- final_set_sorted
    allRows <- c('[Header]',
                 'Institute Name', 
                 'Investigator Name', 
                 'Project Name', 
                 'Date', 
                 '', 
                 '[Manifests]',
                 'A',
                 '',
                 '[Data]',
                 '')
    
    allRows_values <-  c('', 
                         'CCPM Biobank',
                         'Kathleen Barnes',
                         as.character(all$Project[1]),
                         as.character(Sys.Date()),
                         '',
                         '',
                         paste(all$Product,all$Part.Number,"A1", sep="_")[1],
                         '',
                         '',
                         ''
    )
    addClusterFilePath <- rep('', 11)
    addClusterFilePath[8] <- as.character(all$Cluster.File)[1]
    
    rowAdd<- rep('', ncol(x = final_set_sorted))
    for (i in seq(1, length(allRows))){
      if (i==1){
        temp<-rbind(names(final_set_sorted), temp)
      }else{
        rowAdd[1]<-rev(allRows)[i]
        rowAdd[2]<-rev(allRows_values)[i]
        rowAdd[3]<-rev(addClusterFilePath)[i]
        #print(rowAdd)
        temp <- rbind(rowAdd, temp)
      }
    }
    if (!file.exists(paste(gsub(as.character(all$Project[1]), pattern = " ", replacement = "_"), "_BCPmanifest_", j, ".csv", sep=""))){
      write.table(file=paste(gsub(as.character(all$Project[1]), pattern = " ", replacement = "_"), "_BCPmanifest_", j, ".csv", sep=""), x=temp, quote = FALSE, row.names = FALSE, col.names = FALSE, sep=',', na='')
      print("Successfully created manifest!")
      print(paste("Manifest Name: ", gsub(as.character(all$Project[1]), pattern = " ", replacement = "_"), "_BCPmanifest_", j, ".csv", sep=""))
    }else{
      print("ERROR! File already exists!!  Please existing file or move file to different location.")
      print("Manifest has not been created")
    }
    
  }

}



################################################################################
##function: get_redcap_input(manifestPath, detailedPath)                      ##
##input: passed variables from func get_detailed_input()                      ##
##output:                                                                     ##
################################################################################

get_redcap_input <- function(manifestPath, detailedPath){
  cat("Path and name of REDCap report (.csv): ")
  redcapName <- trimws(readLines("stdin", n=1), which="both")
  cat("You entered the following: ", redcapName)
  cat("\n")
  cat("Is this correct? (Y/N/Q) ")
  verification_redcap <- trimws(readLines("stdin", n=1), which="both")
  if ((toupper(verification_redcap) == "YES") | (toupper(verification_redcap) == "Y")){
    return(generate_manifest(manifestPath = manifestPath, detailedPath = detailedPath, redcapPath = redcapName))
  }else if ((toupper(verification_redcap) == "NO") | (toupper(verification_redcap) == "N")){
    return(get_redcap_input(manifestPath = manifestPath, detailedPath = detailedPath))
  }else{
    stop("Exiting program, no changes made. Goodbye!")
  }
}



################################################################################
##function: get_detailed_input(manifestPath)                                  ##
##input: passed variable from func get_manifest_input()                       ##
##output:                                                                     ##
################################################################################

get_detailed_input <- function(manifestPath){
  cat("Path and name of detailed report (.csv): ") ## TO DO: Check if lab wants to call this something else
  detailedName <- trimws(readLines("stdin", n=1), which="both")
  cat("You entered the following: ", detailedName)
  cat("\n")
  cat("Is this correct? (Y/N/Q) ")
  verification_detail <- trimws(readLines("stdin", n=1), which="both")
  if ((toupper(verification_detail) == "YES") | (toupper(verification_detail) == "Y")){
    return(get_redcap_input(manifestPath = manifestPath, detailedPath = detailedName))
  }else if ((toupper(verification_detail) == "NO") | (toupper(verification_detail) == "N")){
    return(get_detailed_input(manifestPath = manifestPath))
  }else{
    stop("Exiting program, no changes made. Goodbye!")
  }
}



################################################################################
##function: get_manifest_input()                                              ##
##input: None -- prompt user for path and name of lab manifest file           ##
##output:                                                                     ##
################################################################################

get_manifest_input <- function(){
  cat("Path and name of sample manifest (.csv): ") ## TO DO:  Check if lab wants to call this something else
  manifestName <- trimws(readLines("stdin", n=1), which="both")
  cat("You entered the following: ", manifestName)
  cat('\n')
  cat("Is this correct? (Y/N/Q) ")
  verification_manifest <- trimws(readLines("stdin", n=1), which="both")
  if ((toupper(verification_manifest) == "YES") | (toupper(verification_manifest) == "Y")){
    return(get_detailed_input(manifestPath = manifestName))
  }else if ((toupper(verification_manifest) == "NO") | (toupper(verification_manifest) == "N")){
    return(get_manifest_input())
  }else{
    stop("Exiting program, no changes made. Goodbye!")
  }
}


#####-----------------------------Call and Running the Script----------------------------#####

# calls to funtions
get_manifest_input()

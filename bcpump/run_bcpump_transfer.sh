#!/bin/bash

# ALL VARIABLES ENDING IN DIR NEED TO END IN "/"

LOG_DIR='/path/to/log/dir/'
MANIFEST_DIR='/path/to/manifest/dir/'
BCPUMP_DIR='/path/to/bcpump/'
BC_LOG_FILE='/path/to/logFile.txt'
CALL_DIR='/path/to/call/gtcs/dir/'
BEELINE_DIR='/path/to/beeline/gtcs/dir/'
TIMESTAMP=$(date '+%Y-%m-%d_%H:%M:%S')
GTC_HEADER_1='SentrixBarcode_A'
GTC_HEADER_2='SentrixPosition_A'
LOG_OUTPUT=${LOG_DIR}logAudit${TIMESTAMP}.txt
IFS=,
regName='.*\s|!|\@|#|\$|%|\^|\&|\*|\(|\)|\+|\"|\?|,.*'


START=$(date '+%Y-%m-%d_%H:%M:%S')
echo "Process started on:  $START" | tee -a "$LOG_OUTPUT"

sed 's/^[0-9]*-[0-9]*-[0-9]*_[0-9]*:[0-9]*:[0-9]*\s//g' $BC_LOG_FILE | grep ".csv"  > ${LOG_DIR}prevRun_"${TIMESTAMP}".txt

cd $MANIFEST_DIR
ls -1 ./*.csv | sed 's/^\.\///g' > ${LOG_DIR}availManifest_"${TIMESTAMP}".txt
cd $LOG_DIR

comm -23 <(sort ${LOG_DIR}availManifest_"${TIMESTAMP}".txt) <(sort ${LOG_DIR}prevRun_"${TIMESTAMP}".txt) > ${LOG_DIR}needToRun_"$TIMESTAMP".txt
sed -i 's/^\.\///g' ${LOG_DIR}needToRun_"$TIMESTAMP".txt

totalNewFiles=$(wc -l ${LOG_DIR}needToRun_"$TIMESTAMP".txt | cut -d' ' -f1)

if [ "$totalNewFiles" != 0 ]; then 
	while read -r filesToBeRun; do
		echo | tee -a "$LOG_OUTPUT"
		echo "Currently analyzing the following manifest: ${filesToBeRun}" | tee -a "$LOG_OUTPUT"

		if [[ "$filesToBeRun" =~ $regName ]]; then
			echo "File name check: FAIL" | tee -a "$LOG_OUTPUT"
			echo "ERROR: file name contains invalid character, please correct.  No changes have been made" | tee -a "$LOG_OUTPUT"
			FINISH=$(date '+%Y-%m-%d_%H:%M:%S')
			echo "Process finished on:  $FINISH" | tee -a "$LOG_DIR"  
			exit 42
		else
			echo "File name check: PASS" | tee -a "$LOG_OUTPUT"
		fi

		full_path=${MANIFEST_DIR}$filesToBeRun
		GTC_1_LOC=0
		GTC_2_LOC=0
		GTC_FILE_NAME_TEMP="list_of_gtcs.txt"
		headerStart=$(grep -in "^sample_id" "$full_path" | awk -F ":" '{print $1}')
		tail -n +"${headerStart}" "$full_path" > temp.txt
		for j in {1..20}; do #number of headers in sample sheet
			if [ "$GTC_1_LOC" != 0 ] && [ "$GTC_2_LOC" != 0 ]; then
				echo "Header check: PASS" | tee -a "$LOG_OUTPUT"
				break
			else 
			
				if [ "$(cut -d',' -f${j} temp.txt | grep -i "$GTC_HEADER_1" | wc -l | cut -d' ' -f1)" == 1 ]; then
					GTC_1_LOC=$j 	
				elif [ "$(cut -d',' -f${j} temp.txt | grep -i "$GTC_HEADER_1" | wc -l | cut -d' ' -f1)" -gt 1 ]; then
					echo "Header check: FAIL" | tee -a "$LOG_OUTPUT"
					echo $j | tee -a "$LOG_OUTPUT"
					"cut -d',' -f${j} temp.txt | grep -i $GTC_HEADER_1 | wc -l" | tee -a "$LOG_OUTPUT"
					echo "ERROR!  ${GTC_HEADER_1} appears as a header multiple times in ${filesToBeRun}, please correct.  No changes have been made to the system for this manifest." | tee -a "$LOG_OUTPUT"
					FINISH=$(date '+%Y-%m-%d_%H:%M:%S')
					echo "Process finished on:  $FINISH" | tee -a "$LOG_DIR"
					exit 42

				fi


				if [ "$(cut -d',' -f${j} temp.txt | grep -i "$GTC_HEADER_2" | wc -l | cut -d' ' -f1)" == 1 ]; then
					GTC_2_LOC=$j
				elif [ "$(cut -d',' -f${j} temp.txt | grep -i "$GTC_HEADER_2" | wc -l | cut -d' ' -f1)" -gt 1 ]; then
					echo "Header check: FAIL" | tee -a "$LOG_OUTPUT"
					echo "ERROR!  ${GTC_HEADER_2} appears as a header multiple times in ${filesToBeRun}, please correct.  No changes have been made to the system for this manifest." | tee -a "$LOG_OUTPUT"
					FINISH=$(date '+%Y-%m-%d_%H:%M:%S')
					echo "Process finished on:  $FINISH" | tee -a "$LOG_DIR"
					exit 42	
				elif ([ $j == 20 ] && [ $GTC_1_LOC == 0 ]) || ([ $j == 20 ] && [ $GTC_2_LOC == 0 ]); then
					echo "Header check: FAIL" | tee -a "$LOG_OUTPUT"
					echo "ERROR!  Missing $GTC_1_LOC and/or $GTC_2_LOC header in ${filesToBeRun} manifest, please correct.  No changes have been made to the system for this manifest." | tee -a "$LOG_OUTPUT"
					FINISH=$(date '+%Y-%m-%d_%H:%M:%S')
					echo "Process finished on:  $FINISH" | tee -a "$LOG_DIR"
					exit 42
				fi
			fi
		done # end of for loop for header identification

		# get name of all gtc files
		cut -d',' -f${GTC_1_LOC},${GTC_2_LOC} temp.txt | tail -n +2 | awk -F"," '{print $1"_"$2".gtc"}' > $GTC_FILE_NAME_TEMP
		
		# check gtcs are unique in manifest
		if [ "$(sort $GTC_FILE_NAME_TEMP | uniq -D | wc -l | cut -d' ' -f1 )" != 0 ]; then
			echo "All gtcs unique in manifest: FAIL" | tee -a "$LOG_OUTPUT"
			echo "ERROR: Some GTC files appear multiple times! The following GTCs are duplicated ${filesToBeRun}:" | tee -a "$LOG_OUTPUT"
			sort $GTC_FILE_NAME_TEMP | uniq -d | tee -a "$LOG_OUTPUT"
			echo "Please correct.  No changes have been made to the system for this manifest." | tee -a "$LOG_OUTPUT"
			FINISH=$(date '+%Y-%m-%d_%H:%M:%S')
			echo "Process finished on:  $FINISH" | tee -a "$LOG_DIR"
			exit 42
		else 
			echo "All gtcs unique in manifest: PASS" | tee -a "$LOG_OUTPUT"
		fi
		
		# check gtcs are unique in bc log
		if [ "$(comm -23 <(sort -f $GTC_FILE_NAME_TEMP) <(sort -f $BC_LOG_FILE) | wc -l | cut -d' ' -f1)" != "$(wc -l $GTC_FILE_NAME_TEMP | cut -d' ' -f1)" ]; then
			echo "All gtcs unique to BC Pump: FAIL" | tee -a "$LOG_OUTPUT"
			echo "ERROR: Some GTC files appear multiple times in BC Log File! The following GTCs are duplicated in BC Pump for the following manifest ${filesToBeRun}:" | tee -a "$LOG_OUTPUT"
			comm -12 <(sort -f $GTC_FILE_NAME_TEMP) <(sort -f $BC_LOG_FILE) | tee -a "$LOG_OUTPUT" # print intersection 
			echo "Please correct.  No changes have been made to the system for this manifest." | tee -a "$LOG_OUTPUT"
			FINISH=$(date '+%Y-%m-%d_%H:%M:%S')
			echo "Process finished on:  $FINISH" | tee -a "$LOG_DIR"
			exit 42
		else
			echo "All gtcs unique to BC Pump: PASS" | tee -a "$LOG_OUTPUT"

		fi

		# find all gtcs in call and beeline directories

		find ${BEELINE_DIR} | grep -f "list_of_gtcs.txt" > full_path_gtc_locs.txt
		find ${CALL_DIR} | grep -f "list_of_gtcs.txt" >> full_path_gtc_locs.txt
		
		sed 's/.*\///g' full_path_gtc_locs.txt > check_names.txt

		if [ "$(sort check_names.txt | uniq | wc -l |cut -d' ' -f1)" == "$(wc -l full_path_gtc_locs.txt | cut -d' ' -f1)" ]; then
			echo "Duplicate gtcs between Beeline and call directories: PASS" | tee -a "$LOG_OUTPUT"
			# CHECK MULTIPLE OF 96
			echo "Checking muliple of 96" | tee -a "$LOG_OUTPUT"
			if [ $(($(wc -l full_path_gtc_locs.txt | cut -d' ' -f1) % 96)) == 0 ]; then
				echo "Multiple of 96: PASS" | tee -a "$LOG_OUTPUT"
				echo "  Copying GTC files to $BCPUMP_DIR ..." | tee -a "$LOG_OUTPUT"
				xargs -a full_path_gtc_locs.txt cp -t $BCPUMP_DIR
				echo "  Copying sample manifest: $full_path to $BCPUMP_DIR" | tee -a "$LOG_OUTPUT"
				cp "$full_path" $BCPUMP_DIR
				echo "Finished!" | tee -a "$LOG_OUTPUT"
			else
				echo "Multiple of 96: FAIL" | tee -a "$LOG_OUTPUT"
				echo "Total gtcs found are $(wc -l full_path_gtc_locs.txt | cut -d' ' -f1).  Need multiple of 96, please correct.  No changes have been made to the system for this manifest." |tee -a "$LOG_OUTPUT"
				FINISH=$(date '+%Y-%m-%d_%H:%M:%S')
				echo "Process finished on:  $FINISH" | tee -a "$LOG_DIR"
				exit 42
			fi
		else
			echo "Duplicate gtc between Beeline and call directories: YES...IN PROCESS OF RESOLVING" | tee -a "$LOG_OUTPUT"
			sort check_names.txt | uniq -d | awk -v env_var="${BEELINE_DIR}" '{print env_var$1}' > resolve_duplicate_gtcs.txt
			echo "     The following gtc files appear in both call folder and Beeline folder:" | tee -a "$LOG_OUTPUT"
			sed 's/.*\///g' resolve_duplicate_gtcs.txt | awk '{print "\t"$1}' | tee -a "$LOG_OUTPUT"
			echo "     DUPLICATE RESOLUTION: Keeping call folder gtc duplicates and ignoring Beeline duplicates..." | tee -a "$LOG_OUTPUT"
			comm -23 <(sort full_path_gtc_locs.txt) <(sort resolve_duplicate_gtcs.txt) > "gtcsToBeCopied.txt"
			
			if [ $(($(wc -l gtcsToBeCopied.txt | cut -d' ' -f1) % 96)) == 0 ]; then
				echo "Multiple of 96: PASS" | tee -a "$LOG_OUTPUT"
				echo "	Copying GTC files to $BCPUMP_DIR ..." | tee -a "$LOG_OUTPUT"
				xargs -a gtcsToBeCopied.txt cp -t $BCPUMP_DIR
				echo "	Copying sample manifest: $full_path to $BCPUMP_DIR" | tee -a "$LOG_OUTPUT"
				cp "$full_path" $BCPUMP_DIR
				echo "Finished!" | tee -a "$LOG_OUTPUT"
				FINISH=$(date '+%Y-%m-%d_%H:%M:%S')
				echo "Process finished on:  $FINISH" | tee -a "$LOG_OUTPUT"

			else
				echo "Multiple of 96: FAIL" | tee -a "$LOG_OUTPUT"
				echo "Total gtcs found are $(wc -l gtcsToBeCopied.txt | cut -d' ' -f1).  Need multiple of 96, please correct.  No changes have been made to the system for this manifest." |tee -a "$LOG_OUTPUT"
				FINISH=$(date '+%Y-%m-%d_%H:%M:%S')
				echo "Process finished on:  $FINISH" | tee -a "$LOG_DIR"
				exit 42
			fi
		fi
		
	done<${LOG_DIR}"needToRun_$TIMESTAMP.txt"
else
	echo "Everything is up to date. No new manifests available, no changes have been made to the system." | tee -a "$LOG_OUTPUT"
	exit 42
fi


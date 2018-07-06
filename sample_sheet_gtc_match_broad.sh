#!/bin/bash

output_loc="/gpfs/share/Biobank/"
gtc_call_dir="/gpfs/share/Biobank/transfer/Illumina_LIMS/call"

for file in /gpfs/share/Biobank/transfer/Illumina_LIMS/sample\ sheets\ from\ GenomeStudio/*
do
	trunc_file_name=$(echo "$file" | sed "s/\ /_/g" | sed "s/.csv//g" | sed "s/\/gpfs\/share\/Biobank\/transfer\/Illumina_LIMS\/sample_sheets_from_GenomeStudio\///g")
	header=$(grep -in "path" "$file" | cut -f1 -d:;)
	tail -n +$header "$file" | awk -F"," '{print $2"_"$3".gtc"}' | sed 's/\\/\//g' > $output_loc$trunc_file_name"_list_of_gtc.txt"
	cd $output_loc
	if [ ! -d $trunc_file_name"_with_gtc_files" ]; then	
		mkdir $trunc_file_name"_with_gtc_files"
		find $gtc_call_dir | grep -f $output_loc$trunc_file_name"_list_of_gtc.txt" > $output_loc$trunc_file_name"_list_of_gtc_full_path.txt"
		rm $output_loc$trunc_file_name"_list_of_gtc.txt"
		cat $output_loc$trunc_file_name"_list_of_gtc_full_path.txt" | xargs -I % cp % $output_loc$trunc_file_name'_with_gtc_files'
		cp "$file" $output_loc$trunc_file_name"_with_gtc_files"	
		mv $output_loc$trunc_file_name"_list_of_gtc_full_path.txt" $output_loc$trunc_file_name"_with_gtc_files"
	fi
	cd $output_loc
	if [ $(ls *_list_of_gtc.txt | wc -l) > 0 ]; then
	rm *_list_of_gtc.txt
	fi
done

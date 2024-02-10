#!/bin/bash
if [[ $1 = "backup" ]]
then
	pwd
	ls *.txt
	mkdir backup
	cd backup/
	echo Moved to backup directory
	pwd
	cp -r $(cd -)/*.txt $(pwd)
	echo Copied all text files to backup directory
	echo 'Current backup:' > date.txt
	date >> date.txt
	cat date.txt
elif [[ $1 = "archive" ]]
then
	if [[ -x "$2" ]]
	then echo "Error: Archive task requires file format\nUsage: ./setup.sh archive <fileformat>"
		#maybe shouldnt have this exit here, but it feels right
		exit 1
	else
		tar -czvf archive-$(date +%F).tgz *.$2
		echo Created archive archive-$(date +%F).tgz
		ls -l
	fi
elif [[ $1 = "sortedcopy" ]]
then 
	#check 2 additional args given
	if [[ $# != 3 ]]
	then echo -e "Error: Expected two additional input parameters. \nUsage: ./setup.sh sortedcopy <sourcedirectory> <targetdirectory>"
		exit 1
	#make sure $2(first additional parameter) is a directory
	elif ! [[ -d "$2" ]]
	then echo -e "Error: Input parameter #2 '$2' is not a directory.\nUsage: ./setup.sh sortedcopy <sourcedirectory> <targetdirectory>"
	exit 2
	else
	#create target dir and copy the files to it		
	if [[ -d $3 ]]
	then echo -e "Directory 'sorted_dir' already exists. Overwrite? (y/n)"
		#prompt user input, if 'y' then delete -r the existing directory
		read yno
		case $yno in
			[y] ) rm -r $3 ;;
			*) exit 3 ;;
		esac
	fi
	mkdir $3
	x=1
	for file in $(ls -p -r | grep -v /) #need to not do this to directories!!!
	do
		#if [$file -d]; then 
			#continue 
	#	fi
		cp "$file" "$3/$x.$file"
		x=$((x+1))
	done
	exit 0
	fi
else 
	echo -e "Error: Task must be specified. Supported tasks: backup, archive, sortedcopy.\nUsage: ./setup.sh <task> <additional_arguments>"
	exit 1
fi

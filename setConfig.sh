#!/usr/bin/env bash

eshBin=$(which esh)
if [ $? -ne 0 ]; then
	exit 1
fi

dirname=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
finalDir="/etc/nixos"
baseDir="$dirname/nixos"
baseLength=$(( ${#baseDir} + 1 ))

function copyFiles() {
	for file in "$1"/*; do
		relfile=${file:$baseLength}
		if [[ -d $file ]]; then
			sudo mkdir -p "$finalDir/$relfile"
			copyFiles $file
		elif [[ $file == *.esh ]]; then
			cp <($eshBin $file) $dirname/eshout
			sudo cp $dirname/eshout "$finalDir/${relfile%.*}"
		else
			sudo cp $file "$finalDir/$relfile"
		fi
	done
}

copyFiles $baseDir


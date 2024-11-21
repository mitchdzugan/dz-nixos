#!/usr/bin/env bash

eshBin=$(which esh)
if [ $? -ne 0 ]; then
	exit 1
fi

selfDir=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
finalDir="/etc/nixos"
configDir="config"
baseDir="$selfDir/$configDir"
pids=()

function copyFile() {
	relFile=$1
	file="$baseDir/$relFile"
	sudo mkdir -p "$finalDir/$(dirname $relFile)"
	if [[ $file == *.esh ]]; then
		$eshBin $file | (sudo cat > "$finalDir/${relFile%.*}")
	else
		sudo cp $file "$finalDir/$relFile"
	fi
}

i=0
while read -r rel; do
	copyFile $rel &
	pids[${i}]=$!
	i=$(( i + 1 ))
done < <(cd $baseDir && git ls-files)

for pid in ${pids[*]}; do
	wait $pid
done

#! /bin/bash

declare -r evaluations=100000
declare -r pSize=512
declare -r iDepth=6

declare -r statDir=$1

if [ "$#" -ne 1 ]; then
    echo "USAGE ./script <targetDirectory>"
    exit 1
fi

declare -r paramFile="ec/iphashing/problems/cuckoo/cuckoo.params"

declare -r runner="ec/iphashing/experiments/run.sh"

`$runner $evaluations $pSize $iDepth $paramFile $statDir`

#! /bin/bash

declare -r evaluations=200000
declare -r pSize=512
declare -r iDepth=6

declare -r statDir="mdRS/set1"
#declare -r statDir="mdRS/set2"
#declare -r statDir="mdRS/set3"
#declare -r statDir="mdRS/set4"

declare -r paramFile="ec/iphashing/problems/merkledamgard/merkledamgardRS.params"

declare -r runner="ec/iphashing/experiments/run.sh"

`$runner $evaluations $pSize $iDepth $paramFile $statDir`

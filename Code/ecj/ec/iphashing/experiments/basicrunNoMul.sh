#! /bin/bash

# this is a basic experiment with by octet ip hashing

declare -r evaluations=100000
declare -r pSize=512
declare -r iDepth=6

#declare -r statDir="basicrunNoMul/set1"
#declare -r statDir="basicrunNoMul/set2"
#declare -r statDir="basicrunNoMul/set3"
declare -r statDir="basicrunNoMul/set4"
declare -r paramFile="ec/iphashing/problems/byoctet/basicRunNoMul.params"

declare -r runner="ec/iphashing/experiments/run.sh"

`$runner $evaluations $pSize $iDepth $paramFile $statDir`

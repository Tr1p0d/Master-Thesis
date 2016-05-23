#! /bin/bash

# this is a basic experiment with by octet ip hashing

declare -r evaluations=100000
declare -r pSize=512
declare -r iDepth=6

#declare -r statDir="basicrunNoERC/set1"
#declare -r statDir="basicrunNoERC/set2"
#declare -r statDir="basicrunNoERC/set3"
declare -r statDir="basicrunNoERC/set4"
declare -r paramFile="ec/iphashing/problems/byoctet/basicRunNoERC.params"

declare -r runner="ec/iphashing/experiments/run.sh"

`$runner $evaluations $pSize $iDepth $paramFile $statDir`

#! /bin/bash

declare -r evaluations=100000
declare -r pSize=512
declare -r iDepth=6

#declare -r statDir="cuckooRS/set1"
#declare -r statDir="cuckooRS/set2"
#declare -r statDir="cuckooRS/set3"
declare -r statDir="cuckooRS/set4"

declare -r paramFile="ec/iphashing/problems/cuckoo/cuckooRS.params"

declare -r runner="ec/iphashing/experiments/run.sh"

`$runner $evaluations $pSize $iDepth $paramFile $statDir`

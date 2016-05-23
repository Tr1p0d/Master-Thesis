#! /bin/bash

# this is a basic experiment with by octet ip hashing

declare -r evaluations=100000
declare -r pSize=512
declare -r iDepth=6

declare -r statDir="basicRSnoElitism"
declare -r paramFile="ec/iphashing/problems/byoctet/basicRandomSearchNoElitism.params"

declare -r runner="ec/iphashing/experiments/run.sh"

`$runner $evaluations $pSize $iDepth $paramFile $statDir`

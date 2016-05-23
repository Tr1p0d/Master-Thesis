#! /bin/bash

# lets see the impact of population size on our results

declare -r experiments=50
declare -r evaluations=100000

declare -r inParallel=12
declare -r statDir="brPopulationDepth/set2"
declare -r statDir1="ec/iphashing/experiments/brPopulationDepth/set2"
declare -r paramFile="ec/iphashing/problems/byoctet/basicRun.params"

declare -r runner="ec/iphashing/experiments/run.sh"

for pSize in {8,16,32,64,128,256,512,1024}; do
    rm -r "$statDir1/$pSize" 2> /dev/null
    mkdir "$statDir1/$pSize"

    for iDepth in {3,4,5,6}; do
        rm -r "$statDir1/$pSize/$iDepth" 2> /dev/null
        mkdir "$statDir1/$pSize/$iDepth"

        `$runner $evaluations $pSize $iDepth $paramFile $statDir/$pSize/$iDepth`
    done
done

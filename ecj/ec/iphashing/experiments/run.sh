#! /bin/bash

# this is a basic experiment with by octet ip hashing

declare -r inParallel=12
declare -r experiments=50

declare -r evaluations=$1
declare -r population=$2
declare -r targetDir=$5
declare -r initialDepth=$3

declare -r statDir="../../experiments/$targetDir"
declare -r paramFile=$4

if [ "$#" -ne 5 ]; then
    echo -n "USAGE ./script <number of evaluations> <population size>"
    echo    " <initial maximum depth> <param file> <target directory>"
    exit 1
fi

i=0

function runExperiment {
    java ec.Evolve\
    -p silent=true\
    -p evaluations=$evaluations\
    -p pop.subpop.0.size=$population\
    -p 'gp.tc.0.init.max-depth'=$initialDepth\
    -p 'pop.subpop.0.species.pipe.maxsize'=6\
    -p 'pop.subpop.0.species.pipe.maxdepth'=6\
    -p stat.file="$statDir/$1" -file $paramFile
    echo "Saving experiment to file $statDir/$1"
}

while true ; do
    for j in $(seq 1 `expr $inParallel - 1`); do
        if [ $i -ge $experiments ]; then
            echo "Done"
	    exit 0
        fi

        runExperiment "run.$i.out" &
        ((i++))
    done
    runExperiment "run.$i.out"
    ((i++))
done

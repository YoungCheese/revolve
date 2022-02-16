#!/bin/bash

runs=20
final_gen=199
experiments=("costplasticoding")


for i in $(seq $runs)
do
    run=$(($i))

    for experiment in "${experiments[@]}"
    do

     echo ""
     file="${experiment}_${run}.log";

     #check if experiments status
     if [[ -f "$file" ]]; then

        value=$(grep "Exported snapshot" $file|tail -n1|sed -E "s/\[(.*),.*Exported snapshot ([0-9]+).*/\2/g");
        echo $file;
        echo $value;
      else
         echo "$file"
         echo "None";
      fi

    done
done

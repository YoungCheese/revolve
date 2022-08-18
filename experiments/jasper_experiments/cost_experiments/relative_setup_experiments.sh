#!/usr/bin/env python3
import asyncio
from pyrevolve import parser
from pyrevolve.evolution import fitness
from pyrevolve.evolution.selection import multiple_selection, tournament_selection
from pyrevolve.evolution.population import Population, PopulationConfig
from pyrevolve.evolution.pop_management.steady_state import steady_state_population_management
from pyrevolve.experiment_management import ExperimentManagement
from pyrevolve.genotype.plasticoding.crossover.crossover import CrossoverConfig
from pyrevolve.genotype.plasticoding.crossover.standard_crossover import standard_crossover
from pyrevolve.genotype.plasticoding.initialization import random_initialization
from pyrevolve.genotype.plasticoding.mutation.mutation import MutationConfig
from pyrevolve.genotype.plasticoding.mutation.standard_mutation import standard_mutation
from pyrevolve.genotype.plasticoding.plasticoding import PlasticodingConfig
from pyrevolve.tol.manage import measures
from pyrevolve.util.supervisor.simulator_queue import SimulatorQueue
from pyrevolve.util.supervisor.analyzer_queue import AnalyzerQueue
from pyrevolve.custom_logging.logger import logger
import sys
#

runs=10
num_terminals=6
start_port=8000
final_gen=199
experiments=("relativeunweighted")
managers_sulfix=("" "")
experiments_path=/storage/jkoning/
managers_path=experiments/jasper_experiments/cost_experiments/

while true
	do

    echo "killing all processes..."
    kill $(  ps aux | grep 'gzserver' | awk '{print $2}');
    kill $(  ps aux | grep 'revolve.py' | awk '{print $2}');

    echo "restarting all processes..."
		# sleep 60s;
    to_do=()

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
            if [ "$value" != "$final_gen" ]; then
              to_do+=("${experiment}_${run}")
             fi
          else
             echo "$file"
             echo "None";
             to_do+=("${experiment}_${run}")
          fi


        done
    done


    # selects num_terminals unfinished experiments to spawn
    to_do=(${to_do[@]:0:$num_terminals})
    for experiment in "${to_do[@]}"
    do
         echo ""
         # echo -d -m -S "${experiment}" -L -Logfile "${experiment}.log" -n19 ./revolve.sh --manager "${managers_path}$(cut -d'_' -f1 <<<"$experiment")${managers_sulfix}.py" --experiment-name "${experiments_path}${experiment}" --evaluation-time 50 --n-cores 4 --port-start $start_port

         screen -d -m -S "${experiment}" -L -Logfile "${experiment}.log" nice -n19 ./revolve.sh --manager "${managers_path}$(cut -d'_' -f1 <<<"$experiment")${managers_sulfix}.py" --experiment-name "${experiments_path}${experiment}" --n-cores 4 --port-start $start_port
         start_port=$((${start_port}+10))
    done

    sleep 720s;

done


# killall screen
# screen -r naaameee = name of experiment
# screen -list

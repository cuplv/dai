# In order to add, remove, or change random seeds, adjust (1) the values for $seed in both for loops, (2) the names of log files supplied to the `paste` commands, (3) the constant passed to the scatter_config.py script (currently 4, set to the total number of seeds), and (4) the constant passed to the cdf.py script (currently 4000, set to seeds * n)
# In order to change the number of edits per experiment, adjust (1) the value of n on line 9, (2) the names of log files throughout, replacing "n500" by "nX" for whatever X you choose, and (3) the constant passed to the cdf.py script (currently 2000, set to seeds * n)

# NB: if you want to re-generate plots without re-running experiments for any reason, just comment out the first for-loop here.
##########################
## Run some experiments ##
##########################
echo "Running some miniature experiments... this should just take 20 seconds or so."
for seed in 4 5 6 7; do
    for n in 500; do
	for qpe in 5; do
	    echo "dd+incr, $qpe qpe, $n iterations with seed $seed"
	    ./run_d1a_experiment -d -i -s $seed -q $qpe $n
	    echo "dd-only, $qpe qpe, $n iterations with seed $seed" 
	    ./run_d1a_experiment -d -s $seed -q $qpe $n
	done
	echo "incr-only, $n iterations with seed $seed" 
	./run_d1a_experiment -i -s $seed $n
	echo "batch, $n iterations with seed $seed" 
	./run_d1a_experiment -s $seed $n
    done
done

####################
## Generate plots ##
####################
echo "\nGenerating miniature versions of Fig. 10 plots..."
mkdir -p tmp
rm tmp/*
for seed in 4 5 6 7; do
    # normalize to same numbers of rows per config
    scripts/to_moving_average_per_edit.py 5 < out/experiments/dd_5qpe_incr_n500_seed$seed.log > tmp/dd_5qpe_incr_n500_seed$seed.log.avgs
    scripts/to_moving_average_per_edit.py 5 < out/experiments/dd_5qpe_n500_seed$seed.log > tmp/dd_5qpe_n500_seed$seed.log.avgs
    # combine all logs for each config (in series)
    cat tmp/dd_5qpe_incr_n500_seed$seed.log.avgs >> tmp/dd_incr_all.log
    cat tmp/dd_5qpe_n500_seed$seed.log.avgs >> tmp/dd_all.log
    cat out/experiments/incr_n500_seed$seed.log >> tmp/incr_all.log
    cat out/experiments/n500_seed$seed.log >> tmp/batch_all.log
done

# combine all logs for each config (in parallel)
paste -d "," tmp/dd_5qpe_incr_n500_seed4.log.avgs tmp/dd_5qpe_incr_n500_seed5.log.avgs tmp/dd_5qpe_incr_n500_seed6.log.avgs tmp/dd_5qpe_incr_n500_seed7.log.avgs > tmp/dd_incr_parallel.log
paste -d "," tmp/dd_5qpe_n500_seed4.log.avgs tmp/dd_5qpe_n500_seed5.log.avgs tmp/dd_5qpe_n500_seed6.log.avgs tmp/dd_5qpe_n500_seed7.log.avgs > tmp/dd_parallel.log
paste -d "," out/experiments/incr_n500_seed4.log out/experiments/incr_n500_seed5.log out/experiments/incr_n500_seed6.log out/experiments/incr_n500_seed7.log > tmp/incr_parallel.log
paste -d "," out/experiments/n500_seed4.log out/experiments/n500_seed5.log out/experiments/n500_seed6.log out/experiments/n500_seed7.log > tmp/batch_parallel.log

# generate scatter plots
echo " ... generating scatter plot: Demand-Driven \& Incremental"
scripts/scatter_config_small.py out/plots/dd_incr_scatter_small.png dd_incr 4 < tmp/dd_incr_parallel.log
echo " ... generating scatter plot: Demand-Driven"
scripts/scatter_config_small.py out/plots/dd_scatter_small.png dd 4 < tmp/dd_parallel.log
echo " ... generating scatter plot: Incremental"
scripts/scatter_config_small.py out/plots/incr_scatter_small.png incr 4 < tmp/incr_parallel.log
echo " ... generating scatter plot: Batch"
scripts/scatter_config_small.py out/plots/batch_scatter_small.png batch 4 < tmp/batch_parallel.log

# generate CDF at out/plots/cdf.png
echo " ... generating CDF"
scripts/cdf.py out/plots/cdf_small.png 2000 tmp/batch_all.log tmp/incr_all.log tmp/dd_all.log tmp/dd_incr_all.log

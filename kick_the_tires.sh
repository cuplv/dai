################################
## Run some small experiments ##
################################
echo "Running some miniature experiments... this should just take 30 seconds or so."
for seed in 0 1 2 3; do
    for n in 100; do
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
echo "Done with experiments"
echo ""
echo "Generating miniature versions of Fig. 10 plots..."
mkdir -p tmp
touch tmp/tmp # silence "no files match tmp/*" warning in next line 
rm tmp/* # clean up from previous runs
for seed in 0 1 2 3; do
    # normalize to same numbers of rows per config
    scripts/to_moving_average_per_edit.py 5 < out/experiments/dd_5qpe_incr_n100_seed$seed.log > tmp/dd_5qpe_incr_n100_seed$seed.log.avgs
    scripts/to_moving_average_per_edit.py 5 < out/experiments/dd_5qpe_n100_seed$seed.log > tmp/dd_5qpe_n100_seed$seed.log.avgs
    # combine all logs for each config (in series)
    cat tmp/dd_5qpe_incr_n100_seed$seed.log.avgs >> tmp/dd_incr_all.log
    cat tmp/dd_5qpe_n100_seed$seed.log.avgs >> tmp/dd_all.log
    cat out/experiments/incr_n100_seed$seed.log >> tmp/incr_all.log
    cat out/experiments/n100_seed$seed.log >> tmp/batch_all.log
done

# combine all logs for each config (in parallel)
paste -d "," tmp/dd_5qpe_incr_n100_seed0.log.avgs tmp/dd_5qpe_incr_n100_seed1.log.avgs tmp/dd_5qpe_incr_n100_seed2.log.avgs tmp/dd_5qpe_incr_n100_seed3.log.avgs > tmp/dd_incr_parallel.log
paste -d "," tmp/dd_5qpe_n100_seed0.log.avgs tmp/dd_5qpe_n100_seed1.log.avgs tmp/dd_5qpe_n100_seed2.log.avgs tmp/dd_5qpe_n100_seed3.log.avgs > tmp/dd_parallel.log
paste -d "," out/experiments/incr_n100_seed0.log out/experiments/incr_n100_seed1.log out/experiments/incr_n100_seed2.log out/experiments/incr_n100_seed3.log > tmp/incr_parallel.log
paste -d "," out/experiments/n100_seed0.log out/experiments/n100_seed1.log out/experiments/n100_seed2.log out/experiments/n100_seed3.log > tmp/batch_parallel.log

# generate scatter plots
echo " ... generating scatter plot: Demand-Driven \& Incremental"
scripts/scatter_config_ktt.py out/plots/dd_incr_scatter.png dd_incr 4 < tmp/dd_incr_parallel.log
echo " ... generating scatter plot: Demand-Driven"
scripts/scatter_config_ktt.py out/plots/dd_scatter.png dd 4 < tmp/dd_parallel.log
echo " ... generating scatter plot: Incremental"
scripts/scatter_config_ktt.py out/plots/incr_scatter.png incr 4 < tmp/incr_parallel.log
echo " ... generating scatter plot: Batch"
scripts/scatter_config_ktt.py out/plots/batch_scatter.png batch 4 < tmp/batch_parallel.log

# generate CDF at out/plots/cdf.png
echo " ... generating CDF"
scripts/cdf_ktt.py out/plots/cdf.png 400 tmp/batch_all.log tmp/incr_all.log tmp/dd_all.log tmp/dd_incr_all.log

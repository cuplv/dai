echo "NOTE: This script just generates the figures shown in the paper and does _not_ actually run the experiments."
echo "      The raw data used for the figures can be found in ./data."
echo "      Comments in this script show the seeds/configurations in which the data were gathered, but note that"
echo "      the experiments were run in parallel and spread out across several servers in a cloud."
echo
echo


# EXPERIMENT CONFIGURATIONS:
# This script would (sequentially) run the experiments shown in the paper.
#for seed in 2 4 5 6 7 9 13 15 101; do
#    for n in 3000; do
#	echo "dd+incr, 5 qpe, $n iterations with seed $seed"
#	./run_d1a_experiment -d -i -s $seed -q 5 $n
#	echo "dd-only, 5 qpe, $n iterations with seed $seed" 
#	./run_d1a_experiment -d -s $seed -q 5 $n
#	echo "incr-only, $n iterations with seed $seed" 
#	./run_d1a_experiment -i -s $seed $n
#	echo "batch, $n iterations with seed $seed" 
#	./run_d1a_experiment -s $seed $n
#    done
#done

####################
## Generate plots ##
####################
echo "Generating Fig. 10 plots..."
mkdir -p tmp
rm tmp/*
for seed in 2 4 5 6 7 9 13 15 101; do
    # normalize to same numbers of rows per config
    scripts/to_moving_average_per_edit.py 5 < data/dd_5qpe_incr_n3000_seed$seed.log > tmp/dd_5qpe_incr_n3000_seed$seed.log.avgs
    scripts/to_moving_average_per_edit.py 5 < data/dd_5qpe_n3000_seed$seed.log > tmp/dd_5qpe_n3000_seed$seed.log.avgs
    # combine all logs for each config (in series)
    cat tmp/dd_5qpe_incr_n3000_seed$seed.log.avgs >> tmp/dd_incr_all.log
    cat tmp/dd_5qpe_n3000_seed$seed.log.avgs >> tmp/dd_all.log
    cat data/incr_n3000_seed$seed.log >> tmp/incr_all.log
    cat data/n3000_seed$seed.log >> tmp/batch_all.log
done

# combine all logs for each config (in parallel)
paste -d "," tmp/dd_5qpe_incr_n3000_seed2.log.avgs tmp/dd_5qpe_incr_n3000_seed4.log.avgs tmp/dd_5qpe_incr_n3000_seed5.log.avgs tmp/dd_5qpe_incr_n3000_seed6.log.avgs tmp/dd_5qpe_incr_n3000_seed7.log.avgs tmp/dd_5qpe_incr_n3000_seed9.log.avgs tmp/dd_5qpe_incr_n3000_seed13.log.avgs tmp/dd_5qpe_incr_n3000_seed15.log.avgs tmp/dd_5qpe_incr_n3000_seed101.log.avgs > tmp/dd_incr_parallel.log

paste -d "," tmp/dd_5qpe_n3000_seed2.log.avgs tmp/dd_5qpe_n3000_seed4.log.avgs tmp/dd_5qpe_n3000_seed5.log.avgs tmp/dd_5qpe_n3000_seed6.log.avgs tmp/dd_5qpe_n3000_seed7.log.avgs tmp/dd_5qpe_n3000_seed9.log.avgs tmp/dd_5qpe_n3000_seed13.log.avgs tmp/dd_5qpe_n3000_seed15.log.avgs tmp/dd_5qpe_n3000_seed101.log.avgs > tmp/dd_parallel.log

paste -d "," data/incr_n3000_seed2.log data/incr_n3000_seed4.log data/incr_n3000_seed5.log data/incr_n3000_seed6.log data/incr_n3000_seed7.log data/incr_n3000_seed9.log data/incr_n3000_seed13.log data/incr_n3000_seed15.log data/incr_n3000_seed101.log > tmp/incr_parallel.log
paste -d "," data/n3000_seed2.log data/n3000_seed4.log data/n3000_seed5.log data/n3000_seed6.log data/n3000_seed7.log data/n3000_seed9.log data/n3000_seed13.log data/n3000_seed15.log data/n3000_seed101.log > tmp/batch_parallel.log

# generate scatter plots
echo " ... generating scatter plot: Demand-Driven \& Incremental"
scripts/scatter_config_full.py out/plots/dd_incr_scatter.png dd_incr 9 < tmp/dd_incr_parallel.log
echo " ... generating scatter plot: Demand-Driven"
scripts/scatter_config_full.py out/plots/dd_scatter.png dd 9 < tmp/dd_parallel.log
echo " ... generating scatter plot: Incremental"
scripts/scatter_config_full.py out/plots/incr_scatter.png incr 9 < tmp/incr_parallel.log
echo " ... generating scatter plot: Batch"
scripts/scatter_config_full.py out/plots/batch_scatter.png batch 9 < tmp/batch_parallel.log

# generate CDF at out/plots/cdf.png
echo " ... generating CDF"
scripts/cdf_full.py out/plots/cdf.png 27000 tmp/batch_all.log tmp/incr_all.log tmp/dd_all.log tmp/dd_incr_all.log

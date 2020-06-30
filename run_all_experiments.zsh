for seed in 0 1 2; do
    for n in 100 500 1000; do
	for qpe in 1 3 5; do
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

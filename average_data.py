

postfixes = [".batch", ".incr", ".dd", ".ddincr"]
runs = [f"run{i}/" for i in range(1, 11)]
num_runs = len(runs)
with open('experiment_inputs/query_artifacts') as f:
    programs = [line.strip() for line in f.readlines()]
    num_programs = len(programs)

def processFile(pathToFile):
    with open(pathToFile) as f:
        lines = [line.strip() for line in f.readlines()]
        run_time = float(lines[-2].split()[-1])
        stats = [int(stat.split(',')[0]) for stat in lines[-1].split()[-5:]]
        # stats is a list of ints, representing |D*|, |Delta|, |unique procedures rho in D*|, total cells, and nonempty cells
        return run_time, stats

# print("program, batch, incr, dd, ddincr")
output = 'program'
for mode in postfixes:
    output = output + ', ' + mode
print(output)
total_of_averages = {}
for run_mode in postfixes:
    total_of_averages[run_mode] = 0
for program in programs:
    output = program
    for run_mode in postfixes:
        average = 0
        other_data = None
        for exp_run in runs:
            time, stats = processFile('out/'+exp_run+program+run_mode)
            average = average+time
        average = average/num_runs
        total_of_averages[run_mode] = total_of_averages[run_mode] + average
        output = output + ', ' + str(round(average, 4))
        if other_data is not None and other_data != stats:
            print(f"differing stats! old:{other_data}; new:{stats}")
            stats_same = False
        other_data = stats
    print(output)
print('-'*20)
output = 'average'
for mode in postfixes:
    output = output + ', ' + str(round(total_of_averages[mode]/num_programs, 4))
print(output)

data = [(prog, [(postfix, processFile('out/'+runs[0]+prog+postfix)[1]) for postfix in postfixes]) for prog in programs]
print(data)


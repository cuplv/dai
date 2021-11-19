

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
        return run_time, stats

print("program, batch, incr, dd, ddincr")
output = 'program'
for mode in postfixes:
    output = output + ', ' + mode
total_of_averages = {}
total_of_averages[postfixes[0]] = 0
total_of_averages[postfixes[1]] = 0
total_of_averages[postfixes[2]] = 0
total_of_averages[postfixes[3]] = 0
for program in programs:
    output = program
    for run_mode in postfixes:
        average = 0
        for run in runs:
            time, _ = processFile('out/'+run+program+run_mode)
            average = average+time
        average = average/num_runs
        total_of_averages[run_mode] = total_of_averages[run_mode] + average
        output = output + ', ' + str(round(average, 4))
    print(output)
print('-'*20)
output = 'average'
for mode in postfixes:
    output = output + ', ' + str(round(total_of_averages[mode]/num_programs, 4))
print(output)


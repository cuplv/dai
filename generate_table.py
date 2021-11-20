#!/usr/bin/python

def processFile(pathToFile, incr=None):
    with open(pathToFile) as f:
        lines = [line.strip() for line in f.readlines()]
        run_time = float(lines[-2].split()[-1])
        stats = [int(stat.split(',')[0]) for stat in lines[-1].split()[-5:]]
        # stats is a list of ints, representing |D*|, |Delta|, |unique procedures rho in D*|, total cells, and nonempty cells after analysis
        # depending on the incr flag, also add on the nonempty cells before analysis
        if incr is not None:
            if incr:
                before_stats = [int(stat.split(',')[0]) for stat in lines[-3].split()[-5:]]
                stats.append(before_stats[-1])
            else:
                stats.append(0)
        return run_time, stats


def readFile(pathToFile):
    with open(pathToFile) as f:
        lines = [line.strip() for line in f.readlines()]
        lines = lines[1:] # drop the generation line
        lines = [line.split() for line in lines]
        progMap = {line[0]: int(line[-1]) for line in lines}
        return progMap # map of program names to some integer data


def percent(mode, file_prefix):
    isIncremental = 'incr' in mode
    _, stats = processFile(file_prefix+mode, isIncremental)
    _, batch_stats = processFile(file_prefix+postfixes[0])
    nonEmpty_pre  = stats[-1]
    nonEmpty_post = stats[-2]
    nonEmpty_batch= batch_stats[-1]
    return (nonEmpty_post - nonEmpty_pre) / nonEmpty_batch
def citeProgram(prog):
    return f"\cite{{bugswarm{prog.split('-')[-1]}}}"
def averageDict(dictionary):
    return round(sum(dictionary.values())/len(dictionary), 2)


postfixes = [".batch", ".incr", ".dd", ".ddincr"]
runs = [f"run{i}/" for i in range(1, 11)]
num_runs = len(runs)
with open('experiment_inputs/query_artifacts') as f:
    programs = [line.strip() for line in f.readlines()]
    num_programs = len(programs)

locs = readFile('linesOfSourceCode')
edited_locs = readFile('diffCounts')
callgraph_sizes = readFile('callgraphSize')

output = 'program'
for mode in postfixes:
    output = output + ', ' + mode
print(output)
total_of_averages = {}
total_of_percents = {}
for run_mode in postfixes:
    total_of_averages[run_mode] = 0
    total_of_percents[run_mode] = 0
for program in programs:
    output = f"{citeProgram(program)} & {locs[program]} & {edited_locs[program]} & {callgraph_sizes[program]}"
    fprefix = 'out/'+runs[0]+program
    for run_mode in postfixes:
        average = 0
        other_data = None
        for exp_run in runs:
            time, _ = processFile('out/'+exp_run+program+run_mode)
            average = average+time
        average = average/num_runs
        total_of_averages[run_mode] = total_of_averages[run_mode] + average
        total_of_percents[run_mode] = total_of_percents[run_mode] + percent(run_mode, fprefix)
        output = output + ' & ' + str(round(average, 2))
        output = output + f" & {round(percent(run_mode, fprefix)*100, 2)}"
    print(output + ' \\\\')

print('\midrule')

output = f'average & {averageDict(locs)} & {averageDict(edited_locs)} & {averageDict(callgraph_sizes)}'
for mode in postfixes:
    output = output + f' & {round(total_of_averages[mode]/num_programs, 2)} & {round(total_of_percents[mode]/num_programs*100, 2)}'
print(output)

data = [(prog, [(postfix, processFile('out/'+runs[0]+prog+postfix)[1]) for postfix in postfixes]) for prog in programs]
# print(data)


#!/usr/bin/python

# Generates table 1 for the paper.
# depends on three suplemental files which describe their creation:
#   'linesOfSourceCode'
#   'diffCounts'
#   'callgraphSize'

num_stats = 6
def processFile(pathToFile):
    with open(pathToFile) as f:
        isIncremental = 'incr' in pathToFile
        lines = [line.strip() for line in f.readlines()]
        run_time = float(lines[-2].split()[-1])
        stats = [int(stat.split(',')[0]) for stat in lines[-1].split()[-num_stats:]]
        # stats is a list of ints, representing |D*|, |Delta|, |unique procedures rho in D*|, total cells, and nonempty cells after analysis, and number of self loops in Delta
        # depending on the incr flag, also add on the nonempty cells before analysis
        if isIncremental:
            before_stats = [int(stat.split(',')[0]) for stat in lines[-3].split()[-num_stats:]]
            stats.append(before_stats[4])
        else:
            stats.append(0)
        return run_time, stats


def readFile(pathToFile):
    with open(pathToFile) as f:
        lines = [line.strip().split() for line in f.readlines()]
        # [1:] to drop the file generation line
        progMap = {line[0]: int(line[-1]) for line in lines[1:]}
        return progMap # map of program names to some integer data


def percent(mode, file_prefix):
    _, stats = processFile(file_prefix+mode)
    _, batch_stats = processFile(file_prefix+postfixes[0])
    nonEmpty_pre  = stats[-1]
    nonEmpty_post = stats[4]
    nonEmpty_batch= batch_stats[4]
    return (nonEmpty_post - nonEmpty_pre) / nonEmpty_batch


def citeProgram(prog):
    return f"\cite{{bugswarm{prog.split('-')[-1]}}}"


def averageDict(dictionary):
    return sum(dictionary.values())/len(dictionary)


def strPercent(percentage):
    percent = percentage*100
    if percent == 0:
        return "0.0"
    if percent < 0.1:
        return "<0.1"
    else:
        return f"{percent:.1f}"


def strTime(ms_time):
    seconds = ms_time/1000.0
    if seconds == 0:
        return "0.00"
    if seconds < 0.01:
        return "<0.01"
    else:
        return f"{seconds:.2f}"


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
total_of_absStates = {}
for run_mode in postfixes:
    total_of_averages[run_mode] = 0
    total_of_percents[run_mode] = 0
    total_of_absStates[run_mode] = 0
for program in programs:
    output = f"{citeProgram(program)} & {locs[program]/1000.0:.1f} & {edited_locs[program]} & {callgraph_sizes[program]}"
    fprefix = 'out/'+runs[0]+program
    for run_mode in postfixes:
        average = 0
        other_data = None
        for exp_run in runs:
            time, stats = processFile('out/'+exp_run+program+run_mode)
            average = average+time
        average = average/num_runs
        total_of_averages[run_mode] = total_of_averages[run_mode] + average
        total_of_percents[run_mode] = total_of_percents[run_mode] + percent(run_mode, fprefix)
        num_varphi = stats[4]-stats[-1]
        total_of_absStates[run_mode] = total_of_absStates[run_mode] + num_varphi
        output = output + f" & {num_varphi}"
        if run_mode != postfixes[0]:
            output = output + f" & {strPercent(percent(run_mode, fprefix))}"
        output = output + f' & {strTime(average)}'
        # output = output + f" & |selfloop|/|Delta| {stats[5]/stats[1] if stats[0] != 0 else '0/0'}"
    print(output + ' \\\\')

print('\midrule')

output = f'average & {averageDict(locs)/1000:.1f} & {averageDict(edited_locs):.0f} & {averageDict(callgraph_sizes):.0f}'
for mode in postfixes:
    output = output + f' & {total_of_absStates[mode]/num_programs:.0f}'
    if mode != postfixes[0]:
        output = output + f' & {strPercent(total_of_percents[mode]/num_programs)}'
    output = output + f' & {strTime(total_of_averages[mode]/num_programs)}'
print(output + ' \\\\')

data = [(prog, [(postfix, processFile('out/'+runs[0]+prog+postfix)[1]) for postfix in postfixes]) for prog in programs]
# print(data)


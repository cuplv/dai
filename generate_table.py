#!/usr/bin/python

# Generates table 1 for the paper.
# depends on three suplemental files which describe their creation:
#   'linesOfSourceCode'
#   'diffCounts'
#   'callgraphSize'

num_stats = 6
def processFile(pathToFile):
    # print(f"procesing {pathToFile}")
    with open(pathToFile) as f:
        isIncremental = 'incr' in pathToFile
        lines = [line.strip() for line in f.readlines() if "SUMMARIES" not in line]
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

def multirow(cell):
    return f"\multirow{{2}}{{*}}{{{cell}}}"

def citeProgram(prog):
    return f"\citetalias{{bugswarm{prog.split('-')[-1]}}}"


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

def filePrefix(analysis_prefix, run_number, program):
    return f'out/{analysis_prefix}{run_number}/{program}'


def generateDataRow(analysis, program):
    row_output = ""
    # the abstract work done should be the same for each program, so just use the first file for that
    fprefix = filePrefix(analysis, 1, program)
    for run_mode in postfixes:
        average = 0
        for exp_run in range(1,num_runs+1):
            time, stats = processFile(filePrefix(analysis, exp_run, program)+run_mode)
            average = average+time
        average = average/num_runs
        total_of_averages[analysis,run_mode] = total_of_averages[analysis,run_mode] + average
        total_of_percents[analysis,run_mode] = total_of_percents[analysis,run_mode] + percent(run_mode, fprefix)
        num_varphi = stats[4]-stats[-1]
        total_of_absStates[analysis,run_mode] = total_of_absStates[analysis,run_mode] + num_varphi
        if run_mode == postfixes[0]:
            row_output = row_output + f" & {num_varphi}"
        else:
            row_output = row_output + f" & {strPercent(percent(run_mode, fprefix))}"
        row_output = row_output + f' & {strTime(average)}'
    return row_output + ' \\\\'


def generateAverageRow(analysis):
    row_output = ""
    for mode in postfixes:
        if mode == postfixes[0]:
            row_output = row_output + f' & {total_of_absStates[analysis,mode]/num_programs:.0f}'
        else:
            row_output = row_output + f' & {strPercent(total_of_percents[analysis,mode]/num_programs)}'
        row_output = row_output + f' & {strTime(total_of_averages[analysis,mode]/num_programs)}'
    return row_output + ' \\\\'

postfixes = [".batch", ".incr", ".dd", ".ddincr"]
runs = [f"run{i}/" for i in range(1, 11)]
interval_prefix = "run"
nullability_prefix = "log"
analysis_prefixes = [interval_prefix, nullability_prefix]
num_runs = 10 # 10
excluded_programs = \
        []
    # ["tananaev-traccar-188473749", "tananaev-traccar-255051211", "raphw-byte-buddy-234970609"]
with open('experiment_inputs/query_artifacts') as f:
    programs = [line.strip() for line in f.readlines() ]
    programs = [program for program in programs if program not in excluded_programs]
    num_programs = len(programs)

locs = readFile('linesOfSourceCode')
edited_locs = readFile('diffCounts')
callgraph_sizes = readFile('callgraphSize')

output = 'program'
for mode in postfixes:
    output = output + ', ' + mode
print(output)

offset = 6*" "
total_of_averages = {}
total_of_percents = {}
total_of_absStates = {}
for run_mode in postfixes:
    for analysis in analysis_prefixes:
        total_of_averages[analysis,run_mode] = 0
        total_of_percents[analysis,run_mode] = 0
        total_of_absStates[analysis,run_mode] = 0
first = True
for program in programs:
    if first:
        first = False
    else:
        print(offset+"\\arrayrulecolor{gray}\\hline")
    kloc = f'{locs[program]/1000.0:.1f}'
    output = offset + f"{multirow(citeProgram(program))} & {multirow(kloc)} & {multirow(edited_locs[program])} & {multirow(callgraph_sizes[program])}"
    print(output)
    print(offset + 3*"  " + "& I" + generateDataRow(interval_prefix, program))
    output = offset + 3*"& "
    print(output + "& N" + generateDataRow(nullability_prefix, program))

print(offset + '\\arrayrulecolor{black}\\midrule')


average_kloc = f'{averageDict(locs)/1000:.1f}'
average_eloc = f'{averageDict(edited_locs):.0f}'
average_cg   = f'{averageDict(callgraph_sizes):.0f}'
output = offset + f'{multirow("average")} & {multirow(average_kloc)} & {multirow(average_eloc)} & {multirow(average_cg)}'
print(output)
output = offset + 3*"  " + "& I"
print(output + generateAverageRow(interval_prefix))
output = offset + 3*"& " + "& N"
print(output + generateAverageRow(nullability_prefix))

# data = [(prog, [(postfix, processFile('out/'+runs[0]+prog+postfix)[1]) for postfix in postfixes]) for prog in programs]
# print(data)


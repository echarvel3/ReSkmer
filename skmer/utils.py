from subprocess import check_output, STDOUT

def pop(args):
    return()

def sequence_stat(sequence):
    total_length = 0
    n_reads = 0
    max_length = 0
    # TODO: seqtk comp causing slowing issues in some clusters especially with Skmer subsample.
    # NOTE: potential fix is to replace with "bbduk.sh in=X lhist=X.txt"
    comp_stdout = check_output(["seqtk", "comp", sequence], stderr=STDOUT, universal_newlines=True)
    reads_stat = comp_stdout.split('\n')
    for stat in reads_stat:
        if not stat.strip():
            continue
        read_length = sum([int(x) for x in stat.split('\t')[2:6]])
        total_length += read_length
        max_length = max(max_length, read_length)
        n_reads += 1
    return int(round(1.0 * total_length / n_reads)), max_length, total_length, n_reads

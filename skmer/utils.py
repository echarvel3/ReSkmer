import os
import fnmatch
from subprocess import check_output, STDOUT, call
from skmer.config import *

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

def sketch(sequence, lib, ce, ee, k, s, cov_thres, seed, has_spectrum = False):
    sample = os.path.basename(sequence).rsplit('.f', 1)[0]
    sample_dir = os.path.join(lib, sample)
    msh = os.path.join(sample_dir, sample)
    cov = ce[sample]
    eps = ee[sample]
    if cov == "NA" and eps == 0:
        call(["mash", "sketch", "-k", str(k), "-s", str(s), "-S", str(seed), "-o", msh, sequence], stderr=open(
            os.devnull, 'w'))
        return
    elif eps == "NA":
        call(["mash", "sketch", "-k", str(k), "-s", str(s), "-S", str(seed), "-r", "-o", msh, sequence], stderr=open(
            os.devnull, 'w'))
        return
    copy_thres = int(cov / cov_thres) + 1
    if cov < cov_thres or eps == 0.0 or has_spectrum:
        # ReSkmer or below Skmer high-cov threshold...
        call(["mash", "sketch", "-k", str(k), "-s", str(s), "-S", str(seed), "-r", "-o", msh, sequence], stderr=open(
            os.devnull, 'w'))
    else:
        # high-coverage Skmer
        call(["mash", "sketch", "-m", str(copy_thres), "-k", str(k), "-s", str(s), "-S", str(seed), "-o", msh,
              sequence], stderr=open(os.devnull, 'w'))
    return

def get_samples_from_files(args):
    files_names = [f for f in os.listdir(args.input_dir)
                   if True in (fnmatch.fnmatch(f, '*' + form) for form in formats)]
    samples_names = [f.rsplit('.f', 1)[0] for f in files_names]

    # Check if refs have duplicate entry or no entries
    if not samples_names:
        raise FileNotFoundError("No files with extensions %s found" % " ".join(formats))
    elif len(samples_names) != len(set(samples_names)):
        raise ValueError('Duplicate inputs (possibly same name with different extensions), please change '
                         'the file name(s) and try again')
    return(files_names, samples_names)

def write_config_file(args):
    '''Creating a config file for references'''
    config_file = os.path.join(args.l, 'CONFIG')
    with open(config_file, mode='w') as f:
        f.write('kmer_length\t{0}\n'.format(args.k) + 'sketch_size\t{0}\n'.format(args.s) +
                'sketching_seed\t{0}\n'.format(args.S))
    #TODO: add skmer version to config files, maybe write 
    return()

def assign_skmer_label(args):
    '''assigns "skmer", "reskmer", or "dipskmer" based on input arguments'''
    if args.r:
        skmer_ver = "reskmer"
    elif args.d:
        skmer_ver = "dipskmer"
    else:
        skmer_ver = "skmer"

    if args.r and args.d:
        raise ValueError('Both diploid and repeat equations cannot be used at the same time! Use either -r or -d flags.')
    return(skmer_ver)

#! /usr/bin/env python
# -*- coding: utf-8 -*-

import os
import errno
import sys
import math
import numpy as np
from subprocess import check_output, STDOUT, run, call

from skmer.reskmer.coverage_estimator import estimate_cov_with_ref
from skmer.config import seq_len_threshold, error_rate_threshold

def write_error_file(info_file, cov, g_len, eps, l):
    with open(info_file, mode='w') as f:
        f.write('coverage\t{0}\n'.format(repr(float(round(cov,5)))) + 'genome_length\t{0}\n'.format(g_len) +
                'error_rate\t{0}\n'.format(repr(float(round(eps, 5)))) + 'read_length\t{0}\n'.format(l))

def count_kmers(sample_dir, sample, sequence, k, nth):
    '''runs jellyfish if jellyfish file does not already exist'''
    #TODO: add alternatives to jellyfish to count k-mers
    mercnt = os.path.join(sample_dir, sample + '.jf')
    histo_file = os.path.join(sample_dir, sample + '.hist')

    # Runs jellyfish if .hist file does not exist.
    if (not os.path.exists(histo_file)) or (os.path.getsize(histo_file) == 0):
        mercnt = os.path.join(sample_dir, sample + '.jf')
        # Reads gzipped data
        # NOTE: only works in specific environments? Explore alternative k-mer counters with friendlier gzip reading.
        if sequence.endswith("gz"):
            jellyfish_cmd = ["zcat", sequence, "|", "jellyfish", "count", "-m", str(k), "-s", "100M", "-t", str(nth), "-C", "-o", mercnt, "/dev/fd/0"]
            run(" ".join(jellyfish_cmd), shell=True, check=True)
        else:
            call(["jellyfish", "count", "-m", str(k), "-s", "100M", "-t", str(nth), "-C", "-o", mercnt, sequence],
                stderr=open(os.devnull, 'w'))
        histo_stderr = check_output(["jellyfish", "histo", "-h", "1000000", mercnt], stderr=STDOUT, universal_newlines=True)
        with open(histo_file, mode='w') as f:
            f.write(histo_stderr)
        os.remove(mercnt)
    else:  
        sys.stderr.write('--[!WARNING!] {0}.hist already exists. Using existing file.\n'.format(sample))
        histo_stderr = open(histo_file).read()
    return(histo_stderr)

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

def estimate_cov(sequence, lib, k, e, nth, ref_hist = None):
    sample = os.path.basename(sequence).rsplit('.f', 1)[0]
    sample_dir = os.path.join(lib, sample)

    try:
        os.makedirs(sample_dir)
    except OSError as Error:
        if Error.errno != errno.EEXIST:
            raise
            
    info_file = os.path.join(sample_dir, sample + '.dat')
    
    # Does not recalculate histogram if histogram already exists
    histo_stderr = count_kmers(sample_dir, sample, sequence, k, nth)
    # Calculate read stats: 
    (l, max_len, tot_len, n_reads) = sequence_stat(sequence)
    
    # if sample is assembly...
    if max_len > seq_len_threshold:
        cov = "NA"
        g_len = tot_len
        eps = 0
        l = "NA"
        write_error_file(info_file, cov, g_len, eps, l)
        return sample, cov, g_len, eps, l

    count = [0]
    ksum = 0
    for item in histo_stderr.split('\n')[:-1]:
        count.append(int(item.split()[1]))
        ksum += int(item.split()[0]) * int(item.split()[1])
    # If coverage is too low
    if len(count) < 3:
        sys.stderr.write('Coverage of {0} is too low, not able to estimate it; no correction applied\n'.format(sample))
        cov = "NA"
        g_len = "NA"
        eps = "NA"
        write_error_file(info_file, cov, g_len, eps, l)
        return sample, cov, g_len, eps, l

    ind = min(count.index(max(count[2:])), len(count) - 2)+1
    # If no reskmer reference
    if (e is not None) and (ref_hist is None):
        eps = e
        p0 = np.exp(-k * eps)
        if ind < 2:
            r21 = 1.0 * count[2] / count[1]
            cov = newton(cov_temp_func, 0.05, args=(r21, p0, k, l))
        else:
            cov = (1.0 / p0) * (1.0 * l / (l - k)) * (ind + 1) * count[ind + 1] / count[ind]
    elif ind < 2:
        sys.stderr.write('Not enough information to co-estimate coverage and error rate of {0}; '.format(sample) +
                         'Using default error rate {0}\n'.format(default_error_rate))
        eps = default_error_rate
        p0 = np.exp(-k * eps)
        r21 = 1.0 * count[2] / count[1]
        cov = newton(cov_temp_func, 0.05, args=(r21, p0, k, l))
    else:
        if ref_hist is not None:
            # repeat spectrum-based calculation of error and coverage (reskmer)
            (eps, lam) = estimate_cov_with_ref(ref_hist, ksum, count, k, sample, l, e)
        else:
            gam = 1.0 * (ind + 1) * count[ind + 1] / count[ind]
            lam = (np.exp(-gam) * (gam ** ind) / math.factorial(ind)) * count[1] / count[ind] + gam * (1 - np.exp(-gam))
            eps = 1 - (gam / lam) ** (1.0 / k)
        cov = (1.0 * l / (l - k)) * lam
    tot_seq = 1.0 * ksum * l / (l - k)
    g_len = int(tot_seq / cov)

    if eps > error_rate_threshold or eps < 0:
        cov = "NA"
        g_len = "NA"
        eps = "NA"

    write_error_file(info_file, cov, g_len, eps, l)
    return sample, cov, g_len, eps, l

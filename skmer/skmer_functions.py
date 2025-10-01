#! /usr/bin/env python
# -*- coding: utf-8 -*-

import os
import errno
import fnmatch
import sys
import multiprocessing as mp
import pandas as pd
import shutil
from subprocess import call


from skmer.config import *
from skmer.estimate_parameters import estimate_cov
from skmer.estimate_distance import estimate_skmer_dist
from skmer.reskmer.distance_estimator import estimate_reskmer_dist
from skmer.reskmer.parse_spectrum import parse_reference
from skmer.subsample.subsample import correction, subsample

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

def reference(args):
    # Creating a directory for reference library
    try:
        os.makedirs(args.l)
    except OSError as Error:
        if Error.errno != errno.EEXIST:
            raise

    # Creating a config file for references
    write_config_file(args)

    # Making a list of sample names
    files_names, samples_names = get_samples_from_files(args)

    # Making a list of genome-skim files
    sequences = [os.path.join(args.input_dir, f) for f in files_names]

    # Initializing distance dataframe
    index = pd.MultiIndex.from_product([samples_names, samples_names], names=['sample', 'sample_2'])
    result_df = pd.DataFrame(columns=index)

    # Initializing coverage, genome length, error rate, and read length dictionaries
    cov_est = dict()
    len_est = dict()
    err_est = dict()
    read_len = dict()
    # for DipSkmer exclusively...
    theta = dict()

    # Number of pools and threads for multi-processing
    n_pool = min(args.p, len(sequences))
    n_thread_cov = int(args.p / n_pool)
    n_proc_cov = int(args.p)
    n_pool_dist = min(args.p, len(sequences) ** 2)

    # Checks for incompatible equation input
    skmer_ver = assign_skmer_label(args)

    # If ReSkmer reference is given, read reference.
    ref_hist = parse_reference(args.r, args.k, args.p, args.l) if (skmer_ver == "reskmer") else None

    # Computing coverage, genome length, error rate, and read length
    sys.stderr.write('[{0}] Estimating coverages using {1} processors...\n'.format(skmer_ver, n_proc_cov))
    #pool_cov = mp.Pool(n_pool)

    if skmer_ver != "dipskmer":
        results_cov = [estimate_cov(seq, args.l, args.k, args.e, n_thread_cov, ref_hist) for seq in sequences]
        for result in results_cov:
            (name, coverage, genome_length, error_rate, read_length) = result
            cov_est[name] = coverage
            len_est[name] = genome_length
            err_est[name] = error_rate
            read_len[name] = read_length

    else:
        results_cov = [estimate_diploid_cov(seq, args.l, args.k, args.e, n_thread_cov, args.theta) for seq in sequences]
        for result in results_cov:
            (name, coverage, genome_length, error_rate, read_length, theta_val) = result
            cov_est[name] = coverage
            len_est[name] = genome_length
            err_est[name] = error_rate
            read_len[name] = read_length
            theta[name] = theta_val
    
    # Sketching genome-skims
    sys.stderr.write('[{0}] Sketching sequences using {1} processors...\n'.format(skmer_ver, n_pool))
    pool_sketch = mp.Pool(n_pool)

    if skmer_ver == "reskmer":
        results_sketch = [pool_sketch.apply_async(sketch, args=(seq, args.l, cov_est, err_est, args.k, args.s,
                                                            coverage_threshold, args.S, True)) for seq in sequences]
    elif skmer_ver == "dipskmer":
        results_sketch = [pool_sketch.apply_async(sketch, args=(seq, args.l, cov_est, err_est, args.k, args.s,
                                                            dip_coverage_threshold, args.S, False)) for seq in sequences]
    else:
        results_sketch = [pool_sketch.apply_async(sketch, args=(seq, args.l, cov_est, err_est, args.k, args.s,
                                                            coverage_threshold, args.S, False)) for seq in sequences]
    
    for result in results_sketch:
        result.get(9999999)
    pool_sketch.close()
    pool_sketch.join()

    # Estimating pair-wise distances
    sys.stderr.write('[{0}] Estimating distances using {1} processors...\n'.format(skmer_ver, n_pool_dist))
    pool_dist = mp.Pool(n_pool_dist)
    if skmer_ver == "reskmer":
        results_dist = [pool_dist.apply_async(estimate_reskmer_dist, args=(s1, s2, args.l, args.l, cov_est, len_est,
                                                               err_est, read_len, args.k, coverage_threshold, args.t, ref_hist))
                    for s1 in samples_names for s2 in samples_names]
    elif skmer_ver == "dipskmer":
        results_dist = [pool_dist.apply_async(estimate_dipskmer_dist, args=(s1, s2, args.l, args.l, cov_est, len_est,
                                                               err_est, read_len, args.k, dip_coverage_threshold, args.t, theta))
                    for s1 in samples_names for s2 in samples_names]
    else:
        results_dist = [pool_dist.apply_async(estimate_skmer_dist, args=(s1, s2, args.l, args.l, cov_est, len_est,
                                                               err_est, read_len, args.k, coverage_threshold, args.t))
                    for s1 in samples_names for s2 in samples_names]


    for result in results_dist:
        dist_output = result.get(9999999)
        result_df[(dist_output[0], dist_output[1])] = [repr(dist_output[2])]

    # Writing distances to file
    sys.stderr.write('[{0}] Writing to file...\n'.format(skmer_ver))
    result_dfm = pd.melt(result_df, value_name='distance')
    result_mat = result_dfm.pivot(index='sample', columns='sample_2', values='distance')
    result_mat.to_csv(args.o + ".txt", sep='\t', mode='w')

def distance(args):
    skmer_ver = assign_skmer_label(args)
    # Loading reference config
    config_file = os.path.join(args.library, 'CONFIG')
    with open(config_file) as f:
        config = f.read()
    kl = int(config.split('\n')[0].split('\t')[1])

    # Making a list of reference samples
    refs = [item for item in os.listdir(args.library) if os.path.isdir(os.path.join(args.library, item))]
 #   print(refs)
    # Initializing distance dataframe
    index = pd.MultiIndex.from_product([refs, refs], names=['sample', 'sample_2'])
    result_df = pd.DataFrame(columns=index)

    # Loading coverage, genome length, error rate, and read length information
    cov_est = dict()
    len_est = dict()
    err_est = dict()
    read_len = dict()
    is_diploid = args.d
    if is_diploid:
        theta = dict()

    for ref in refs:
        ref_dir = os.path.join(args.library, ref)
        info_file = os.path.join(ref_dir, ref + '.dat')
    # print(info_file)
        with open(info_file) as f:
            info = f.read()
        cov_value = info.split('\n')[0].split('\t')[1]
        gl_value = info.split('\n')[1].split('\t')[1]
        if cov_value == "NA":
            if gl_value == "NA":
                cov_est[ref] = "NA"
                len_est[ref] = "NA"
                err_est[ref] = "NA"
                read_len[ref] = int(info.split('\n')[3].split('\t')[1])
                if is_diploid:
                    theta[ref] = default_theta
            else:
                cov_est[ref] = "NA"
                len_est[ref] = int(info.split('\n')[1].split('\t')[1])
                err_est[ref] = 0
                read_len[ref] = "NA"
                if is_diploid:
                    theta[ref] = default_theta
        else:
            cov_est[ref] = float(info.split('\n')[0].split('\t')[1])
            len_est[ref] = int(info.split('\n')[1].split('\t')[1])
            err_est[ref] = float(info.split('\n')[2].split('\t')[1])
            read_len[ref] = int(info.split('\n')[3].split('\t')[1])
            if is_diploid:
                theta[ref] = float(info.split('\n')[4].split('\t')[1])

    # Number of pools and threads for multi-processing
    n_pool_dist = min(args.p, len(refs) ** 2)

    # Estimating pair-wise distances
    sys.stderr.write('[{0}] Estimating distances using {1} processors...\n'.format(skmer_ver, n_pool_dist))
    pool_dist = mp.Pool(n_pool_dist)

    if is_diploid:
        results_dist = [pool_dist.apply_async(estimate_dipskmer_dist, args=(r1, r2, args.library, args.library, cov_est, len_est,
                                                               err_est, read_len, kl, dip_coverage_threshold, args.t, theta))
                    for r1 in refs for r2 in refs]
    elif args.r is not None:
        ref_hist=parse_reference(args.r, kl, args.p, args.library)
        results_dist = [pool_dist.apply_async(estimate_reskmer_dist, args=(r1, r2, args.library, args.library, cov_est, len_est,
                                                               err_est, read_len, kl, coverage_threshold, args.t, ref_hist))
                    for r1 in refs for r2 in refs]
    else:
        results_dist = [pool_dist.apply_async(estimate_skmer_dist, args=(r1, r2, args.library, args.library, cov_est, len_est,
                                                               err_est, read_len, kl, coverage_threshold, args.t))
                    for r1 in refs for r2 in refs]


    for result in results_dist:
        dist_output = result.get(9999999)
        result_df[(dist_output[0], dist_output[1])] = [repr(dist_output[2])]

    # Writing distances to file
    sys.stderr.write('[{0}] Writing to file...\n'.format(skmer_ver))
    result_dfm = pd.melt(result_df, value_name='distance')
    result_mat = result_dfm.pivot(index='sample', columns='sample_2', values='distance')
    result_mat.to_csv(args.o + ".txt", sep='\t', mode='w')

def query(args):
    # Loading reference config
    skmer_ver = assign_skmer_label(args)

    config_file = os.path.join(args.library, 'CONFIG')
    with open(config_file) as f:
        config = f.read()
    kl = int(config.split('\n')[0].split('\t')[1])
    ss = int(config.split('\n')[1].split('\t')[1])
    try:
        seed = int(config.split('\n')[2].split('\t')[1])
    except IndexError:
        seed = 42

    # Creating a directory for the query
    sample = os.path.basename(args.input).rsplit('.f', 1)[0]
    sample_dir = os.path.join(os.getcwd(), sample)
    try:
        os.makedirs(sample_dir)
    except OSError as Error:
        if Error.errno != errno.EEXIST:
            raise

    # Making a list of references samples
    refs = [item for item in os.listdir(args.library) if os.path.isdir(os.path.join(args.library, item))]

    # Check if the sample is already in the refs
    if sample in set(refs):
        raise ValueError('A reference sample exists with the same name as the query, please change '
                         'the name of query file {0} and try again'.format(sample))

    # Initializing distances series
    result_s = pd.Series(index=refs, name=sample)

    # Loading coverage, genome length, error rate, and read length information
    cov_est = dict()
    len_est = dict()
    err_est = dict()
    read_len = dict()
    # for DipSkmer exclusively...
    is_diploid = args.d
    if is_diploid:
        theta = dict()

    for ref in refs:
        ref_dir = os.path.join(args.library, ref)
        info_file = os.path.join(ref_dir, ref + '.dat')
        with open(info_file) as f:
            info = f.read()
        cov_value = info.split('\n')[0].split('\t')[1]
        gl_value = info.split('\n')[1].split('\t')[1]
        if cov_value == "NA":
            if gl_value == "NA":
                cov_est[ref] = "NA"
                len_est[ref] = "NA"
                err_est[ref] = "NA"
                read_len[ref] = int(info.split('\n')[3].split('\t')[1])
                if is_diploid:
                    theta[ref] = default_theta
            else:
                cov_est[ref] = "NA"
                len_est[ref] = int(info.split('\n')[1].split('\t')[1])
                err_est[ref] = 0
                read_len[ref] = "NA"
                if is_diploid:
                    theta[ref] = default_theta

        else:
            cov_est[ref] = float(info.split('\n')[0].split('\t')[1])
            len_est[ref] = int(info.split('\n')[1].split('\t')[1])
            err_est[ref] = float(info.split('\n')[2].split('\t')[1])
            read_len[ref] = int(info.split('\n')[3].split('\t')[1])
            if is_diploid:
                theta[ref] = int(info.split('\n')[4].split('\t')[1])
            

    # Number of pools for multi-processing
    n_pool_dist = min(args.p, len(refs))

    # Processing Reference Histogram
    ref_hist = parse_reference(args.r, kl, args.p, args.library) if args.r else None

    # Computing the coverage, genome length, error rate, and read length of query sample
    sys.stderr.write('[{0}] Estimating the coverage using {1} processors...\n'.format(skmer_ver, args.p))
    #(dummy, coverage, genome_length, error_rate, read_length) = estimate_cov(args.input, os.getcwd(), kl, args.e,
    #                                                                         args.p)
    if is_diploid:
        print("here:")
        (dummy, coverage, genome_length, error_rate, read_length, theta_val) = estimate_diploid_cov(args.input, os.getcwd(), kl, args.e, args.p, args.theta)
        print('end')
        cov_est[sample] = coverage
        len_est[sample] = genome_length
        err_est[sample] = error_rate
        read_len[sample] = read_length
        theta[sample] = theta_val
    else: 
        (dummy, coverage, genome_length, error_rate, read_length) = estimate_cov(args.input, os.getcwd(), kl, args.e, 
                                                                                args.p, ref_hist)
        cov_est[sample] = coverage
        len_est[sample] = genome_length
        err_est[sample] = error_rate
        read_len[sample] = read_length

    # Sketching the query genome-skim
    sys.stderr.write('[{0}] Sketching the genome-skim...\n'.format(skmer_ver))
    if args.r is not None:
        sketch(args.input, os.getcwd(), cov_est, err_est, kl, ss, coverage_threshold, seed, True)
    if is_diploid:
        sketch(args.input, os.getcwd(), cov_est, err_est, kl, ss, dip_coverage_threshold, seed, False)
    else:
        sketch(args.input, os.getcwd(), cov_est, err_est, kl, ss, coverage_threshold, seed, False)

    # Estimating pair-wise distances
    sys.stderr.write('[{0}] Estimating distances using {1} processors...\n'.format(skmer_ver, n_pool_dist))
    pool_dist = mp.Pool(n_pool_dist)
    if is_diploid:
        results_dist = [pool_dist.apply_async(estimate_dipskmer_dist, args=(sample, ref, os.getcwd(), args.library, cov_est, len_est,
                                                               err_est, read_len, kl, dip_coverage_threshold, args.t, theta)) for ref in refs]
    elif args.r is not None:
        results_dist = [pool_dist.apply_async(estimate_reskmer_dist, args=(sample, ref, os.getcwd(), args.library, cov_est, len_est,
                                                               err_est, read_len, kl, coverage_threshold, args.t, ref_hist)) for ref in refs]
    else:
        results_dist = [pool_dist.apply_async(estimate_skmer_dist, args=(sample, ref, os.getcwd(), args.library, cov_est, len_est,
                                                               err_est, read_len, kl, coverage_threshold, args.t)) for ref in refs]
    
    for result in results_dist:
        dist_output = result.get(9999999)
        result_s[dist_output[1]] = dist_output[2]

    # Writing distances to file
    sys.stderr.write('[{0}] Writing to file...\n'.format(skmer_ver))
    result_s.sort_values(inplace=True)
    result_sr = result_s.apply(repr)
    result_sr.to_csv('{0}-{1}.txt'.format(args.o, sample.lower()), sep='\t', mode='w')

    # Adding query to the reference library
    if args.a:
        try:
            shutil.copytree(sample_dir, os.path.join(args.library, sample))
        except shutil.Error as e:
            print('Directory not copied. Error: %s' % e)
        except OSError as e:
            print('Directory not copied. Error: %s' % e)

    shutil.rmtree(sample_dir)

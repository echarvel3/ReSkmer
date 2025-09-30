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
    sys.stderr.write('[skmer] Estimating coverages using {0} processors...\n'.format(n_proc_cov))
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
#! /usr/bin/env python
# -*- coding: utf-8 -*-


import numpy as np
from scipy.optimize import newton, brenth, minimize
import argparse
import os
import shutil
import fnmatch
import sys
import errno
import pandas as pd
import subprocess
from subprocess import call, check_output, STDOUT
import multiprocessing as mp
import io

from sympy.solvers import solve
from sympy import Symbol
import math
from  numpy import random
# import glob

__version__ = 'skmer 4.3.0'

# Hard-coded param
coverage_threshold = 5
error_rate_threshold = 0.03
seq_len_threshold = 2000
default_error_rate = 0.01

############################################
##            SKMER-2 EQUATIONS           ##
############################################

def parse_reference(reference_path, k, nth, library):
    ext=reference_path.split('.')[-1]
    ref_hist=None
    
    print("unadjusted:", ref_hist)
    if (ext == 'hist'):
        ref_hist = pd.read_csv(reference_path, sep=' ', header=None)

    elif (ext == 'txt'):    
        ref_hist = pd.read_csv(reference_path, sep='\t', header=0)

        ref_names = pd.Series([x.rsplit('.', 1)[0] for x in ref_hist.pop('sample')])
        ref_lengths = ref_hist.pop('genome_length')
        ref_hist = ref_hist.transpose()
        ref_hist = ref_hist.rename(columns=ref_names)

        ref_lengths = pd.concat([ref_lengths], axis=1).transpose().rename(columns=ref_names)

        ref_hist = ref_hist.reset_index(drop=True)
        ref_hist = pd.concat([pd.Series([float(x) for x in range(1,ref_hist.shape[0]+1)]), ref_hist], axis=1)
        print("unadjusted:", ref_hist)
        for sample in ref_names:
            genome_length = ref_lengths[sample].loc['genome_length']
            ksum = np.dot(ref_hist.iloc[:, 0], ref_hist[sample])
            
            # update_r1 = True
            # if update_r1:
            #new_Rs = ((genome_length) / (ksum)) * ref_hist[sample]
            #ref_hist[sample] = [int(x) for x in new_Rs]
            # else:
            #     if ksum != ref_hist[sample].iloc[0]:
            new_Rs = ((genome_length - ref_hist[sample].iloc[0]) / (ksum - ref_hist[sample].iloc[0])) * ref_hist[sample].iloc[1:]
            #ref_hist[sample].iloc[1:] = [int(x) for x in new_Rs]
            ref_hist.loc[1:,sample] = [int(x) for x in new_Rs]


        stop_count = 0
        # print(ref_hist.shape[0])

        # print(ref_hist)
        pseudo_count = pow(10, -10)
        #print(pseudo_count)

        for sample in ref_names:
            
            slope = (math.log(ref_hist.loc[10, sample] + pseudo_count) - math.log(ref_hist.loc[len(ref_hist)-3, sample] + pseudo_count)/2 - math.log(ref_hist.loc[len(ref_hist)-2, sample] + pseudo_count)/2) / 40
            #print(sample, slope)
            stop_count = 0
            for i in random.randint(10,ref_hist.shape[0]-2, 5000):
                #print(i)
                # IF the difference between one spectra and the next is LARGER THAN THE SLOPE:
                if abs(math.log(ref_hist.loc[i, sample] + pseudo_count) - math.log(ref_hist.loc[i+1, sample] + pseudo_count)) > slope:

                    if ref_hist.iloc[i,1] < ref_hist.iloc[i+1, 1]:
                        y = (i*ref_hist.loc[i, sample]  + (3*i+1) * ref_hist.loc[i+1, sample]            ) / (2 * (2*i+1))
                    else:
                        y = ( (i+2) * ref_hist.loc[i+1, sample]  + 3 *(i) * ref_hist.loc[i, sample] ) / (2 * (2*i+1))
                    
                    x = (ref_hist.loc[i, sample] *i + ref_hist.loc[i+1, sample] *(i+1) - y * (i+1)) / i

                    ref_hist.loc[i, sample] = int(x)
                    ref_hist.loc[i+1, sample]  = int(y)
                    #print(x, y)
                    stop_count = 0
                else:
                    #print(".", end = " ")
                    stop_count = stop_count + 1
                
                if (stop_count == 50):
                    #print('end')
                    break

       # ref_hist.pop(0)

        # OLD RESPECT INPUT PROCESSING (SINGLE INPUT) #   
        # ref_hist = pd.read_csv(reference_path, sep='\t', header=0).drop(labels='sample', axis=1).transpose().iloc[:,0]
        # ref_hist = ref_hist.reset_index(drop=True)
        # ref_hist = pd.concat([pd.Series([float(x) for x in range(1,ref_hist.shape[0]+1)]), ref_hist], axis=1)

        # data_path = reference_path.split("/")
        # data_file = "/".join(data_path[:-1] + [data_path[-1].replace("spectra", "parameters")])
        # genome_length = int(pd.read_csv(data_file, sep='\t', header=0)["genome_length"].iloc[0])

        # ksum = np.dot(ref_hist.iloc[:, 0], ref_hist.iloc[:, 1])
        # update_r1 = False
        # if update_r1:
        #     new_Rs = ((genome_length - ref_hist.iloc[0,1]) / (ksum - ref_hist.iloc[0,1])) * ref_hist.iloc[1:,1]
        #     ref_hist.iloc[1:,1] = [int(x) for x in new_Rs]
        # else:
        #     new_Rs = ((genome_length) / (ksum )) * ref_hist.iloc[0:,1]
        #     ref_hist.iloc[0:,1] = [int(x) for x in new_Rs]

    elif (ext[0] == 'f'):
        sample = os.path.basename(reference_path).rsplit('.f', 1)[0]
        mercnt = os.path.join(library, sample + ".jf")
        print(mercnt, reference_path)
        call(["jellyfish", "count", "-m", str(k), "-s", "100M", "-t", str(nth), "-C", "-o", mercnt, reference_path], stderr=open(os.devnull, 'w'))
        histo_stderr = io.StringIO(check_output(["jellyfish", "histo", "-h", "1000000",  mercnt], stderr=STDOUT, universal_newlines=True))
        os.remove(mercnt)
        ref_hist = pd.read_csv(histo_stderr, sep=' ', header=None)
    return ref_hist

def get_hist_data(lib, sample):
    sample_dir = os.path.join(lib, sample)
    histo_file = os.path.join(sample_dir, sample + '.hist')
    ref_hist = pd.read_csv(histo_file, sep=' ', header=None)
    ksum = np.dot(ref_hist.iloc[:, 0], ref_hist.iloc[:, 1])
    usum = sum(ref_hist.iloc[:, 1])
    return ref_hist, ksum, usum

def estimate_intersection(ref_hist, sliced_ref_hist, lam1, lam2, eps1, eps2, eta1, eta2, d, k, num_terms):
    '''calculates exp|AuB|?'''

    nonerr_term1 = 1 - np.power(1-eta1, ref_hist.iloc[:,0])
    nonerr_term2  = 1 - np.power((1-eta2*((1-d)**k)), ref_hist.iloc[:,0])
    nonerr_ins = np.dot(ref_hist.iloc[:,1], nonerr_term1*nonerr_term2)
    
    print("I_0:\n", nonerr_ins)

    b = k*(1-math.exp(-1/(3*k)))
    print("b:", b)
    if eps1:
        n1 = 1-np.exp(-1*ref_hist.iloc[:,0]*b*lam1*eps1*np.power(1-eps1,k-1))
        print("n1:\n", n1)
    else:
        n1 = 0
    if eps2:
        n21 = np.power(1-d,k)*np.exp(-1*b*lam2*eps2*np.power(1-eps2, k-1))
        n22 = d*np.power(1-d,k-1)*b*(np.exp(-1*lam2*np.power(1-eps2, k))-1)
        n23 = 1 - np.power(1-d,k)
    else:
        n21 = 0
        n22 = 0

    term1 = n1
    print("term1:\n", term1)
    term2 = 1-np.power(n21 + n22 + n23, ref_hist.iloc[:,0])
    print("term2:\n", term2)
    extra_ins = 3*k*np.dot(ref_hist.iloc[:,1], term1*term2)
    print("I_1:\n", extra_ins)

    return np.dot([1, 1], [nonerr_ins, extra_ins])

def intersection_fnctn(ref_hist, sliced_ref_hist, msh_int, cov_1, cov_2, eps_1, eps_2, read_len_1, read_len_2, k, num_terms, log_funct = False, is_lambda=False):
    '''takes GENOME ASSEMBLY as input? returns function of est exp|AuB| - obs|AuB|'''
    
   #use this when coverage is already lambda
    if not is_lambda:
        lam1 = cov_1 * (read_len_1 - k + 1) / read_len_1 if read_len_1 != "NA" else None 
        lam2 = cov_2 * (read_len_2 - k + 1) / read_len_2 if read_len_2 != "NA" else None
        print(lam1, lam2)
    else:
        lam1 = cov_1
        lam2 = cov_2

    eta1 = 1 - np.exp(-lam1 * ((1-eps_1)**k)) if eps_1 else 1
    eta2 = 1 - np.exp(-lam2 * ((1-eps_2)**k)) if eps_2 else 1
    
    if log_funct:
        for x in range(0,20):
            z=x/100.0
            print(x)
            print(z,estimate_intersection(ref_hist, sliced_ref_hist, lam1, lam2, eps_1, eps_2, eta1, eta2, z, k, num_terms), msh_int) 

    zde = estimate_intersection(ref_hist,  sliced_ref_hist,lam1, lam2, eps_1, eps_2, eta1, eta2, 0.0, k, num_terms)
    if (((zde - msh_int) / zde) < 0.01):
        #if (((zde - msh_int) / zde) > -0.01):
        msh_int = zde
        
    def g(est_d):
       return estimate_intersection(ref_hist, sliced_ref_hist, lam1, lam2, eps_1, eps_2, eta1, eta2, est_d, k, num_terms) - msh_int

    return g 


def estimate_dist(sample_1, sample_2, lib_1, lib_2, ce, le, ee, rl, k, cov_thres, tran, ref_hist_df):
    if ref_hist_df.shape[1] > 2:
        ref_hist = ref_hist_df[[0, sample_1]]
    elif ref_hist_df.shape[1] == 2:
        ref_hist = ref_hist_df

    try:
        if sample_1 == sample_2 and lib_1 == lib_2:
            return sample_1, sample_2, 0.0
        
        sample_dir_1 = os.path.join(lib_1, sample_1)
        sample_dir_2 = os.path.join(lib_2, sample_2)

        gl_1 = le[sample_1]
        gl_2 = le[sample_2]

        #if gl_1 == "NA" or gl_2 == "NA":
        #    gl_1 = 1
        #    gl_2 = 1
        cov_1 = ce[sample_1]
        cov_2 = ce[sample_2]
        eps_1 = ee[sample_1] if ee[sample_1] != "NA" else None
        eps_2 = ee[sample_2] if ee[sample_2] != "NA" else None
        #eps_1 = eps_2 = 0.005
        l_1 = rl[sample_1]
        l_2 = rl[sample_2]

        hist_1, size_1, usize_1 = get_hist_data(lib_1, sample_1)
        hist_2, size_2, usize_2 = get_hist_data(lib_2, sample_2)

        msh_1 = os.path.join(sample_dir_1, sample_1 + ".msh")
        msh_2 = os.path.join(sample_dir_2, sample_2 + ".msh")
        dist_stderr = check_output(["mash", "dist", msh_1, msh_2], stderr=STDOUT, universal_newlines=True)

        
        j = float(dist_stderr.split()[4].split("/")[0]) / float(dist_stderr.split()[4].split("/")[1])
        i = j * (usize_1 + usize_2) / (1.0 + j)

        num_terms= min(5,len(ref_hist))
        genome_size = np.dot(ref_hist.iloc[:, 0], ref_hist.iloc[:, 1]) 
        
        if (False):
            adjusted_hist = ref_hist.copy() 
            adjusted_hist.iloc[:,1] = ref_hist.iloc[:, 1] * float(gl_2+gl_1) / float(genome_size) / 2.0
        else:
            adjusted_hist = ref_hist
            #cov_1 *= float(gl_1/genome_size)
            #cov_2 *= float(gl_2/genome_size)
            cov_1 = float(size_1/genome_size)
            cov_2 = float(size_2/genome_size)
            #cov_1 = cov_2 = 1.6

        if True:
            num_unique_kmers = sum((1)*ref_hist.iloc[i,1] for i in range(0,len(ref_hist)))

            print(sample_1, sample_2)
            print(adjusted_hist)
            sliced_adjusted_hist = adjusted_hist[:num_terms].copy()
            #sliced_adjusted_hist.iloc[num_terms-1,1] += (np.dot(adjusted_hist.iloc[num_terms:,1],adjusted_hist.iloc[num_terms:,0]))/(num_terms)
            sliced_adjusted_hist.iloc[num_terms-1,1] += np.sum(adjusted_hist.iloc[num_terms:,1])
            sliced_ln = np.dot(sliced_adjusted_hist.iloc[:,1],sliced_adjusted_hist.iloc[:,0])
            print(genome_size)
            print("jaccard:", j,  num_unique_kmers, 
                  "reference genome size:", genome_size, 
                  "sliced length:", sliced_ln,
                  "sliced adj hist:", sliced_adjusted_hist,
                  "genome size:", gl_1, gl_2, 
                  "kmer set sizes:", usize_1, usize_2, 
                  "intersection:", i, 
                  "coverages:", cov_1, cov_2, 
                  "epsilon:", eps_1,  eps_2, 
                  "read lengths:", l_1, l_2, 
                  "kmer size:", k, 
                  "num terms:", num_terms, "\n")
        
        intersection_fnctn(adjusted_hist, sliced_adjusted_hist, i, cov_1, cov_2, eps_1, eps_2, l_1, l_2, k, num_terms, True, True)
        d = brenth(intersection_fnctn(adjusted_hist,  sliced_adjusted_hist, i, cov_1, cov_2, eps_1, eps_2, l_1, l_2, k, num_terms, False, True), 0, 1)
        print(sample_1, sample_2, d , "\n")
        print("----------------------------------")
        if tran:
            if d < 0.75:
                d = max(0, -0.75 * np.log(1 - 4.0 * d / 3.0))
            else:
                d = 5.0
        return sample_1, sample_2, d
    except Exception as e:
        print(e)
        return sample_1, sample_2, None
    
def estimate_cov_from_r(ref_hist_df, ksum, count, k, sample, l, e, range_start=None, range_end=None, maxj=None):
        # Check if reference is one sample or mutliple
        if ref_hist_df.shape[1] > 2:
            ref_hist = ref_hist_df[[0, sample.replace("8x","4x")]]
        elif ref_hist_df.shape[1] == 2:
            ref_hist = ref_hist_df

        genome_size = np.dot(ref_hist.iloc[:, 0], ref_hist.iloc[:, 1]) 
        lam = float(ksum/genome_size)
        hl = 2
        maxj = min(ref_hist.shape[0], 50)
        erscore = {}
        for hl in range(2,10*(int(lam)+1)):
                if range_start:
                    hrange=range(range_start, range_end)
                else:
                    hrange=range(max(int(lam),2),min(max((int(lam)+hl),5),len(count)))
                def estim_oh(xi, hr = hrange, l2= False):
                    #print("I am here, ",xi)
                    errs = [(count[h] - np.dot( 
                              ref_hist.iloc[0:maxj,1],
                              np.array( [np.exp(-j*xi) * np.power(j*xi,h) / math.factorial(h) for j in range(1, maxj+1)])
                          ))/math.sqrt(count[h]) for h in hr]
                    if l2:
                        err = sum((e ** 2 for e in errs)) /  len(errs)
                    else:
                        err = sum( errs )
                    #print("returning", err)
                    return(err)
                def xi_function():
                    fxn = (lambda xi : estim_oh(xi,hrange, True) )
                    return(fxn)
                #for x in range(10, int(lam*110)):
                #    print(x/100.0, 1 - (x/100.0 / lam) ** (1.0 / k),  xi_function(ref_hist, h)(x/100.0))
                try:
                    xi = minimize(xi_function(), lam*((1-0.003)**k), bounds=[(lam*(1-0.03)**k,lam*((1-0.0001)**k))]) # Note 0.03 is maximum error allowed
                    #print(xi)
                    xi = xi.x[0]
                    #print(xi)
                    eps = 1 - (xi / lam) ** (1.0 / k)
                    eps2 = 1 - (xi / (4.0/((1.0 * l / (l - k))))) ** (1.0 / k)
                    eps3 = 1 - (xi / (8.0/((1.0 * l / (l - k))))) ** (1.0 / k)
                    #estim_oh(xi,True)
                    #print(xi)
                    erscore[estim_oh(xi,hrange,True)] = (eps, hl)  
                except ValueError:
                    print(traceback.format_exc())
                    eps = -1
        #print([("%.3f" % k,erscore[k]) for k in sorted(erscore.keys())])
        if erscore.keys():
            eps = erscore[min(erscore.keys())][0]
            hrange = range(hrange[0],erscore[min(erscore.keys())][1]+hrange[0])
        else: 
                eps = -1
                hrange = None
        #print("epsilon, lambda estimates, computed at h range: ", eps, lam, hrange)
        cov = (1.0 * l / (l - k)) * lam
        print( "%.4f" % eps,"%.3f" %  lam,"%.3f" %  cov, hrange, maxj, genome_size)
        eps = eps if e is None else e
        return (eps, lam)

############################################

############################################
##               FST   CODE               ##
############################################

def fst(args):
    return None

############################################

def sequence_stat(sequence):
    total_length = 0
    n_reads = 0
    max_length = 0
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


def sample_reads(sequence, seed, bl_sz, bs_dir):
   
    try:
        os.makedirs(bs_dir)
    except OSError as Error:
        if Error.errno != errno.EEXIST:
            raise

    bs_rep = os.path.join(bs_dir, os.path.split(sequence)[-1])
  
    with open(bs_rep, 'w') as fp: 
        subprocess.run(["seqtk", "sample",  "-s", str(seed), sequence, str(bl_sz)], stdout=fp) 

    return 


def cov_temp_func(x, r, p, k, l):
    lam = x * (1.0 * (l - k)) / l
    return lam * (p ** 2) * np.exp(-lam * p) - 2 * r * (p * np.exp(-lam * p) + 1 - p)

def estimate_cov(sequence, lib, k, e, nth, ref_hist=None):
    sample = os.path.basename(sequence).rsplit('.f', 1)[0]
    sample_dir = os.path.join(lib, sample)
    try:
        os.makedirs(sample_dir)
    except OSError as Error:
        if Error.errno != errno.EEXIST:
            raise
    info_file = os.path.join(sample_dir, sample + '.dat')

    histo_file = os.path.join(sample_dir, sample + '.hist')
    if not os.path.exists(histo_file):
        mercnt = os.path.join(sample_dir, sample + '.jf')
        call(["jellyfish", "count", "-m", str(k), "-s", "100M", "-t", str(nth), "-C", "-o", mercnt, sequence],
             stderr=open(os.devnull, 'w'))
        histo_stderr = check_output(["jellyfish", "histo", "-h", "1000000", mercnt], stderr=STDOUT, universal_newlines=True)
        with open(histo_file, mode='w') as f:
            f.write(histo_stderr)
        os.remove(mercnt)
    else:
        histo_stderr = open(histo_file).read()
        #print(histo_stderr)
        print("========================")

    (l, ml, tl, n_reads) = sequence_stat(sequence)
    if ml > seq_len_threshold:
        cov = "NA"
        g_len = tl
        eps = 0
        l = "NA"
        with open(info_file, mode='w') as f:
            f.write('coverage\t{0}\n'.format(cov) + 'genome_length\t{0}\n'.format(g_len) +
                    'error_rate\t{0}\n'.format(eps) + 'read_length\t{0}\n'.format(l))
        return sample, cov, g_len, eps, l

    count = [0]
    ksum = 0
    for item in histo_stderr.split('\n')[:-1]:
        count.append(int(item.split()[1]))
        ksum += int(item.split()[0]) * int(item.split()[1])
    if len(count) < 3:
        sys.stderr.write('Coverage of {0} is too low, not able to estimate it; no correction applied\n'.format(sample))
        cov = "NA"
        g_len = "NA"
        eps = "NA"
        with open(info_file, mode='w') as f:
            f.write('coverage\t{0}\n'.format(cov) + 'genome_length\t{0}\n'.format(g_len) +
                    'error_rate\t{0}\n'.format(eps) + 'read_length\t{0}\n'.format(l))
        return sample, cov, g_len, eps, l
    ind = min(count.index(max(count[2:])), len(count) - 2)+1
    print("index is : " + str(ind))
    print(sequence)
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
            #CALLED HERE
            (eps, lam) = estimate_cov_from_r(ref_hist, ksum, count, k, sample, l, e)
            print("epsilon:", eps)
        else:
            gam = 1.0 * (ind + 1) * count[ind + 1] / count[ind]
            lam = (np.exp(-gam) * (gam ** ind) / np.math.factorial(ind)) * count[1] / count[ind] + gam * (1 - np.exp(-gam))
            eps = 1 - (gam / lam) ** (1.0 / k)
        cov = (1.0 * l / (l - k)) * lam
    tot_seq = 1.0 * ksum * l / (l - k)
    g_len = int(tot_seq / cov)

    if eps > error_rate_threshold or eps < 0:
        cov = "NA"
        g_len = "NA"
        eps = "NA"
        with open(info_file, mode='w') as f:
            f.write('coverage\t{0}\n'.format(cov) + 'genome_length\t{0}\n'.format(g_len) +
                    'error_rate\t{0}\n'.format(eps) + 'read_length\t{0}\n'.format(l))
        return sample, cov, g_len, eps, l

    with open(info_file, mode='w') as f:
        f.write('coverage\t{0}\n'.format(repr(cov)) + 'genome_length\t{0}\n'.format(g_len) +
                'error_rate\t{0}\n'.format(repr(eps)) + 'read_length\t{0}\n'.format(l))
    return sample, cov, g_len, eps, l


def estimate_stats(sequence, nth):
    sample = os.path.basename(sequence).rsplit('.f', 1)[0]

    (l, ml, tl, n_reads) = sequence_stat(sequence)
    if ml > seq_len_threshold:
        cov = "NA"
        g_len = tl
        eps = 0
        l = "NA"
    else:
       # Set to dummy values for reads to initialize dictionaries. 
       # Will be recomputed for each subsample.
        cov = 0.0
        g_len = tl
        eps = 0.0
        l = l
    return sample, cov, g_len, eps, l, n_reads

def create_sketch_dir(sequence, lib, ce, ge, ee, le,  nth):
    sample = os.path.basename(sequence).rsplit('.f', 1)[0]
    sample_dir = os.path.join(lib, sample)
    try:
        os.makedirs(sample_dir)
    except OSError as Error:
        if Error.errno != errno.EEXIST:
            raise
    info_file = os.path.join(sample_dir, sample + '.dat')

    cov = ce[sample]
    g_len = ge[sample]
    eps = ee[sample]
    l = le[sample]
    with open(info_file, mode='w') as f:
        f.write('coverage\t{0}\n'.format(cov) + 'genome_length\t{0}\n'.format(g_len) +
                'error_rate\t{0}\n'.format(eps) + 'read_length\t{0}\n'.format(l))
    return


def sketch(sequence, lib, ce, ee, k, s, cov_thres, seed, has_ref):
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
    if cov < cov_thres or eps == 0.0 or has_ref:
        call(["mash", "sketch", "-k", str(k), "-s", str(s), "-S", str(seed), "-r", "-o", msh, sequence], stderr=open(
            os.devnull, 'w'))
    else:
        call(["mash", "sketch", "-m", str(copy_thres), "-k", str(k), "-s", str(s), "-S", str(seed), "-o", msh,
              sequence], stderr=open(os.devnull, 'w'))
    return


def jacc2dist(j, k, gl1, gl2, len_penalty):
    if len_penalty:
        return 1 - (1.0 * (gl1 + gl2) * j / (1.0 * (gl1 + gl2) * (1 + j) / 2)) ** (1.0 / k)
    else:
        return 1 - (1.0 * (gl1 + gl2) * j / (1.0 * min(gl1, gl2) * (1 + j))) ** (1.0 / k)


def dist_temp_func(cov, eps, k, l, cov_thres):
    if cov == "NA":
        return [1.0, 0]
    p = np.exp(-k * eps)
    copy_thres = int(1.0 * cov / cov_thres) + 1
    lam = 1.0 * cov * (l - k) / l
    if copy_thres == 1 or p == 1:
        return [1 - np.exp(-lam * p), lam * (1 - p)]
    else:
        s = [(lam * p) ** i / np.math.factorial(i) for i in range(copy_thres)]
        return [1 - np.exp(-lam * p) * sum(s), 0]


def estimate_dist2(sample_1, sample_2, lib_1, lib_2, ce, le, ee, rl, k, cov_thres, tran):
    if sample_1 == sample_2 and lib_1 == lib_2:
        return sample_1, sample_2, 0.0
    sample_dir_1 = os.path.join(lib_1, sample_1)
    sample_dir_2 = os.path.join(lib_2, sample_2)
    msh_1 = os.path.join(sample_dir_1, sample_1 + ".msh")
    msh_2 = os.path.join(sample_dir_2, sample_2 + ".msh")
    dist_stderr = check_output(["mash", "dist", msh_1, msh_2], stderr=STDOUT, universal_newlines=True)
    j = float(dist_stderr.split()[4].split("/")[0]) / float(dist_stderr.split()[4].split("/")[1])
    gl_1 = le[sample_1]
    gl_2 = le[sample_2]
    if gl_1 == "NA" or gl_2 == "NA":
        gl_1 = 1
        gl_2 = 1
    cov_1 = ce[sample_1]
    cov_2 = ce[sample_2]
    eps_1 = ee[sample_1]
    eps_2 = ee[sample_2]
    l_1 = rl[sample_1]
    l_2 = rl[sample_2]
    r_1 = dist_temp_func(cov_1, eps_1, k, l_1, cov_thres)
    r_2 = dist_temp_func(cov_2, eps_2, k, l_2, cov_thres)
    wp = r_1[0] * r_2[0] * (gl_1 + gl_2) * 0.5
    zp = sum(r_1) * gl_1 + sum(r_2) * gl_2
    d = max(0, 1 - (1.0 * zp * j / (wp * (1 + j))) ** (1.0 / k))
    if tran:
        if d < 0.75:
            d = max(0, -0.75 * np.log(1 - 4.0 * d / 3.0))
        else:
            d = 5.0
    return sample_1, sample_2, d


def reference(args):
    # TODO: HOW TO CHECK IF LIBRARY HAS BEEN CONSTRUCTED FOR REFERENCE OR NOT.
    # Creating a directory for reference library
    try:
        os.makedirs(args.l)
    except OSError as Error:
        if Error.errno != errno.EEXIST:
            raise

    # Creating a config file for references
    config_file = os.path.join(args.l, 'CONFIG')
    with open(config_file, mode='w') as f:
        f.write('kmer_length\t{0}\n'.format(args.k) + 'sketch_size\t{0}\n'.format(args.s) +
                'sketching_seed\t{0}\n'.format(args.S))

    # Making a list of sample names
    formats = ['.fq', '.fastq', '.fa', '.fna', '.fasta']
    files_names = [f for f in os.listdir(args.input_dir)
                   if True in (fnmatch.fnmatch(f, '*' + form) for form in formats)]
    samples_names = [f.rsplit('.f', 1)[0] for f in files_names]
    if samples_names:
       print("Found these samples: ", samples_names)
    else:
        raise FileNotFoundError("no files with extensions %s found" % " ".join(formats))

    # Check if refs have duplicate entry
    if len(samples_names) != len(set(samples_names)):
        raise ValueError('Duplicate inputs (possibly same name with different extensions), please change '
                         'the file name(s) and try again')

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

    # Number of pools and threads for multi-processing
    n_pool = min(args.p, len(sequences))
    n_thread_cov = int(args.p / n_pool)
    n_proc_cov = n_pool * n_thread_cov
    n_pool_dist = min(args.p, len(sequences) ** 2)

    # Computing coverage, genome length, error rate, and read length
    sys.stderr.write('[skmer] Estimating coverages using {0} processors...\n'.format(n_proc_cov))
    pool_cov = mp.Pool(n_pool)
    #GETTING REF_HIST
    ref_hist= parse_reference(args.r, args.k, args.p, args.l) if args.r else None
    results_cov = [pool_cov.apply_async(estimate_cov, args=(seq, args.l, args.k, args.e, n_thread_cov,ref_hist))
                   for seq in sequences]
    
    for result in results_cov:
        (name, coverage, genome_length, error_rate, read_length) = result.get(9999999)
        cov_est[name] = coverage
        len_est[name] = genome_length
        err_est[name] = error_rate
        read_len[name] = read_length
    pool_cov.close()
    pool_cov.join()
    
    # Sketching genome-skims
    sys.stderr.write('[skmer] Sketching sequences using {0} processors...\n'.format(n_pool))
    pool_sketch = mp.Pool(n_pool)

    if args.r is not None:
        results_sketch = [pool_sketch.apply_async(sketch, args=(seq, args.l, cov_est, err_est, args.k, args.s,
                                                            coverage_threshold, args.S, True)) for seq in sequences]
    else:
        results_sketch = [pool_sketch.apply_async(sketch, args=(seq, args.l, cov_est, err_est, args.k, args.s,
                                                            coverage_threshold, args.S, False)) for seq in sequences]
    
    for result in results_sketch:
        result.get(9999999)
    pool_sketch.close()
    pool_sketch.join()

    # Estimating pair-wise distances
    sys.stderr.write('[skmer] Estimating distances using {0} processors...\n'.format(n_pool_dist))
    pool_dist = mp.Pool(n_pool_dist)
    if args.r is not None:
        results_dist = [pool_dist.apply_async(estimate_dist, args=(s1, s2, args.l, args.l, cov_est, len_est,
                                                               err_est, read_len, args.k, coverage_threshold, args.t, ref_hist))
                    for s1 in samples_names for s2 in samples_names]
    else:
        results_dist = [pool_dist.apply_async(estimate_dist2, args=(s1, s2, args.l, args.l, cov_est, len_est,
                                                               err_est, read_len, args.k, coverage_threshold, args.t))
                    for s1 in samples_names for s2 in samples_names]


    for result in results_dist:
        dist_output = result.get(9999999)
        result_df[(dist_output[0], dist_output[1])] = [repr(dist_output[2])]

    # Writing distances to file
    sys.stderr.write('[skmer] Writing to file...\n')
    result_dfm = pd.melt(result_df, value_name='distance')
    result_mat = result_dfm.pivot(index='sample', columns='sample_2', values='distance')
    result_mat.to_csv(args.o + ".txt", sep='\t', mode='w')


def subsample(args):

    # Creating a directory for subsample
    try:
        os.makedirs(args.sub)
    except OSError as Error:
        if Error.errno != errno.EEXIST:
            raise

    # Making a list of sample names
    formats = ['.fq', '.fastq', '.fa', '.fna', '.fasta']
    files_names = [f for f in os.listdir(args.input_dir)
                   if True in (fnmatch.fnmatch(f, '*' + form) for form in formats)]
    samples_names = [f.rsplit('.f', 1)[0] for f in files_names]

    # Check if refs have duplicate entry
    if len(samples_names) != len(set(samples_names)):
        raise ValueError('Duplicate inputs (possibly same name with different extensions), please change '
                         'the file name(s) and try again')

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
    bs_kmer_sum = dict()
    sample_read_cnt = dict()

    # Number of pools and threads for multi-processing
    n_pool = min(args.p, len(sequences))
    n_thread_cov = int(args.p / n_pool)
    n_proc_cov = n_pool * n_thread_cov
    n_pool_dist = min(args.p, len(sequences) ** 2)

    # Computing coverage, genome length, error rate, read length and k-mer count
    sys.stderr.write('[skmer] Starting subsampling using {0} processors...\n'.format(n_proc_cov))
    pool_cov = mp.Pool(n_pool)
    results_cov = [pool_cov.apply_async(estimate_stats, args=(seq, n_thread_cov))
                   for seq in sequences]
    for result in results_cov:
        (name, coverage, genome_length, error_rate, read_length, rd_cnt) = result.get(9999999)
        cov_est[name] = coverage
        len_est[name] = genome_length
        err_est[name] = error_rate
        read_len[name] = read_length
        sample_read_cnt[name] = rd_cnt
        bs_kmer_sum[name] = args.s
    pool_cov.close()
    pool_cov.join()
    #print(sample_read_cnt)


    # Check whether inputs are reads or assemblies
    if "NA" in list(read_len.values()):
        input_data = 'assemblies'
    else:
        input_data = 'reads'

    
    ### Choose procedure for reads or assemblies ###
    sys.stderr.write('[skmer] Input processed as {}...\n'.format(input_data))

    # Compute block size

    np.random.seed(args.S)
    rand_seed_list = list(np.random.randint(low = 0, high = 4294967294, size = args.b))
    #print(rand_seed_list)

    bs_block_sz = {}
    bs_sample_sz = {}
    coef = args.c
    asm_sketch_sz = 0

    if input_data == 'reads':
       bs_sample_sz = sample_read_cnt
       for key, value in sample_read_cnt.items():
           bs_block_sz [key] = round((value)**(coef))
    else:
        bs_sample_sz = bs_kmer_sum
        mean_bs_kmer_count = np.mean(list(bs_kmer_sum.values()))
        asm_sketch_sz = round((mean_bs_kmer_count)**(coef))
        for key, value in bs_kmer_sum.items():
           bs_block_sz[key] = asm_sketch_sz

    #print(bs_block_sz)
    #print(bs_sample_sz)



    # Computing replicates
    for b in range (0, args.b):

        sys.stderr.write('[skmer] Computing replicate {0} using {1} processors...\n'.format(b, n_pool))

        # Creating replicate directory
        sub_rep = os.path.join(args.sub, "rep" + str(args.i))
        args.i +=1
        try:
            os.makedirs(sub_rep)
        except OSError as Error:
            if Error.errno != errno.EEXIST:
                raise

        # Creating replicate/library directory
        sub_lib = os.path.join(sub_rep, 'library')
        try:
            os.makedirs(sub_lib)
        except OSError as Error:
            if Error.errno != errno.EEXIST:
                raise

        # Update paths for subsampled replicate
        bs_sequences = [os.path.join(sub_rep, os.path.split(seq)[-1]) for seq in sequences]


        # Creating a config file for subsample  replicate
        config_file = os.path.join(sub_rep, 'CONFIG')
        with open(config_file, mode='w') as f:
            f.write('kmer_length\t{0}\n'.format(args.k) + 'sketch_size\t{0}\n'.format(args.s) +
                'sketching_seed\t{0}\n'.format(rand_seed_list[b]))


        # Write sample size dictionary to file
        np.save(os.path.join(sub_rep, 'block_size.npy'), bs_block_sz)        
        np.save(os.path.join(sub_rep, 'sample_size.npy'), bs_sample_sz)

        # Update  coverage and error estimates for subsample
        if input_data == 'reads':


            # Generate subsample replicates and save to bootstrap directory
            pool_sketch = mp.Pool(n_pool)
            results_sketch = [pool_sketch.apply_async(sample_reads, args=(seq, rand_seed_list[b], bs_block_sz[(os.path.split(seq)[-1]).rsplit('.f', 1)[0]], sub_rep)) for seq in sequences]
            for result in results_sketch:
                result.get(9999999)
            pool_sketch.close()
            pool_sketch.join()


            # Computing coverage, genome length, error rate, and read length of replicates  using reference function
            pool_cov = mp.Pool(n_pool)
            results_cov = [pool_cov.apply_async(estimate_cov, args=(seq, sub_lib, args.k, args.e, n_thread_cov))
                       for seq in bs_sequences]
            for result in results_cov:
                (name, coverage, genome_length, error_rate, read_length) = result.get(9999999)
                cov_est[name] = coverage
                len_est[name] = genome_length
                err_est[name] = error_rate
                read_len[name] = read_length
            pool_cov.close()
            pool_cov.join()


            # Sketching genome-skims
            pool_sketch = mp.Pool(n_pool)
            #reads_sketch_sz = 100000
            results_sketch = [pool_sketch.apply_async(sketch, args=(seq, sub_lib, cov_est, err_est, args.k, args.s,
                                                            coverage_threshold, rand_seed_list[b])) for seq in bs_sequences]
            for result in results_sketch:
                result.get(9999999)
            pool_sketch.close()
            pool_sketch.join()


            # Estimating pair-wise distances
            pool_dist = mp.Pool(n_pool_dist)
            results_dist = [pool_dist.apply_async(estimate_dist, args=(s1, s2, sub_lib, sub_lib, cov_est, len_est, err_est,
                                                                   read_len, args.k, coverage_threshold, args.t))
                        for s1 in samples_names for s2 in samples_names]

            for result in results_dist:
                dist_output = result.get(9999999)
                result_df[(dist_output[0], dist_output[1])] = [repr(dist_output[2])]


        else:

            # Prepare genome-skims directory structure
            pool_sketch = mp.Pool(n_pool)
            results_sketch = [pool_sketch.apply_async(create_sketch_dir, args=(seq, sub_lib, cov_est, len_est, err_est, 
                                                                               read_len, args.t)) for seq in sequences]
            pool_sketch.close()
            pool_sketch.join()



            # Sketching genome-skims
            pool_sketch = mp.Pool(n_pool)
            results_sketch = [pool_sketch.apply_async(sketch, args=(seq, sub_lib, cov_est, err_est, args.k, asm_sketch_sz,
                                                            coverage_threshold, rand_seed_list[b])) for seq in sequences]
            for result in results_sketch:
                result.get(9999999)
            pool_sketch.close()
            pool_sketch.join()



            # Estimating pair-wise distances
            pool_dist = mp.Pool(n_pool_dist)
            results_dist = [pool_dist.apply_async(estimate_dist, args=(s1, s2, sub_lib, sub_lib, cov_est, len_est, err_est,
                                                                   read_len, args.k, coverage_threshold, args.t))
                        for s1 in samples_names for s2 in samples_names]

            for result in results_dist:
                dist_output = result.get(9999999)
                result_df[(dist_output[0], dist_output[1])] = [repr(dist_output[2])]



        # Writing distances to file
        sys.stderr.write('[skmer] Writing to file...\n')
        result_dfm = pd.melt(result_df, value_name='distance')
        result_mat = result_dfm.pivot(index='sample', columns='sample_2', values='distance')
        final_path = os.path.join(sub_rep, "dimtrx_rep" + ".txt")
        result_mat.to_csv(final_path, float_format='%f', sep='\t', mode='w')


        # Cleaning up 
        if args.fa:
                for fi in bs_sequences:
                    try:
                        os.remove(fi)
                    except OSError:
                        pass

        if args.msh:
            sketch_fi = []
            for (dirpath, dirnames, filenames) in os.walk(sub_lib):
                sketch_fi += [os.path.join(dirpath, file) for file in filenames if file.endswith(".msh") ]
            #print(sketch_fi)
            for fi in sketch_fi:
                try:
                    os.remove(fi)
                except OSError:
                    pass


    # Clean up subsample folders 
    #import shutil
    #shutil.rmtree(sub_lib)
    #shutil.rmtree(args.bs)
    #shutil.rmtree(args.l)


def correction(args):
     


    # Making a list of sample names
    try:
        #with open(args.main,"r") as f:
        df = pd.read_csv(args.main, header = 0, sep='\t', skiprows = 0)
        samples_names = list(df.iloc[:,0])
        #print(samples_names)
    except:
        raise ValueError('Please check file name for main distance matrix and try again')


    # Initializing distance dataframe
    index = pd.MultiIndex.from_product([samples_names, samples_names], names=['sample', 'sample_2'])
    result_df = pd.DataFrame(columns=index)
    combo_result_df = pd.DataFrame()


    # Round distances up to 12 digits since fastme doesn't except more than 12 decimals
    no_strap_dfm = pd.melt(df, id_vars=['sample'], value_vars= list(df.columns[1:]) )
    no_strap_dfm.rename(columns={'variable':'sample_2'}, inplace=True) 
    no_strap_dfm.rename(columns={'value':'no_strapped_dist'}, inplace=True)    

    decimals = 12
    no_strap_dfm['no_strapped_dist'] = no_strap_dfm['no_strapped_dist'].apply(pd.to_numeric, errors='coerce')
    no_strap_dfm['no_strapped_dist'] =  no_strap_dfm['no_strapped_dist'].apply(lambda x: round(x, decimals))

    no_strap_mat = no_strap_dfm.pivot(index='sample', columns='sample_2', values='no_strapped_dist')
    no_strap_mat.to_csv(os.path.splitext(args.main)[0] + "_cor_" +  ".txt", float_format='%f', sep='\t', mode='w')

    # List replicate directories
    try:
        for dir in [name for name in os.listdir(args.sub) if 'rep' in name]:
            print(dir)
            rep_mtrx = os.path.join(args.sub, dir, "dimtrx_rep.txt")
            df = pd.read_csv(rep_mtrx, header = 0, sep='\t', skiprows = 0)

            # Append estimates to combo dataframe
            result_dfm = pd.melt(df, id_vars=['sample'], value_vars= list(df.columns[1:]) )
            result_dfm.rename(columns={'variable':'sample_2'}, inplace=True)
            result_dfm.rename(columns={'value':'uncorrected_dist'}, inplace=True)
            result_dfm['rep'] = int(dir.split('rep', 1)[-1])

            # Load dictionaries
            bs_block_sz = np.load(os.path.join(args.sub, dir, 'block_size.npy'), allow_pickle='TRUE').item()
            bs_sample_sz = np.load(os.path.join(args.sub, dir, 'sample_size.npy'), allow_pickle='TRUE').item()
            
            result_dfm['b_s1'] = result_dfm['sample'].map(bs_block_sz)
            result_dfm['b_s2'] = result_dfm['sample_2'].map(bs_block_sz)
            result_dfm['b_s1'] = result_dfm['b_s1'].apply(pd.to_numeric, errors='coerce')
            result_dfm['b_s2'] = result_dfm['b_s2'].apply(pd.to_numeric, errors='coerce')
            result_dfm['b_mean'] = result_dfm[['b_s1', 'b_s2']].mean(axis=1, skipna=True)
            
            result_dfm['n_s1'] = result_dfm['sample'].map(bs_sample_sz)
            result_dfm['n_s2'] = result_dfm['sample_2'].map(bs_sample_sz)
            result_dfm['n_s1'] = result_dfm['n_s1'].apply(pd.to_numeric, errors='coerce')
            result_dfm['n_s2'] = result_dfm['n_s2'].apply(pd.to_numeric, errors='coerce')
            result_dfm['N_mean'] = result_dfm[['n_s1', 'n_s2']].mean(axis=1, skipna=True)
            combo_result_df = combo_result_df.append(result_dfm, ignore_index = True)
            #print(combo_result_df)

    except:
        raise ValueError('Please check subsample directory and try again')


   
    # Computing distance correction

    combo_result_df['uncorrected_dist'] = combo_result_df['uncorrected_dist'].apply(pd.to_numeric, errors='coerce')
    res = combo_result_df.groupby(['sample', 'sample_2'], as_index=False)['uncorrected_dist'].mean()
    res.rename({'uncorrected_dist': 'subsample_mean_dist'}, axis=1, inplace=True)    
    new_df = pd.merge(combo_result_df, res,  how='left', left_on=['sample','sample_2'], right_on = ['sample','sample_2'])

    new_df_out = pd.merge(new_df, no_strap_dfm,  how='left', left_on=['sample','sample_2'], right_on = ['sample','sample_2'])
    new_df_out['no_strapped_dist'] = new_df_out['no_strapped_dist'].apply(pd.to_numeric, errors='coerce')
    
    new_df_out['corrected_dist'] = ((new_df_out['b_mean']/new_df_out['N_mean'])**(1/2))*(new_df_out['uncorrected_dist']-new_df_out['subsample_mean_dist'])+new_df_out['no_strapped_dist']
    new_df_out['corrected_dist_cons'] = ((new_df_out['b_mean']/new_df_out['N_mean'])**(1/2))*(new_df_out['uncorrected_dist']-new_df_out['subsample_mean_dist'])+new_df_out['subsample_mean_dist']
    
    #replace negative values with 0.0 so fastme can handle matrices
    new_df_out.corrected_dist = np.where(new_df_out.corrected_dist < 0, 0.0, new_df_out.corrected_dist)
    new_df_out.corrected_dist_cons = np.where(new_df_out.corrected_dist_cons < 0, 0.0, new_df_out.corrected_dist_cons)

    # round distances up to 12 digits since fastme doesn't except more than 12 decimals
    new_df_out['corrected_dist'] =  new_df_out['corrected_dist'].apply(lambda x: round(x, decimals))
    new_df_out['corrected_dist_cons'] =  new_df_out['corrected_dist_cons'].apply(lambda x: round(x, decimals))
    new_df_out.to_csv(os.path.join(args.sub, "_summary" + ".csv"), float_format='%f', sep=',', mode='w')



    # Writing distances to file
    sys.stderr.write('[skmer] Writing to file...\n')
    
    b_list = list((new_df_out.loc[:, 'rep']).unique())
    #print(b_list)
    for b in b_list:
        sub_dfm = new_df_out.loc[(new_df_out['rep'] == b)]
        
        #-mean+main
        sub_dfm_main = sub_dfm[['sample','sample_2', 'corrected_dist']]
        result_mat_main = sub_dfm_main.pivot(index='sample', columns='sample_2', values='corrected_dist')
        result_mat_main.to_csv(os.path.join(args.sub, "rep" + str(b),  "dimtrx_rep_cor.txt"), float_format='%f', sep='\t', mode='w')
        
        #-mean+mean
        sub_dfm_cons = sub_dfm[['sample','sample_2', 'corrected_dist_cons']]
        result_mat_cons = sub_dfm_cons.pivot(index='sample', columns='sample_2', values='corrected_dist_cons')
        result_mat_cons.to_csv(os.path.join(args.sub, "rep" + str(b),  "dimtrx_rep_cor_cons.txt"), float_format='%f', sep='\t', mode='w')


def distance(args):
    # Loading reference config
    config_file = os.path.join(args.library, 'CONFIG')
    with open(config_file) as f:
        config = f.read()
    kl = int(config.split('\n')[0].split('\t')[1])

    # Making a list of reference samples
    refs = [item for item in os.listdir(args.library) if os.path.isdir(os.path.join(args.library, item))]

    # Initializing distance dataframe
    index = pd.MultiIndex.from_product([refs, refs], names=['sample', 'sample_2'])
    result_df = pd.DataFrame(columns=index)

    # Loading coverage, genome length, error rate, and read length information
    cov_est = dict()
    len_est = dict()
    err_est = dict()
    read_len = dict()
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
            else:
                cov_est[ref] = "NA"
                len_est[ref] = int(info.split('\n')[1].split('\t')[1])
                err_est[ref] = 0
                read_len[ref] = "NA"
        else:
            cov_est[ref] = float(info.split('\n')[0].split('\t')[1])
            len_est[ref] = int(info.split('\n')[1].split('\t')[1])
            err_est[ref] = float(info.split('\n')[2].split('\t')[1])
            read_len[ref] = int(info.split('\n')[3].split('\t')[1])

    # Number of pools and threads for multi-processing
    n_pool_dist = min(args.p, len(refs) ** 2)

    # Estimating pair-wise distances
    sys.stderr.write('[skmer] Estimating distances using {0} processors...\n'.format(n_pool_dist))
    pool_dist = mp.Pool(n_pool_dist)

    if args.r is not None:
        sys.stderr.write('[skmer] Parsing reference with {0} processors...\n'.format(n_pool_dist))
        ref_hist=parse_reference(args.r, kl, args.p, args.library)
        results_dist = [pool_dist.apply_async(estimate_dist, args=(r1, r2, args.library, args.library, cov_est, len_est,
                                                               err_est, read_len, kl, coverage_threshold, args.t, ref_hist))
                    for r1 in refs for r2 in refs]
    else:
        results_dist = [pool_dist.apply_async(estimate_dist2, args=(r1, r2, args.library, args.library, cov_est, len_est,
                                                               err_est, read_len, kl, coverage_threshold, args.t))
                    for r1 in refs for r2 in refs]


    for result in results_dist:
        dist_output = result.get(9999999)
        result_df[(dist_output[0], dist_output[1])] = [repr(dist_output[2])]

    # Writing distances to file
    sys.stderr.write('[skmer] Writing to file...\n')
    result_dfm = pd.melt(result_df, value_name='distance')
    result_mat = result_dfm.pivot(index='sample', columns='sample_2', values='distance')
    result_mat.to_csv(args.o + ".txt", sep='\t', mode='w')


def query(args):
    # Loading reference config
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
            else:
                cov_est[ref] = "NA"
                len_est[ref] = int(info.split('\n')[1].split('\t')[1])
                err_est[ref] = 0
                read_len[ref] = "NA"
        else:
            cov_est[ref] = float(info.split('\n')[0].split('\t')[1])
            len_est[ref] = int(info.split('\n')[1].split('\t')[1])
            err_est[ref] = float(info.split('\n')[2].split('\t')[1])
            read_len[ref] = int(info.split('\n')[3].split('\t')[1])

    # Number of pools for multi-processing
    n_pool_dist = min(args.p, len(refs))

    # Processing Reference Histogram
    ref_hist = parse_reference(args.r, kl, args.p, args.library) if args.r else None

    # Computing the coverage, genome length, error rate, and read length of query sample
    sys.stderr.write('[skmer] Estimating the coverage using {0} processors...\n'.format(args.p))
    #(dummy, coverage, genome_length, error_rate, read_length) = estimate_cov(args.input, os.getcwd(), kl, args.e,
    #                                                                         args.p)
    (dummy, coverage, genome_length, error_rate, read_length) = estimate_cov(args.input, os.getcwd(), kl, args.e, args.p, ref_hist)
    cov_est[sample] = coverage
    len_est[sample] = genome_length
    err_est[sample] = error_rate
    read_len[sample] = read_length

    # Sketching the query genome-skim
    sys.stderr.write('[skmer] Sketching the genome-skim...\n')
    if args.r is not None:
        sketch(args.input, os.getcwd(), cov_est, err_est, kl, ss, coverage_threshold, seed, True)
    else:
        sketch(args.input, os.getcwd(), cov_est, err_est, kl, ss, coverage_threshold, seed, False)

    # Estimating pair-wise distances
    sys.stderr.write('[skmer] Estimating distances using {0} processors...\n'.format(n_pool_dist))
    pool_dist = mp.Pool(n_pool_dist)
    if args.r is not None:
        results_dist = [pool_dist.apply_async(estimate_dist, args=(sample, ref, os.getcwd(), args.library, cov_est, len_est,
                                                               err_est, read_len, kl, coverage_threshold, args.t, ref_hist))
                    for ref in refs]
    else:
        results_dist = [pool_dist.apply_async(estimate_dist2, args=(sample, ref, os.getcwd(), args.library, cov_est, len_est,
                                                               err_est, read_len, kl, coverage_threshold, args.t))
                    for ref in refs]
        
    for result in results_dist:
        dist_output = result.get(9999999)
        result_s[dist_output[1]] = dist_output[2]

    # Writing distances to file
    sys.stderr.write('[skmer] Writing to file...\n')
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

def main():
    # Input arguments parser
    parser = argparse.ArgumentParser(description='{0} - Estimating genomic distances between '.format(__version__) +
                                                 'genome-skims',
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    # parser.add_argument('-v', '--version', action='store_true', help='print the current version')
    parser.add_argument('--debug', action='store_true', help='Print the traceback when an exception is raised')
    subparsers = parser.add_subparsers(title='commands',
                                       description='reference   Process a library of reference genome-skims or assemblies\n'
                                                   'distance    Compute pairwise distances for a processed library\n'
                                                   'query       Compare a genome-skim or assembly against a reference library\n'
                                                   'subsample   Performs  subsample on a library of reference genome-skims or assemblies\n'
                                                   'correct     Performs correction of subsampled distance matrices obtained for reference' 
                                                   ' genome-skims or assemblies'
                                                   ,
                                       help='Run skmer {commands} [-h] for additional help',
                                       dest='{commands}')

    # To make sure that subcommand is required in python >= 3.3
    python_version = sys.version_info
    if (python_version[0] * 10 + python_version[1]) >= 33:
        subparsers.required = True

    # Reference command subparser
    parser_ref = subparsers.add_parser('reference',
                                       description='Process a library of reference genome-skims or assemblies')
    parser_ref.add_argument('input_dir',
                            help='Directory of input genome-skims or assemblies (dir of .fastq/.fq/.fa/.fna/.fasta files)')
    parser_ref.add_argument('-l', default=os.path.join(os.getcwd(), 'library'),
                            help='Directory of output (reference) library. Default: working_directory/library')
    parser_ref.add_argument('-o', default='ref-dist-mat',
                            help='Output (distances) prefix. Default: ref-dist-mat')
    parser_ref.add_argument('-k', type=int, choices=list(range(1, 32)), default=31, help='K-mer length [1-31]. ' +
                                                                                         'Default: 31', metavar='K')
    parser_ref.add_argument('-s', type=int, default=10 ** 5, help='Sketch size. Default: 100000')
    parser_ref.add_argument('-S', type=int, default=42, help='Sketching random seed. Default: 42')
    parser_ref.add_argument('-e', type=float, help='Base error rate. By default, the error rate is automatically '
                                                   'estimated.')
    parser_ref.add_argument('-t', action='store_true',
                            help='Apply Jukes-Cantor transformation to distances. Output 5.0 if not applicable')
    parser_ref.add_argument('-p', type=int, choices=list(range(1, mp.cpu_count() + 1)), default=mp.cpu_count(),
                            help='Max number of processors to use [1-{0}]. '.format(mp.cpu_count()) +
                                 'Default for this machine: {0}'.format(mp.cpu_count()), metavar='P')
    parser_ref.add_argument('-r', help='Path to reference genome, histogram, or repeat spectra data')
    parser_ref.set_defaults(func=reference)

    # Subsample command subparser
    parser_bt = subparsers.add_parser('subsample',
                                       description='Performs subsample on a library of reference genome-skims or assemblies')
    parser_bt.add_argument('input_dir',
                            help='Directory of input genome-skims or assemblies (dir of .fastq/.fq/.fa/.fna/.fasta files)')
    #parser_bt.add_argument('-l', default=os.path.join(os.getcwd(), 'library'),
    #                        help='Directory of output (reference) library. Default: working_directory/library')
    parser_bt.add_argument('-sub', default=os.path.join(os.getcwd(), 'subsample'),
                            help='Directory of output for subsample replicates. Default: working_directory/subsample')
    parser_bt.add_argument('-fa', action='store_false',
                            help='Save subsampled genome-skims. Default: false')
    parser_bt.add_argument('-msh', action='store_false',
                            help='Save sketches. Default: false')
   # parser_bt.add_argument('-o', default='ref-dist-mat',
   #                         help='Output (distances) prefix. Default: ref-dist-mat')
    parser_bt.add_argument('-k', type=int, choices=list(range(1, 32)), default=31, help='K-mer length [1-31]. ' +
                                                                                         'Default: 31', metavar='K')
    parser_bt.add_argument('-s', type=int, default=10 ** 5, help='Sketch size. Default: 100000')
    parser_bt.add_argument('-S', type=int, default=42, help='Sketching random seed. Default: 42')
    parser_bt.add_argument('-i', type=int, default=0, help='Start index of subsampled replicate (eg 5 for dir rep5). Default: 0')
    parser_bt.add_argument('-b', type=int, default=100, help='Number of subsampled replicates. Default: 100')    
    parser_bt.add_argument('-c', type=float, default=0.9, help='Exponent value for subsampling. Default: 0.9')
    parser_bt.add_argument('-e', type=float, help='Base error rate. By default, the error rate is automatically '
                                                   'estimated.')
    parser_bt.add_argument('-t', action='store_true',
                            help='Apply Jukes-Cantor transformation to distances. Output 5.0 if not applicable')
    parser_bt.add_argument('-p', type=int, choices=list(range(1, mp.cpu_count() + 1)), default=mp.cpu_count(),
                            help='Max number of processors to use [1-{0}]. '.format(mp.cpu_count()) +
                                 'Default for this machine: {0}'.format(mp.cpu_count()), metavar='P')
    parser_bt.set_defaults(func=subsample)    


    # Correction command subparser
    parser_cor = subparsers.add_parser('correct',
                                       description='Performs correction of subsampled distance matrices obtained for reference genome-skims or assemblies')
    parser_cor.add_argument('-main',
                            help='Distance matrix of main estimate')
    parser_cor.add_argument('-sub', default=os.path.join(os.getcwd(), 'subsample'),
                            help='Directory of output for subsample replicates. Default: working_directory/subsample')
    parser_cor.add_argument('-p', type=int, choices=list(range(1, mp.cpu_count() + 1)), default=mp.cpu_count(),
                            help='Max number of processors to use [1-{0}]. '.format(mp.cpu_count()) +
                                 'Default for this machine: {0}'.format(mp.cpu_count()), metavar='P')
    parser_cor.set_defaults(func=correction)

    # Distance command subparser
    parser_dist = subparsers.add_parser('distance', description='Compute the distance matrix for a processed library')
    parser_dist.add_argument('library', help='Directory of the input (processed) library')
    parser_dist.add_argument('-o', default='ref-dist-mat',
                             help='Output (distances) prefix. Default: ref-dist-mat')
    parser_dist.add_argument('-t', action='store_true',
                             help='Apply Jukes-Cantor transformation to distances. Output 5.0 if not applicable')
    parser_dist.add_argument('-p', type=int, choices=list(range(1, mp.cpu_count() + 1)), default=mp.cpu_count(),
                             help='Max number of processors to use [1-{0}]. '.format(mp.cpu_count()) +
                                  'Default for this machine: {0}'.format(mp.cpu_count()), metavar='P')
    parser_dist.add_argument('-r', help='Path to reference genome, histogram, or repeat spectra data')
    parser_dist.set_defaults(func=distance)

    # query command subparser
    parser_qry = subparsers.add_parser('query',
                                       description='Compare an input genome-skim or assembly against a reference library')
    parser_qry.add_argument('input', help='Input (query) genome-skim or assembly (a .fastq/.fq/.fa/.fna/.fasta file)')
    parser_qry.add_argument('library', help='Directory of (reference) library')
    parser_qry.add_argument('-a', action='store_true',
                            help='Add the processed input (query) to the (reference) library')
    parser_qry.add_argument('-o', default='dist',
                            help='Output (distances) prefix. Default: dist')
    parser_qry.add_argument('-e', type=float, help='Base error rate. By default, the error rate is automatically '
                                                   'estimated.')
    parser_qry.add_argument('-t', action='store_true',
                            help='Apply Jukes-Cantor transformation to distances. Output 5.0 if not applicable')
    parser_qry.add_argument('-p', type=int, choices=list(range(1, mp.cpu_count() + 1)), default=mp.cpu_count(),
                            help='Max number of processors to use [1-{0}]. '.format(mp.cpu_count()) +
                                 'Default for this machine: {0}'.format(mp.cpu_count()), metavar='P')
    parser_qry.set_defaults(func=query)

    # fst command subparser
    parser_fst = subparsers.add_parser('fst',
                                       description='Given an annotation file and a reference distance matrix, it will output the fst matrices of different populations.')
    parser_fst.add_argument('matrix', help='Path to distance matrix')
    parser_fst.add_argument('annotation', help='Path to annotation file. A TSV file with the 1st column being the file name and the 2nd the population name')
    parser_fst.add_argument('-o', help='Path to output matrices.')
    parser_fst.set_defaults(func=fst)

    args = parser.parse_args()

    # Handling traceback on exceptions
    def exception_handler(exception_type, exception, traceback, debug_hook=sys.excepthook):
        if args.debug:
            debug_hook(exception_type, exception, traceback)
        else:
            print("{0}: {1}".format(exception_type.__name__, exception))

    sys.excepthook = exception_handler

    args.func(args)


if __name__ == "__main__":
    main()

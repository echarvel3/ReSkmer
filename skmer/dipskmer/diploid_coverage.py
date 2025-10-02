import os
import errno
import math
import sys
import numpy as np
from scipy.optimize import minimize

from subprocess import run

from skmer.utils import write_error_file, count_kmers, sequence_stat
from skmer.config import *

def estimate_diploid_cov(sequence, lib, k, e, nth, theta_arg = None):
    sample = os.path.basename(sequence).rsplit('.f', 1)[0]
    sample_dir = os.path.join(lib, sample)
    try:
        os.makedirs(sample_dir)
    except OSError as Error:
        if Error.errno != errno.EEXIST:
            raise
    info_file = os.path.join(sample_dir, sample + '.dat')

    (l, max_len, tot_len, n_reads) = sequence_stat(sequence)
    if max_len > seq_len_threshold:
        raise TypeError("[ERROR] Assemblies not appropriate for diploid equations.")

    histo_stderr = count_kmers(sample_dir, sample, sequence, k, nth)

    count = [0]
    ksum = 0
    for item in histo_stderr.split('\n')[:-1]:
        count.append(int(item.split()[1]))
        ksum += int(item.split()[0]) * int(item.split()[1])
    if len(count) < 3:
        # too little coverage
        sys.stderr.write('[WARNING] Coverage of {0} is too low, not able to estimate it; no correction applied\n'.format(sample))
        cov = "NA"
        g_len = "NA"
        eps = "NA"
        theta = "NA"
        write_error_file(info_file, cov, g_len, eps, l, theta)
        return sample, cov, g_len, eps, l, theta

    ind = min(count.index(max(count[2:])), len(count) - 2)
    if (ind < 2):
        sys.stderr.write('[WARNING] Not enough information to co-estimate coverage, theta, and error rate of {0}; '.format(sample) +
                         'Using default error rate {0}\n'.format(default_error_rate))
        eps = default_error_rate
        p0 = np.exp(-k * eps)
        r21 = 1.0 * count[2] / count[1]
        cov = newton(cov_temp_func, 0.05, args=(r21, p0, k, l))
        theta = default_theta if (theta_arg is None) else theta_arg
    else:
        r =  count[ind + 1] / count[ind]
        rn =  count[ind + 2] / count[ind+1]
        if (8 * (2 + ind) * rn > 9 * (1 + ind) * r) or (theta_arg is not None):
            sys.stderr.write('[WARNING] Not enough information to co-estimate coverage, theta, and error rate of {0}; '.format(sample) +
                         'Using default theta but computing other values {0}\n'.format(default_theta))
            theta = default_theta if (theta_arg is None) else theta_arg
            Q = (1-theta)**k
            xi = minimize(lambda x: (r+x*(-2* np.exp(x)+2* (-2**ind+np.exp(x)) * Q)/((1+ind)* (2* np.exp(x)+(2**ind-2* np.exp(x))*Q )))**2, 0.5, bounds = [[0,100]]).x[0]
        else:
            xi = 1./4 * (3 * (1 + ind) * r - math.sqrt((1 + ind) * r) *math.sqrt(max(0, -8 * (2 + ind) * rn + 9 * (1 + ind) * r)))
            theta = 1 - 2**(1/ k) * ((np.exp(xi) * (-r - ind* r + xi))/( 2**ind *r - 2 * np.exp(xi) * r + 2**ind * ind * r - 2* np.exp(xi) * ind* r - 2**(1 + ind) * xi + 2 * np.exp(xi)* xi))**(1/k) 
            Q = (1-theta)**k
        # sets error rate
        if e is not None:
            eps = e
            lam = xi / (1-eps)**k
        else:
           lam = count[1]/count[ind] * ( (2 *np.exp(-xi) + (2**ind * np.exp(-2*xi) - 2 * np.exp(-xi))* Q) * (xi**ind) )/(2 * math.factorial(ind) ) + (-1 + np.exp(xi)) * (np.exp(-xi) + np.exp(-2*xi) * Q) * xi
           eps = 1-(xi/lam)**(1/k)
        cov = (1.0 * l / (l - k)) * lam
        #print("Error calc:",i, r, xi, lam, Q, theta, cov, eps)

    tot_seq = 1.0 * ksum 
    g_len = int(tot_seq / 2.0 / lam)
    #print("ksum,",ksum,g_len,lam, l / (l - k))

    if eps > error_rate_threshold or eps < 0:
        cov = "NA"
        g_len = "NA"
        eps = "NA"
        theta = "NA"
        write_error_file(info_file, cov, g_len, eps, l, theta)

        return sample, cov, g_len, eps, l, theta

    write_error_file(info_file, cov, g_len, eps, l, theta)
    return sample, cov, g_len, eps, l, theta

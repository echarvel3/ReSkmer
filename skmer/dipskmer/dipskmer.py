
def dip_dist_temp_func(cov, eps, k, l, cov_thres, theta):
    if cov == "NA":
        return [1.0, 0]
    p = np.exp(-k * eps)
    copy_thres = int(1.0 * cov / (1.0* cov_thres)) + 1
    lam = 1.0 * cov * (l - k) / l
    print(cov,eps,cov_thres)
    if copy_thres == 1 or p == 1:
        return [1 - np.exp(-lam * p), lam * (1 - p)]
    else:
        print("cov thresh: ", cov_thres, copy_thres, cov, cov/cov_thres, p)
        # TODO: change 2 lam to to lam / (2-(1-theta)**k)
        #s = [(2*lam / (2-(1-0.003)**31) * p) ** i / np.math.factorial(i) for i in range(copy_thres)]
        #return [1 - np.exp(-2*(lam / (2-(1-0.003)**31)) * p) * sum(s), 0]
        #NOTE: change 31 to k?
        #s = [(2*p*lam/(1+theta)) ** i / np.math.factorial(i) for i in range(copy_thres)]
        s = [np.exp(-lam*(1-eps)**k) * math.pow(lam*(1-eps)**k, i)/math.factorial(i) for i in range(copy_thres)] 
        print(1-sum(s))
        return [1 - sum(s), 0]


def estimate_dipskmer_dist(sample_1, sample_2, lib_1, lib_2, ce, le, ee, rl, k, cov_thres, tran, theta):
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
    cov_1 = ce[sample_1] if ce[sample_1] != "NA" else 1.0
    cov_2 = ce[sample_2] if ce[sample_2] != "NA" else 1.0

    eps_1 = ee[sample_1] if ee[sample_1] != "NA" else default_error_rate
    eps_2 = ee[sample_2] if ee[sample_2] != "NA" else default_error_rate

    l_1 = rl[sample_1]
    l_2 = rl[sample_2]
    theta_1 = theta[sample_1]
    theta_2 = theta[sample_2]

    #print("pars used:", eps_1,eps_2,cov_1,cov_2)
    r_1 = dip_dist_temp_func(cov_1, eps_1, k, l_1, cov_thres, theta_1)
    r_2 = dip_dist_temp_func(cov_2, eps_2, k, l_2, cov_thres, theta_2)
    
    hist_1, size_1, usize_1 = get_hist_data(lib_1, sample_1)
    hist_2, size_2, usize_2 = get_hist_data(lib_2, sample_2)
    
    i = j * (usize_1 + usize_2) / (1.0 + j)
    EI = i / gl_1
    #print(usize_1, usize_2)
    #numerator = (11*EI) + (4*r_1[0]*r_2[0] * ( r_1[0] + r_2[0] - 5))
    #denominator = r_1[0]*r_2[0] * (11*r_1[0]*r_2[0] - 18*(r_1[0]+r_2[0]) + 24)
    lam_1 = 1.0 * cov_1 * (l_1 - k) / l_1
    lam_2 = 1.0 * cov_2 * (l_2 - k) / l_2
    #psi_1 = 2*lam_1/2*(1-np.power(1-eps_1, k))
    #psi_2 = 2*lam_2/2*(1-np.power(1-eps_2, k))
    psi_1 = r_1[1]
    psi_2 = r_2[1]
    eta1= r_1[0]
    eta2= r_2[0]
    #print("psi1", psi_1, "psi_2", psi_2, "j", j)
    t = int(1.0 * cov_1 / (1.0* cov_thres)) + 1
    xi_1 = lam_1 * np.exp(-k*eps_1)
    xi_2 = lam_2 * np.exp(-k*eps_2)
    
    if t > 1:
        print("printing exact solution, since coverage is high:", cov_1, cov_2)
        nt = lambda x, y : 1 - poisson.cdf(t-1, x*xi_1 + y*xi_2)
        numerator = 11*(j*nt(2,2) - nt(0,2)*nt(2,0))
        denom_1 = j*(4*nt(0,1) + nt(0,2) + 4*nt(1,0) + 4*nt(1,1) + 4*nt(1,2) + nt(2,0) + 4*nt(2,1) - 11*nt(2,2))
        denom_2 = -4*nt(0,1) * (nt(1,0) + nt(2,0)) + nt(0,2)*(11*nt(2,0) - 4*nt(1,0))
    
        Q = 1 + numerator / (denom_1 + denom_2)
        d = 1 - np.power(Q, (6/11 * 1/k))
    else:
        numerator = j * ( -5*(eta1**2 +eta2**2) + 22*(eta1+ eta2+ psi_1 + psi_2) ) + 4 * (1+j) * eta2*eta1 *( eta1 + eta2 - 5 )
        power = (6/11 * 1/k)
        denominator = eta1*eta2*(11*eta2*eta1 +24 -18*eta2 -18*eta1)*(1 + j) + 6*j*(eta2**2 + eta1**2)
        d = 1 - np.power(numerator/denominator, (6/11 * 1/k))

    if tran or math.isnan(d):
        if d < 0.75:
            d = max(0, -0.75 * np.log(1 - 4.0 * d / 3.0))
        else:
            d = 'nan'
    d = 0.0 if float(d) < 0.0 else round(float(d), 6)
    print("distance:", sample_1, sample_2, ":", str(d))
    return sample_1, sample_2, d

def estimate_dipskmer_dist_approx(sample_1, sample_2, lib_1, lib_2, ce, le, ee, rl, k, cov_thres, tran, theta):
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
    theta_1 = theta[sample_1]
    theta_2 = theta[sample_2]

    #print("pars used:", eps_1,eps_2,cov_1,cov_2)
    r_1 = dip_dist_temp_func(cov_1, eps_1, k, l_1, cov_thres, theta_1)
    r_2 = dip_dist_temp_func(cov_2, eps_2, k, l_2, cov_thres, theta_2)
    
    hist_1, size_1, usize_1 = get_hist_data(lib_1, sample_1)
    hist_2, size_2, usize_2 = get_hist_data(lib_2, sample_2)
    
    i = j * (usize_1 + usize_2) / (1.0 + j)
    EI = i / gl_1
    #print(usize_1, usize_2)
    #numerator = (11*EI) + (4*r_1[0]*r_2[0] * ( r_1[0] + r_2[0] - 5))
    #denominator = r_1[0]*r_2[0] * (11*r_1[0]*r_2[0] - 18*(r_1[0]+r_2[0]) + 24)
    power = (6/11 * 1/k)
    lam_1 = 1.0 * cov_1 * (l_1 - k) / l_1
    lam_2 = 1.0 * cov_2 * (l_2 - k) / l_2
    #psi_1 = 2*lam_1/2*(1-np.power(1-eps_1, k))
    #psi_2 = 2*lam_2/2*(1-np.power(1-eps_2, k))
    psi_1 = r_1[1]
    psi_2 = r_2[1]
    eta1= r_1[0]
    eta2= r_2[0]
    #print("psi1", psi_1, "psi_2", psi_2, "j", j)

    numerator = j * ( -5*(eta1**2 +eta2**2) + 22*(eta1+ eta2+ psi_1 + psi_2) ) + 4 * (1+j) * eta2*eta1 *( eta1 + eta2 - 5 )
    #4*eta1*eta2*(eta1 + eta2 - 5) + j *((4*eta1-5)*(eta2**2) + 4*(eta1-5)*eta1*eta2+(22-5*eta1)*eta1 + 11*(2*eta2 + psi_1 + psi_2))
    denominator = eta1*eta2*(11*eta2*eta1 +24 -18*eta2 -18*eta1)*(1 + j) + 6*j*(eta2**2 + eta1**2)
    #(r_2[0]**2)*(11*(r_1[0]**2)*(j+1) - (18*r_1[0]*(j+1)) + 6*j) + 6*(r_1[0]**2)*j - 6*(3*r_1[0]-4)*r_1[0]*r_2[0]*(j+1)
    #print("eta1", r_1[0], "eta2", r_2[0], "int per pos",  EI, "int", i, "num", numerator, "denom", denominator, "pow", power)
    d = 1 - np.power(numerator/denominator,power)
    
    if tran or math.isnan(d):
        if d < 0.75:
            d = max(0, -0.75 * np.log(1 - 4.0 * d / 3.0))
        else:
            d = 'nan'
    print(d)
    return sample_1, sample_2, d


def estimate_diploid_cov(sequence, lib, k, e, nth, theta_arg = None):
    sample = os.path.basename(sequence).rsplit('.f', 1)[0]
    sample_dir = os.path.join(lib, sample)
    try:
        os.makedirs(sample_dir)
    except OSError as Error:
        if Error.errno != errno.EEXIST:
            raise
    info_file = os.path.join(sample_dir, sample + '.dat')

    (l, ml, tl, n_reads) = sequence_stat(sequence)
    if ml > seq_len_threshold:
        raise TypeError("Assemblies not appropriate for DipSkmer diploid equations.")
        # cov = "NA"
        # g_len = tl
        # eps = 0
        # l = "NA"
        # with open(info_file, mode='w') as f:
        #     f.write('coverage\t{0}\n'.format(cov) + 'genome_length\t{0}\n'.format(g_len) +
        #             'error_rate\t{0}\n'.format(eps) + 'read_length\t{0}\n'.format(l))
        # return sample, cov, g_len, eps, l

    histo_stderr = check_jellyfish_files(sample_dir, sample, sequence, k, nth)

    count = [0]
    ksum = 0
    for item in histo_stderr.split('\n')[:-1]:
        count.append(int(item.split()[1]))
        ksum += int(item.split()[0]) * int(item.split()[1])
    if len(count) < 3:
        # too little coverage
        sys.stderr.write('Coverage of {0} is too low, not able to estimate it; no correction applied\n'.format(sample))
        cov = "NA"
        g_len = "NA"
        eps = "NA"
        theta = "NA"
        with open(info_file, mode='w') as f:
            f.write('coverage\t{0}\n'.format(cov) + 'genome_length\t{0}\n'.format(g_len) +
                    'error_rate\t{0}\n'.format(eps) + 'read_length\t{0}\n'.format(l) + 'theta\t{0}\n'.format(theta))
        return sample, cov, g_len, eps, l, theta

    ind = min(count.index(max(count[2:])), len(count) - 2)
    if (ind < 2):
        sys.stderr.write('Not enough information to co-estimate coverage, theta, and error rate of {0}; '.format(sample) +
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
            sys.stderr.write('Not enough information to co-estimate coverage, theta, and error rate of {0}; '.format(sample) +
                         'Using default theta but computing other values {0}\n'.format(default_theta))
            theta = default_theta if (theta_arg is None) else theta_arg
            Q = (1-theta)**k
            xi = minimize(lambda x: (r+x*(-2* np.exp(x)+2* (-2**ind+np.exp(x)) * Q)/((1+ind)* (2* np.exp(x)+(2**ind-2* np.exp(x))*Q )))**2, 0.5, bounds = [[0,100]]).x[0]
        else:
            xi = 1./4 * (3 * (1 + ind) * r - math.sqrt((1 + ind) * r) *math.sqrt(max(0, -8 * (2 + ind) * rn + 9 * (1 + ind) * r)))
            theta = 1 - 2**(1/ k) * ((np.exp(xi) * (-r - ind* r + xi))/( 2**ind *r - 2 * np.exp(xi) * r + 2**ind * ind * r - 2* np.exp(xi) * ind* r - 2**(1 + ind) * xi + 2 * np.exp(xi)* xi))**(1/k) 
            Q = (1-theta)**k

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
        with open(info_file, mode='w') as f:
            f.write('coverage\t{0}\n'.format(cov) + 'genome_length\t{0}\n'.format(g_len) +
                    'error_rate\t{0}\n'.format(eps) + 'read_length\t{0}\n'.format(l) + 'theta\t{0}\n'.format(repr(theta)))
        return sample, cov, g_len, eps, l, theta

    with open(info_file, mode='w') as f:
        f.write('coverage\t{0}\n'.format(repr(cov)) + 'genome_length\t{0}\n'.format(g_len) +
                'error_rate\t{0}\n'.format(repr(eps)) + 'read_length\t{0}\n'.format(l) + 'theta\t{0}\n'.format(repr(theta)))
    return sample, cov, g_len, eps, l, theta

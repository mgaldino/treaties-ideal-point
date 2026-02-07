import numpy as np
import sys
import os
import csv

# Ensure we can import local modules
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from dynIRT_KD import dynIRT_KD

DATA_DIR = "data/processed/python_validation"

def read_csv_numpy(filepath, dtype=float, fill_value=0):
    try:
        # Check if file exists
        if not os.path.exists(filepath):
            raise FileNotFoundError(f"{filepath} not found")

        # Detect header (non-numeric first row)
        with open(filepath, 'r') as f:
            first_line = f.readline().strip()
        has_header = False
        if first_line:
            tokens = [t.strip().strip('"') for t in first_line.split(',')]
            for tok in tokens:
                if tok == "":
                    continue
                try:
                    float(tok)
                except ValueError:
                    has_header = True
                    break

        skip = 1 if has_header else 0
        data = np.genfromtxt(filepath, delimiter=',', skip_header=skip, filling_values=fill_value)
        
        # If 1D (single column or single row), ensure correct shape?
        # genfromtxt returns 1D array if 1 col.
        # R write.csv might produce row names if not carefully disabled, but the script had row.names=FALSE.
        
        return data.astype(dtype)
    except Exception as e:
        print(f"Error reading {filepath}: {e}")
        raise

def load_data():
    print("Loading validation data...")
    
    # rc: N x J (with NAs potentially, but fillna(0) was used in pandas version)
    # The R script: rc_num[is.na] <- m. But missing items are 0 in flow$rc?
    # R script: write.csv(flow$rc, ...). flow$rc is integer matrix with 0=missing.
    # So we just read it.
    rc = read_csv_numpy(os.path.join(DATA_DIR, "rc.csv"), dtype=int)
    
    startlegis = read_csv_numpy(os.path.join(DATA_DIR, "startlegis.csv"), dtype=int).flatten()
    endlegis = read_csv_numpy(os.path.join(DATA_DIR, "endlegis.csv"), dtype=int).flatten()
    bill_session = read_csv_numpy(os.path.join(DATA_DIR, "bill_session.csv"), dtype=int).flatten()
    
    # Metadata
    # N, J, T_periods, K
    meta_path = os.path.join(DATA_DIR, "metadata.csv")
    with open(meta_path, 'r') as f:
        reader = csv.reader(f)
        header = next(reader)
        row = next(reader)
        meta = {h: v for h, v in zip(header, row)}
        
    N = int(meta['N'])
    J = int(meta['J'])
    if 'T_periods' in meta:
        T_periods = int(meta['T_periods'])
    else:
        T_periods = int(meta['T'])
    K = int(meta['K'])
    
    # Priors
    x_mu0 = read_csv_numpy(os.path.join(DATA_DIR, "x_mu0.csv"))
    x_Sigma0 = read_csv_numpy(os.path.join(DATA_DIR, "x_Sigma0_diag.csv"))
    beta_mu = read_csv_numpy(os.path.join(DATA_DIR, "beta_mu.csv")).flatten()
    beta_sigma = read_csv_numpy(os.path.join(DATA_DIR, "beta_sigma.csv"))
    omega = read_csv_numpy(os.path.join(DATA_DIR, "omega.csv"))
    
    # Starts
    alpha_start = read_csv_numpy(os.path.join(DATA_DIR, "alpha_start.csv")).flatten()
    beta_start = read_csv_numpy(os.path.join(DATA_DIR, "beta_start.csv"))
    
    x_start = np.zeros((N, K, T_periods))
    for t in range(T_periods):
        fname = os.path.join(DATA_DIR, f"x_start_t{t+1}.csv")
        data_t = read_csv_numpy(fname)
        x_start[:, :, t] = data_t
        
    # Reshape safeguards
    if x_mu0.ndim == 1: x_mu0 = x_mu0.reshape(N, K)
    if x_Sigma0.ndim == 1: x_Sigma0 = x_Sigma0.reshape(N, K)
    if beta_sigma.ndim == 1: beta_sigma = beta_sigma.reshape(K+1, K+1)
    if omega.ndim == 1: omega = omega.reshape(K, K)
    if beta_start.ndim == 1: beta_start = beta_start.reshape(J, K)
    
    return {
        'data': {
            'rc': rc,
            'startlegis': startlegis,
            'endlegis': endlegis,
            'bill_session': bill_session,
            'T': T_periods
        },
        'starts': {
            'alpha': alpha_start,
            'beta': beta_start,
            'x': x_start
        },
        'priors': {
            'x_mu0': x_mu0,
            'x_Sigma0': x_Sigma0,
            'beta_mu': beta_mu,
            'beta_sigma': beta_sigma,
            'omega': omega
        },
        'control': {
            'verbose': False,
            'thresh': 1e-10,
            'maxit': 50,
            'checkfreq': 50,
            'estimate_omega': False,
            'diagonal_omega': False
        },
        'K': K
    }

def load_ref_results(T_periods, N, K):
    ref_alpha = read_csv_numpy(os.path.join(DATA_DIR, "ref_alpha.csv")).flatten()
    ref_beta = read_csv_numpy(os.path.join(DATA_DIR, "ref_beta.csv"))
    
    ref_x = np.zeros((N, K, T_periods))
    for t in range(T_periods):
        fname = os.path.join(DATA_DIR, f"ref_x_t{t+1}.csv")
        ref_x[:, :, t] = read_csv_numpy(fname)
        
    ref_loglik = read_csv_numpy(os.path.join(DATA_DIR, "ref_loglik_trace.csv")).flatten()
    
    if ref_beta.ndim == 1: ref_beta = ref_beta.reshape(-1, K)
    
    return ref_alpha, ref_beta, ref_x, ref_loglik

def main():
    try:
        inputs = load_data()
    except Exception as e:
        print(f"Failed to load data: {e}")
        return

    print("")
    print("Running Python dynIRT_KD...")
    res = dynIRT_KD(
        inputs['data'],
        inputs['starts'],
        inputs['priors'],
        inputs['control'],
        K=inputs['K']
    )
    
    print("")
    print("Loading Reference Results...")
    try:
        N, J = inputs['data']['rc'].shape
        T = inputs['data']['T']
        K = inputs['K']
        ref_alpha, ref_beta, ref_x, ref_loglik = load_ref_results(T, N, K)
    except Exception as e:
        print(f"Failed to load reference results: {e}")
        return
    
    # Compare
    py_alpha = res['means']['alpha']
    py_beta = res['means']['beta']
    py_x = res['means']['x']
    py_loglik = np.array(res['runtime']['loglik_trace'])
    
    diff_alpha = np.max(np.abs(py_alpha - ref_alpha))
    diff_beta = np.max(np.abs(py_beta - ref_beta))
    diff_x = np.max(np.abs(py_x - ref_x))
    
    # Loglik
    n_iter = min(len(py_loglik), len(ref_loglik))
    diff_loglik = np.max(np.abs(py_loglik[:n_iter] - ref_loglik[:n_iter]))
    
    print("")
    print("="*40)
    print("VALIDATION RESULTS")
    print("="*40)
    print(f"{'Parameter':<15} | {'Max Diff':<15} | {'Threshold':<15} | {'Status'}")
    print("-" * 60)
    
    def status(diff, tol=1e-8):
        return "PASS" if diff < tol else "FAIL"
        
    print(f"{'alpha':<15} | {diff_alpha:<15.2e} | {1e-8:<15} | {status(diff_alpha)}")
    print(f"{'beta':<15} | {diff_beta:<15.2e} | {1e-8:<15} | {status(diff_beta)}")
    print(f"{'x (ideal pts)':<15} | {diff_x:<15.2e} | {1e-8:<15} | {status(diff_x)}")
    print(f"{'loglik':<15} | {diff_loglik:<15.2e} | {1e-6:<15} | {status(diff_loglik, 1e-6)}")
    
    if diff_alpha < 1e-8 and diff_beta < 1e-8 and diff_x < 1e-8:
        print("")
        print("OVERALL: PASS")
    else:
        print("")
        print("OVERALL: FAIL")

if __name__ == "__main__":
    main()

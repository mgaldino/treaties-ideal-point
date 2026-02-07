import os
import sys
import csv
import numpy as np

# Ensure local imports work when run as a script
sys.path.append(os.path.dirname(__file__))

from dynIRT_KD import dynIRT_KD


def read_matrix(path, dtype=float):
    return np.loadtxt(path, delimiter=",", dtype=dtype)


def read_vector(path, dtype=float):
    return np.atleast_1d(np.loadtxt(path, delimiter=",", dtype=dtype))


def load_x_by_t(prefix, T):
    mats = []
    for t in range(1, T + 1):
        mats.append(read_matrix(f"{prefix}_t{t}.csv", dtype=float))
    return np.stack(mats, axis=2)


def main():
    base_dir = os.path.join("data", "processed", "python_validation")
    if not os.path.isdir(base_dir):
        print(f"Missing directory: {base_dir}")
        sys.exit(1)

    with open(os.path.join(base_dir, "metadata.csv"), newline="") as f:
        reader = csv.DictReader(f)
        row = next(reader)
        N = int(row["N"])
        J = int(row["J"])
        T = int(row["T"])
        K = int(row["K"])

    rc = read_matrix(os.path.join(base_dir, "rc.csv"), dtype=int)
    startlegis = read_vector(os.path.join(base_dir, "startlegis.csv"), dtype=int)
    endlegis = read_vector(os.path.join(base_dir, "endlegis.csv"), dtype=int)
    bill_session = read_vector(os.path.join(base_dir, "bill_session.csv"), dtype=int)

    x_mu0 = read_matrix(os.path.join(base_dir, "x_mu0.csv"), dtype=float)
    x_Sigma0_diag = read_matrix(os.path.join(base_dir, "x_Sigma0_diag.csv"), dtype=float)

    alpha_start = read_vector(os.path.join(base_dir, "alpha_start.csv"), dtype=float)
    beta_start = read_matrix(os.path.join(base_dir, "beta_start.csv"), dtype=float)
    x_start = load_x_by_t(os.path.join(base_dir, "x_start"), T)

    beta_mu = read_vector(os.path.join(base_dir, "beta_mu.csv"), dtype=float)
    beta_sigma = read_matrix(os.path.join(base_dir, "beta_sigma.csv"), dtype=float)
    omega = read_matrix(os.path.join(base_dir, "omega.csv"), dtype=float)

    ref_alpha = read_vector(os.path.join(base_dir, "ref_alpha.csv"), dtype=float)
    ref_beta = read_matrix(os.path.join(base_dir, "ref_beta.csv"), dtype=float)
    ref_x = load_x_by_t(os.path.join(base_dir, "ref_x"), T)
    ref_loglik = read_vector(os.path.join(base_dir, "ref_loglik_trace.csv"), dtype=float)

    data = {
        "rc": rc,
        "startlegis": startlegis,
        "endlegis": endlegis,
        "bill_session": bill_session,
        "T": T,
    }
    starts = {"alpha": alpha_start, "beta": beta_start, "x": x_start}
    priors = {
        "x_mu0": x_mu0,
        "x_Sigma0": x_Sigma0_diag,
        "beta_mu": beta_mu,
        "beta_sigma": beta_sigma,
        "omega": omega,
    }
    control = {
        "verbose": False,
        "thresh": 1e-10,
        "maxit": 50,
        "checkfreq": 50,
        "estimate_omega": False,
        "diagonal_omega": False,
        "thresh_loglik": None,
        "thresh_aitken": None,
        "loglik_patience": 3,
        "ncores": 1,
    }

    res = dynIRT_KD(data, starts, priors, control, K=K)

    alpha_py = res["means"]["alpha"]
    beta_py = res["means"]["beta"]
    x_py = res["means"]["x"]
    loglik_py = res["runtime"]["loglik_trace"]

    max_alpha_diff = float(np.max(np.abs(alpha_py - ref_alpha)))
    max_beta_diff = float(np.max(np.abs(beta_py - ref_beta)))
    max_x_diff = float(np.max(np.abs(x_py - ref_x)))

    if loglik_py.shape[0] != ref_loglik.shape[0]:
        print(
            f"Loglik length mismatch: R={ref_loglik.shape[0]} Python={loglik_py.shape[0]}"
        )
    iters = min(loglik_py.shape[0], ref_loglik.shape[0])

    loglik_diff = np.abs(loglik_py[:iters] - ref_loglik[:iters])
    max_loglik_diff = float(np.max(loglik_diff)) if iters > 0 else float("nan")

    print("iter, loglik_R, loglik_py, diff")
    for i in range(iters):
        print(
            f"{i+1:3d}, {ref_loglik[i]:.10f}, {loglik_py[i]:.10f}, {loglik_diff[i]:.10e}"
        )

    passed = (
        max_alpha_diff < 1e-8
        and max_beta_diff < 1e-8
        and max_x_diff < 1e-8
        and max_loglik_diff < 1e-6
    )

    print("\nSummary")
    print(f"max|alpha_diff| = {max_alpha_diff:.10e}")
    print(f"max|beta_diff|  = {max_beta_diff:.10e}")
    print(f"max|x_diff|     = {max_x_diff:.10e}")
    print(f"max|loglik_diff|= {max_loglik_diff:.10e}")
    print("PASS" if passed else "FAIL")


if __name__ == "__main__":
    main()

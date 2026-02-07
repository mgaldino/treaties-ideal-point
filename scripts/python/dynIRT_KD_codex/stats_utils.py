import math
import numpy as np

try:
    from scipy.stats import norm as _norm

    def logpdf(z):
        return _norm.logpdf(z)

    def logcdf(z):
        return _norm.logcdf(z)

    def logsf(z):
        return _norm.logsf(z)

except Exception:
    _LOG_SQRT_2PI = 0.5 * np.log(2.0 * np.pi)
    _SQRT2 = np.sqrt(2.0)
    _erfc = np.vectorize(math.erfc, otypes=[float])

    def logpdf(z):
        z = np.asarray(z, dtype=float)
        return -_LOG_SQRT_2PI - 0.5 * z * z

    def logcdf(z):
        z = np.asarray(z, dtype=float)
        return np.log(0.5 * _erfc(-z / _SQRT2))

    def logsf(z):
        z = np.asarray(z, dtype=float)
        return np.log(0.5 * _erfc(z / _SQRT2))

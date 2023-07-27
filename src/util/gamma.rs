use num_complex::Complex64;
use std::f64::consts;

// g = 8
// n = 12
// numbers taken from wikipedia:
// https://en.wikipedia.org/wiki/Lanczos_approximation

const GAMMA_P: &[f64] = &[
    1975.3739023578852322,
    -4397.3823927922428918,
    3462.6328459862717019,
    -1156.9851431631167820,
    154.53815050252775060,
    -6.2536716123689161798,
    0.034642762454736807441,
    -7.4776171974442977377e-7,
    6.3041253821852264261e-8,
    -2.7405717035683877489e-8,
    4.0486948817567609101e-9,
];

const GAMMA_LEN: usize = GAMMA_P.len();
const GAMMA_G: f64 = 8.0;
const SQRT_2PI: f64 = 2.506628274631000502416;

const FACTORIAL: &[u64] = &[
    1,
    1,
    2,
    6,
    24,
    120,
    720,
    5040,
    40320,
    362880,
    3628800,
    39916800,
    479001600,
    6227020800,
    87178291200,
    1307674368000,
    20922789888000,
    355687428096000,
    6402373705728000,
    121645100408832000,
    2432902008176640000
];

// Credit for implementation:
// https://github.com/Bobingstern/Lanczos-Approximation/blob/main/gamma.hpp
pub fn gamma(z: Complex64) -> Complex64 {
    // exact factorial calculation
    if z.im == 0.0 && z.re.fract() == 0.0 {
        let x = z.re - 1.0;

        if x < 0.0 {
            return f64::NAN.into();
        }

        let n = x as usize;

        if n < FACTORIAL.len() {
            return (FACTORIAL[n] as f64).into()
        }
    }

    // Reflection formula
    if z.re < 0.5 {
        let t = Complex64 {
            re: 1f64 - z.re,
            im: -z.im,
        };
        let r = z * consts::PI;
        let y = Complex64 {
            re: consts::PI,
            im: 0f64,
        };

        return y / (Complex64::sin(r) * gamma(t));
    }

    let z = Complex64 {
        re: z.re - 1.0,
        im: z.im,
    };

    let mut x = Complex64 {
        re: 0.9999999999999999298,
        im: 0f64,
    };
    for i in 0..GAMMA_LEN {
        let gamma_pval = Complex64 {
            re: GAMMA_P[i],
            im: 0f64,
        };
        x += gamma_pval / (z + (i + 1) as f64)
    }

    let t = Complex64 {
        re: z.re + 0.5 + GAMMA_G,
        im: z.im,
    };
    let t_pow = t.powc(z + 0.5);
    let neg = -t;
    let exp_t = neg.exp();

    x * SQRT_2PI * t_pow * exp_t
}

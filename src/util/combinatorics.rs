use num_bigfloat::{BigFloat, EPSILON};

use super::gamma::factorial;

fn gcd(a: i128, b: i128) -> i128 {
    let mut p = a;
    let mut q = b;

    while q != 0 {
        (p, q) = (q, p % q)
    }

    return p;
}

fn ncr_int_int(n: i128, r: i128) -> BigFloat {
    if n == 0 && r == 0 {
        return num_bigfloat::ONE;
    }

    if n < r && n >= 0 {
        return num_bigfloat::ZERO;
    }

    let mut numer: i128 = 1;
    let mut denom: i128 = 1;

    for i in 0..r {
        numer *= n - i;
        denom *= r - i;
        let d = gcd(numer, denom);
        numer /= d;
        denom /= d
    }

    BigFloat::from(numer / denom)
}

fn ncr_int(n: &BigFloat, r: i128) -> BigFloat {
    if n.frac().abs() < EPSILON {
        let n_int = match n.to_i128() {
            Some(v) => v,
            None => return num_bigfloat::NAN,
        };
        return ncr_int_int(n_int, r);
    };

    let mut cur = num_bigfloat::ONE;

    for i in 0..r {
        cur *= *n - BigFloat::from(i);
        cur /= BigFloat::from(r - i);
    }

    cur
}

pub fn ncr(n: &BigFloat, r: &BigFloat) -> BigFloat {
    if r < &num_bigfloat::ZERO {
        return num_bigfloat::ZERO;
    }

    if r.frac().abs() < EPSILON {
        let r_int = match r.to_i128() {
            Some(v) => v,
            None => return num_bigfloat::NAN,
        };
        return ncr_int(n, r_int);
    }

    factorial(n) / (factorial(&(*n - r)) * factorial(r))
}

fn npr_int_int(n: i128, r: i128) -> BigFloat {
    if n == 0 && r == 0 {
        return num_bigfloat::ONE;
    }

    if r >= 0 {
        if n < r && n >= 0 {
            num_bigfloat::ZERO
        } else {
            let mut ans = 1;

            for i in 0..r {
                ans *= n - i;
            }

            BigFloat::from(ans)
        }
    } else {
        let mut ans: i128 = 1;

        // automatically, n - r > n
        // => n! / (n-r)! = 1/((n+1) * ... * (n - r))

        for i in n + 1..(n - r + 1) {
            ans *= i;
        }

        num_bigfloat::ONE / BigFloat::from(ans)
    }
}

fn npr_int(n: &BigFloat, r: i128) -> BigFloat {
    if n.frac().abs() < EPSILON {
        let n_int = match n.to_i128() {
            Some(v) => v,
            None => return num_bigfloat::NAN,
        };
        return npr_int_int(n_int, r);
    };

    let mut cur = num_bigfloat::ONE;

    for i in 0..r {
        cur *= *n - BigFloat::from(i);
    }

    cur
}

pub fn npr(n: &BigFloat, r: &BigFloat) -> BigFloat {
    if r.frac().abs() < EPSILON {
        let r_int = match r.to_i128() {
            Some(v) => v,
            None => return num_bigfloat::NAN,
        };
        return npr_int(n, r_int);
    }
    factorial(n) / factorial(&(*n - r))
}

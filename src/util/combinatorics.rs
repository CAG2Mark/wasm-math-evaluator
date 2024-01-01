use num_bigfloat::{BigFloat, RoundingMode::ToEven};

use super::{bigfloat_utils::try_to_int, gamma::factorial};

fn gcd(a: i128, b: i128) -> i128 {
    let mut p = a;
    let mut q = b;

    while q != 0 {
        (p, q) = (q, p % q)
    }

    return p;
}

fn ncr_int_int(n: i128, r: i128) -> Option<BigFloat> {
    if n == 0 && r == 0 {
        return Some(num_bigfloat::ONE);
    }

    if n < r && n >= 0 {
        return Some(num_bigfloat::ZERO);
    }

    let mut numer: i128 = 1;
    let mut denom: i128 = 1;

    for i in 0..r {
        let g = gcd(n - i, r - i);

        numer = numer.checked_mul((n - i) / g)?;
        denom = denom.checked_mul((r - i) / g)?;

        let d = gcd(numer, denom);
        numer /= d;
        denom /= d
    }

    Some(BigFloat::from(numer / denom))
}

fn ncr_int(n: &BigFloat, r: i128) -> BigFloat {
    if r < 0 {
        return num_bigfloat::ZERO;
    }

    fn try_int_int(n: &BigFloat, r: i128) -> Option<BigFloat> {
        ncr_int_int(try_to_int(&n)?, r)
    }

    let round = try_to_int(&n).is_some();

    match try_int_int(&n, r) {
        Some(ans) => ans,
        None => {
            let mut cur = num_bigfloat::ONE;

            for i in 0..r {
                cur *= *n - BigFloat::from(i);
                cur /= BigFloat::from(r - i);
            }

            if round {
                cur.round(0, ToEven)
            } else {
                cur
            }
        }
    }
}

pub fn ncr(n: &BigFloat, r: &BigFloat) -> BigFloat {
    match try_to_int(&r) {
        Some(r) => ncr_int(n, r),
        None => match try_to_int(&(*n - r)) {
            Some(p) if p < 0 => num_bigfloat::ZERO,
            _ => factorial(n) / (factorial(&(*n - r)) * factorial(r))
        }
    }
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
    match try_to_int(n) {
        Some(n) => npr_int_int(n, r),
        None => {
            let mut cur = num_bigfloat::ONE;

            for i in 0..r {
                cur *= *n - BigFloat::from(i);
            }

            cur
        }
    }
}

pub fn npr(n: &BigFloat, r: &BigFloat) -> BigFloat {
    match try_to_int(&r) {
        Some(r) => npr_int(n, r),
        None => factorial(n) / factorial(&(*n - r)),
    }
}

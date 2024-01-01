use num_bigfloat::{BigFloat, RoundingMode::ToEven};

use super::bigfloat_utils::try_to_int;

// Computed myself.
// (Using Sage if you're curious)
/*
[0.999999999999997091820464226980423814845091856769434826679,
 57.1562356658629235165793934859774442732544428929201597974,
 -59.5979603554754912481422661316028360019101008542062523362,
 14.1360979747417471738634195408767260156591640475908033406,
 -0.491913816097620199782840028530307844163480831247389540127,
 0.0000339946499848118886989193415523415528348299560683871662560,
 0.0000465236289270485756652302249658228895163875629445539657351,
 -0.0000983744753048795646765383706347073037313023763295354403184,
 0.000158088703224912488836072413443663961027996353183006491358,
 -0.000210264441724104883192699283005711909975091780943802677890,
 0.000217439618115212643196144649603834698887252870433365891148,
 -0.000164318106536763890217069562280184138735003751345784173915,
 0.0000844182239838527432928118153454549852880174385776729071038,
 -0.0000261908384015814086696650362479549032790025720474993021797,
 3.68991826595316227036759674456787385147712197067648273315e-6]
*/

// Usually accurate up to 15-20 decimal places.
// Unused, Spouge's approximation is way better.

/*
pub fn gamma(z: BigFloat) -> BigFloat {
    // exact factorial calculation
    if z.frac().abs() < num_bigfloat::EPSILON {
        let mut x = z - num_bigfloat::ONE;

        let mut ans = num_bigfloat::ONE;

        if x < num_bigfloat::ZERO {
            return f64::NAN.into();
        }

        while x.abs() >= num_bigfloat::EPSILON {
            ans *= x - x.frac();
            x -= num_bigfloat::ONE;
        }

        return ans;
    }

    let gamma_p: &[BigFloat] = &[
        BigFloat::parse("0.999999999999997091820464226980423814845091856769434826679").unwrap(),
        BigFloat::parse("57.1562356658629235165793934859774442732544428929201597974").unwrap(),
        BigFloat::parse("-59.5979603554754912481422661316028360019101008542062523362").unwrap(),
        BigFloat::parse("14.1360979747417471738634195408767260156591640475908033406").unwrap(),
        BigFloat::parse("-0.491913816097620199782840028530307844163480831247389540127").unwrap(),
        BigFloat::parse("0.0000339946499848118886989193415523415528348299560683871662560").unwrap(),
        BigFloat::parse("0.0000465236289270485756652302249658228895163875629445539657351").unwrap(),
        BigFloat::parse("-0.0000983744753048795646765383706347073037313023763295354403184")
            .unwrap(),
        BigFloat::parse("0.000158088703224912488836072413443663961027996353183006491358").unwrap(),
        BigFloat::parse("-0.000210264441724104883192699283005711909975091780943802677890").unwrap(),
        BigFloat::parse("0.000217439618115212643196144649603834698887252870433365891148").unwrap(),
        BigFloat::parse("-0.000164318106536763890217069562280184138735003751345784173915").unwrap(),
        BigFloat::parse("0.0000844182239838527432928118153454549852880174385776729071038").unwrap(),
        BigFloat::parse("-0.0000261908384015814086696650362479549032790025720474993021797")
            .unwrap(),
        BigFloat::parse("0.00000368991826595316227036759674456787385147712197067648273315e")
            .unwrap(),
    ];

    let gamma_len: usize = gamma_p.len();
    let gamma_g: BigFloat = BigFloat::from(4.7421875);
    let sqrt_2pi: BigFloat =
        BigFloat::parse("2.50662827463100050241576528481104525300698674060993831663").unwrap();

    // Reflection formula
    if z < BigFloat::from(0.5) {
        let t = num_bigfloat::ONE - z;
        return num_bigfloat::PI / ((num_bigfloat::PI * z).sin() * gamma(t));
    }

    let mut x = gamma_p[0];

    let w = z - num_bigfloat::ONE;

    for i in 1..gamma_len {
        x += gamma_p[i] / (w + BigFloat::from(i as u8));
    }

    let t = w + gamma_g + BigFloat::from(0.5);
    let t_pow = t.pow(&(w + BigFloat::from(0.5)));
    let neg = -t;
    let exp_t = neg.exp();
    x * sqrt_2pi * t_pow * exp_t
}
*/

pub fn spouge_coeff(a: u8, k: u8) -> BigFloat {
    if k == 0 {
        return (num_bigfloat::PI * BigFloat::from(2)).sqrt();
    } else {
        let sgn = if k % 2 == 1 { 1 } else { -1 };
        // A little trick.
        // The formula is
        // (-1)^(k + 1) (a - k)^(k - 1/2) e^(a - k) / (k - 1)!
        // To avoid directly computing the factorial which limits
        // the number of terms we can use, we compute
        // (a - k)^(k - 1/2)
        // and
        // (k - 1)!
        // iteratively, adding one term each time.
        let mut prod = num_bigfloat::ONE;
        let base = BigFloat::from(a) - BigFloat::from(k);
        for i in 1..k {
            prod *= base;
            prod /= BigFloat::from(i)
        }
        // prod is now equal to (a-k)^(k-1) / (k - 1)!
        // Multiply the remaining (a - k)^(1/2)
        prod *= base.sqrt();

        // let f = fact(k - 1);
        // let exponent = BigFloat::from(k) - BigFloat::from(0.5);
        // let pow_ = BigFloat::from(a - k).pow(&exponent);
        let exp_ = BigFloat::from(a - k).exp();
        BigFloat::from(sgn) * exp_ * prod
    }
}

pub fn gamma_spouge(z: &BigFloat) -> BigFloat {
    let a: u8 = 34;

    match try_to_int(z) {
        Some(n) if n < 34 => {
            // exact factorial calculation
            if n < 0 {
                return f64::NAN.into();
            }
            if n == 0 {
                return num_bigfloat::ONE;
            }
            let mut m: u128 = n as u128 - 1;

            let mut ans: u128 = 1;

            while m > 0 {
                ans *= m;
                m -= 1
            }

            BigFloat::from(ans)
        }
        Some(n) => {
            let mut m: u128 = n as u128 - 1;

            let mut ans = num_bigfloat::ONE;
            
            while m > 0 {
                ans *= BigFloat::from(m);
                m -= 1
            }

            ans.round(0, ToEven)
        }
        _ => {
            // Reflection formula
            if z < &BigFloat::from(0.5) {
                let t = num_bigfloat::ONE - z;
                return num_bigfloat::PI / ((num_bigfloat::PI * z).sin() * gamma_spouge(&t));
            }

            let mut x = spouge_coeff(a, 0);

            // console_log!("{}", z_bigg);
            let w = *z - BigFloat::from(1);

            for i in 1..a {
                x += spouge_coeff(a, i) / (w + BigFloat::from(i))
            }

            let t = w + BigFloat::from(a);
            let exponent = w + BigFloat::from(0.5);
            let t_pow = t.pow(&exponent);
            let neg = -t;
            let exp_t = neg.exp();

            x * t_pow * exp_t
        }
    }
}

pub fn factorial(z: &BigFloat) -> BigFloat {
    return gamma_spouge(&(num_bigfloat::ONE + z));
}

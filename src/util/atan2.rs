use num_bigfloat::BigFloat;

pub fn atan2(y: &BigFloat, x: &BigFloat) -> BigFloat {
    let zero = num_bigfloat::ZERO;
    let pi = num_bigfloat::PI;
    let piby2 = num_bigfloat::PI / BigFloat::from(2);
    if x > &zero {
        (*y / x).atan()
    } else if y > &zero {
        piby2 - (*x / y).atan()
    } else if y < &zero {
        -piby2 - (*x / y).atan()
    } else if x < &zero {
        let ans = (*y / x).atan();
        if &ans < &zero {
            ans + &pi
        } else {
            ans - &pi
        }
    } else {
        num_bigfloat::NAN
    }
}
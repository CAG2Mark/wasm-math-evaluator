use num_bigfloat::BigFloat;

pub fn get_exponent(v: &BigFloat) -> i64 {
    v.abs().log10().floor().to_i64().unwrap()
}

// auto determine decimal places
pub fn bigfloat_auto_str(v: &BigFloat) -> String {
    if v.is_nan() || v.is_inf() {
        return v.to_string();
    }

    if v.abs() < num_bigfloat::EPSILON {
        return "0".to_string();
    }

    // If the exponent is too extreme
    // use scientific notation
    let exp_adjusted = get_exponent(v);

    if exp_adjusted >= 30 || exp_adjusted <= -5 {
        return bigfloat_scientific(v);
    }

    let mut u = *v;
    let mut d = 0;
    let ten = BigFloat::from(10);

    while u.abs() < num_bigfloat::MAX && u.frac().abs() >= num_bigfloat::EPSILON {
        d += 1;
        u *= ten;
    }
    return bigfloat_to_str(v, d);
}

pub fn mantissa_tostr(mantissa: [i16; 10], fill: bool) -> String {
    let mut cur = "".to_string();
    
    let mut started = false;
    for x in mantissa.iter().rev() {
        if *x == 0 {
            continue;
        }
        // right align, pad left with zeros
        if started {
            cur += &format!("{:0>4}", x)
        } else {
            cur += &x.to_string()
        }

        started = true;
    }

    if fill {
        format!("{:0<40}", cur)
    } else {
        cur
    }
    
}

pub fn bigfloat_scientific(v: &BigFloat) -> String {
    if v.is_nan() || v.is_inf() {
        return v.to_string();
    }

    let (mantissa, _dp, sign, _exp) = v.to_raw_parts().unwrap();

    let cur = mantissa_tostr(mantissa, false);

    let exp_adjusted = get_exponent(v);

    let ret = cur[..1].to_string() + "." + &cur[1..] + &format!("\\times10^{{{exp_adjusted}}}");

    if sign == 1 {
        ret
    } else {
        "-".to_string() + &ret
    }
}

pub fn bigfloat_to_str(v: &BigFloat, decimal_digits: usize) -> String {
    if v.is_nan() || v.is_inf() {
        return v.to_string();
    }

    if v.abs() < num_bigfloat::EPSILON {
        return "0".to_string();
    }

    let (mantissa, _dp, sign, _exp) = v.to_raw_parts().unwrap();
    
    let mut cur = mantissa_tostr(mantissa, false);

    let exp_adjusted = get_exponent(v);

    // case 1: we need to add a decimal point at the start
    let ret = if exp_adjusted < 0 {
        let l = cur.len().min(decimal_digits - (-exp_adjusted as usize) + 1);
        if l == 0 {
            "0".to_string()
        } else {
            let mut pre: String = "0.".to_string();
            for _ in 0..(-exp_adjusted - 1) {
                pre += "0"
            }
            // truncate decimal points

            pre + &cur[0..l]
        }
    }
    // case 2: decimal point is added in between
    else if (exp_adjusted as usize) < cur.len() - 1 {
        let idx = exp_adjusted as usize + 1;
        let first_part = &cur[..idx];
        let second_part = &cur[idx..];

        if decimal_digits == 0 {
            first_part.to_string()
        } else {
            let l = second_part.len().min(decimal_digits);

            let second_sliced = &second_part[..l];

            first_part.to_string() + "." + second_sliced
        }
    }
    // case 3: decimal point implies the number is exactly an integer
    else if (exp_adjusted as usize) == cur.len() - 1 {
        cur
    } else {
        // case 4: need to add zeros
        let add = exp_adjusted - (cur.len() as i64) + 1;
        for _ in 0..add {
            cur += "0"
        }

        cur
    };

    if sign == 1 {
        ret
    } else {
        "-".to_string() + &ret
    }
}

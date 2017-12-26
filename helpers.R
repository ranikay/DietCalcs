library(data.table)
library(DT)

# Basic conversions
lb_to_kg <- function(lb){
  return(lb / 2.2)
}
kg_to_lb <- function(kg){
  return(kg * 2.2)
}
in_to_cm <- function(inch){
  return(inch * 2.54)
}
cm_to_in <- function(cm){
  return(cm / 2.54)
}
bmi <- function(ht, wt, ht_unit = 'cm', wt_unit = 'kg'){
  w = ifelse(wt_unit == 'kg', wt, lb_to_kg(wt))
  h = ifelse(ht_unit == 'cm', ht / 100, in_to_cm(ht) / 100)
  return(round(w / (h**2), 1))
}
ibw <- function(sex, ht_in){
  result = ifelse(sex == 'Male', 
    106 + (ht_in - 60) * 6, 100 + (ht_in - 60) * 5)
  return(round(result, 0))
}
abw <- function(wt_lb, ibw_lb){
  return(round((wt_lb - ibw_lb) * .25 + ibw_lb, 1))
}

# Patient info
get_pt_ht_wt <- function(ht_in, wt_lb){
  return(data.frame(
    ht_in = ht_in,
    ht_cm = round(in_to_cm(ht_in), 1),
    wt_lbs = wt_lb,
    wt_kg = round(lb_to_kg(wt_lb), 1)
  ))
}

# All the body weights
get_wts <- function(ht_in, wt_lb, ibw_lb){
  abw_lb = (wt_lb - ibw_lb) * .25 + ibw_lb
  return(data.frame(
    BMI = bmi(ht_in, wt_lb, 'in', 'lb'),
    IBW = ibw_lb,
    IBW_p = round((wt_lb / ibw_lb) * 100, 1),
    ABW_lb = round(abw_lb, 1),
    ABW_kg = round(lb_to_kg(abw_lb), 1)
  ))
}

# Mifflin St Jeor
get_msj <- function(sex, ht_cm, wt_kg, age){
  dat = data.frame(
    sapply(c(1, 1.1, 1.2, 1.3, 1.5), function(a){
      list(msj(sex, ht_cm, wt_kg, age, activity = a))
    })
  )
  dat$Sex = sex
  return(dat)
}
msj <- function(sex, height_cm, weight_kg, age_yr, activity = 1){
  if (sex == 'Male'){
    result = 10 * weight_kg + 6.25 * height_cm - 5 * age_yr + 5
  }
  if (sex == 'Female'){
    result = 10 * weight_kg + 6.25 * height_cm - 5 * age_yr - 161
  }
  return(round(result * activity, 0))
}

# Quick method
get_qm <- function(wt_kg){
  dat = data.frame(
    sapply(c(20, 22, 25, 30, 32, 35), function(k){
      list(round(k * wt_kg, 0))
    })
  )
  return(dat)
}

# Quick method
get_protein <- function(wt_kg, abw_kg){
  dat = data.frame(
    CBW = sapply(c(.8, 1, 1.2, 1.5, 2), function(k){
      round(k * wt_kg, 1)
    }),
    ABW = sapply(c(.8, 1, 1.2, 1.5, 2), function(k){
      round(k * abw_kg, 1)
    })
  )
  return(t(dat))
}

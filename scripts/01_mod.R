

# libraries --------------------------------------------------------------------
library(lme4)
library(lmerTest)
library(sjPlot)


# mod_asm ----------------------------------------------------------------------
mod_asm_no <- glm(
  asm_ordered ~
    group +
    scale(age, scale = FALSE) + 
    sex +
    neuromodulation +
    scale(total_meds_baseline, scale = FALSE)
  ,
  family = "binomial",
  ed_no
)
summary(mod_asm_no)

mod_asm_yes <- glmer(
  asm_ordered ~
    group +
    scale(age, scale = FALSE) + 
    sex +
    neuromodulation +
    scale(total_meds_baseline, scale = FALSE) +
    (1|id)
  ,
  family = binomial,
  ed_yes
)
summary(mod_asm_yes)


# mod_ct -----------------------------------------------------------------------
mod_ct_no <- glm(
  as.factor(ct) ~
    group +
    scale(age, scale = FALSE) + 
    sex +
    neuromodulation +
    scale(total_meds_baseline, scale = FALSE)
  ,
  family = "binomial",
  ed_no
)
summary(mod_ct_no)

mod_ct_yes <- glmer(
  as.factor(ct) ~
    group +
    scale(age, scale = FALSE) + 
    sex +
    neuromodulation +
    scale(total_meds_baseline, scale = FALSE) +
    (1|id)
  ,
  family = binomial,
  ed_yes
)
summary(mod_ct_yes)


# mod_tox ----------------------------------------------------------------------
mod_tox_no <- glm(
  as.factor(tox_screen) ~
    group +
    scale(age, scale = FALSE) + 
    sex +
    neuromodulation +
    scale(total_meds_baseline, scale = FALSE)
  ,
  family = "binomial",
  ed_no
)
summary(mod_tox_no)

mod_tox_yes <- glmer(
  as.factor(tox_screen) ~
    group +
    scale(age, scale = FALSE) + 
    sex +
    neuromodulation +
    scale(total_meds_baseline, scale = FALSE) +
    (1|id)
  ,
  family = binomial,
  ed_yes
)
summary(mod_tox_yes)


# mod_duration_ed --------------------------------------------------------------
mod_duration_ed_no <- glm(
  log(duration_ed) ~
    group +
    scale(age, scale = FALSE) + 
    sex +
    neuromodulation +
    scale(total_meds_baseline, scale = FALSE) ,
  data = ed_no
)
summary(mod_duration_ed_no)

mod_duration_ed_yes <- lmer(
  log(duration_ed) ~
    group +
    scale(age, scale = FALSE) + 
    sex +
    neuromodulation +
    scale(total_meds_baseline, scale = FALSE) +
    (1|id),
  ed_yes
)
summary(mod_duration_ed_yes)



# libraries --------------------------------------------------------------------
library(table1)
library(sjPlot)


# pvalue function --------------------------------------------------------------
pvalue <- function(x, ...) {

    y <- unlist(x)
    g <- factor(rep(1:length(x), times=sapply(x, length)))
    if (is.numeric(y)) {

    ano <- aov(y ~ g)
    p <- summary(ano)[[1]][[5]][1]
    
  } else {

        p <- chisq.test(table(y, g))$p.value
        
  }

  c(
    "", 
    sub(
      "<", 
      "&lt;", 
      format.pval(
        p, 
        digits = 3,
        eps = 0.001
        )
      )
    )
  
}

# by group
table1(
  ~
    revisit + 
    age + 
    sex +
    substance_abuse +
    neuromodulation +
    hx_head_trauma +
    hx_psych_trauma +
    prior_eeg +
    seizure_captured +
    admitted_monitored +
    total_meds_baseline |
    group
  ,
  extra.col = list(`p-value`=pvalue),
  overall = FALSE,
  data = ed
)


# unique subjects
ed_no %>% group_by(group) %>% distinct(id, .keep_all = TRUE) %>% summarize(n = n())
ed_yes %>% group_by(group) %>% distinct(id, .keep_all = TRUE) %>% summarize(n = n())


# asm --------------------------------------------------------------------------
table1(
  ~
    as.factor(asm_ordered) |
    group
  ,
  overall = FALSE,
  data = ed
)


# ct ---------------------------------------------------------------------------
table1(
  ~
    as.factor(ct) |
    group
  ,
  overall = FALSE,
  data = ed
)


# tox --------------------------------------------------------------------------
table1(
  ~
    as.factor(tox_screen) |
    group
  ,
  overall = FALSE,
  data = ed
)


# duration ed ------------------------------------------------------------------
table1(
  ~ duration_ed | group, data = ed
)


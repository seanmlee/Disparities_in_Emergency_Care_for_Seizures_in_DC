

# libraries --------------------------------------------------------------------
library(tidyverse)


# read .Renviron ---------------------------------------------------------------
ed <- Sys.getenv("ED_DATA")
ed <- read.csv(ed)


# format -----------------------------------------------------------------------
ed <- read.csv("data/ed.csv", header = TRUE) %>%
  
  filter(
    race == "Black or African American" | race == "White"
  ) %>%
  
  filter(
    age > 0
  ) %>%
  
  mutate(
    revisit = ifelse(
      duplicated(id) | duplicated(id, fromLast = TRUE), 
      "Yes", 
      "No"
      )
    ) %>%
  
  filter(
    neuromodulation != "lacosamide"
  ) %>%
  
  mutate(
    tox_screen = ifelse(
      tox_screen == "None",
      "No",
      tox_screen
    )
  ) %>%
  
  mutate(
    asm_ordered = ifelse(
      asm_ordered == "ordered",
      1,
      0
    )
  ) %>%
  
  mutate(
    ward = ifelse(
      ward >= 7,
      "7-8",
      "0-6"
    )
  ) %>%
  
  mutate(
    ward = as.factor(ward)
  ) %>%
  
  mutate(
    prior_eeg = case_when(
      prior_eeg == " No" ~ "No",
      prior_eeg == " Yes" ~ "Yes",
      TRUE ~ prior_eeg
    )
  ) %>%
  
  mutate(
    baseline_asm = as.factor(baseline_asm)
  ) %>%
  
  mutate(
    group = case_when(
      race == "White" & ward == "0-6" ~ "A",
      race == "Black or African American" & ward == "0-6" ~ "B",
      race == "Black or African American" & ward == "7-8" ~ "C",
      TRUE ~ NA
    )
  ) %>%
  
  mutate(
    across(
      c(
        ct, 
        substance_abuse, 
        neuromodulation, 
        hx_head_trauma, 
        hx_psych_trauma,
        prior_eeg,
        seizure_captured, 
        admitted_monitored,
        baseline_asm  
        ), ~ gsub(" ", "", .)
      )
    ) %>%
  
  filter(
    !is.na(group)
  )


# divvy ------------------------------------------------------------------------
ed_no <- ed %>% filter(revisit == "No")
ed_yes <- ed %>% filter(revisit == "Yes")

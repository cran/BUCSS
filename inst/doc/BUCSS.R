## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(BUCSS)

## -----------------------------------------------------------------------------
result <- ss_buc_independent_t(t_observed = 3, n = 20, alpha_prior = .05,
                         alpha_planned = .05, assurance = .80, desired_power = .80)
result

## -----------------------------------------------------------------------------
result$value[result$term == "necessary_n_per_group"]
result$value[result$term == "ncp_adjusted"]
result$value[result$term == "smd_planning"]

## -----------------------------------------------------------------------------
attr(result, "sample_size_unit")

## -----------------------------------------------------------------------------
tidy(result)
tidy(result)$estimate
glance(result)

## -----------------------------------------------------------------------------
planning_sentence(result)

## -----------------------------------------------------------------------------
replication <- ss_buc_independent_t(t_observed = 3, n = c(50, 55),
                                    alpha_prior = .05, alpha_planned = .05,
                                    desired_power = .80, assurance = .90)
replication

## ----error = TRUE-------------------------------------------------------------
try({
ss_buc_independent_t(t_observed = 3, n = 20, assurance = .95)
})


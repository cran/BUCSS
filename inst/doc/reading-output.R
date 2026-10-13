## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(BUCSS)

## -----------------------------------------------------------------------------
result <- ss_buc_factorial_anova(F_observed = 5, N = 120, levels_A = 2,
                                 levels_B = 3, effect = "factor_B")

## -----------------------------------------------------------------------------
result

## -----------------------------------------------------------------------------
old <- options(bucss.digits = 5)
result
options(old)  # restore the previous setting

## -----------------------------------------------------------------------------
result$value[result$term == "necessary_n_per_cell"]
result$value[result$term == "total_N"]

## -----------------------------------------------------------------------------
tidy(result)
tidy(result)$estimate

## -----------------------------------------------------------------------------
glance(result)
glance(result)$ncp_adjusted

## -----------------------------------------------------------------------------
planning_sentence(result)

## -----------------------------------------------------------------------------
f_planning <- result$value[result$term == "f_planning"]
f_planning
f_planning^2 / (1 + f_planning^2)  # the same effect as a partial eta squared

## -----------------------------------------------------------------------------
total_N <- result$value[result$term == "total_N"]
1 - pf(qf(.95, 2, total_N - 6), 2, total_N - 6, ncp = total_N * f_planning^2)
result$value[result$term == "actual_power"]


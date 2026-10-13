## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(BUCSS)
# Helpers for numbers quoted in the text: one row of a result by its term, and
# a proportion at the printed tables' precision, without the leading zero.
row_value <- function(x, term) x$value[x$term == term]
apa <- function(x, digits = 3) {
  sub("^0\\.", ".", format(signif(x, digits), scientific = FALSE))
}

## -----------------------------------------------------------------------------
first_plan <- ss_buc_correlation(r_observed = .35, N = 100, alpha_prior = .05,
                                 alpha_planned = .05, assurance = .80,
                                 desired_power = .80)
first_plan

## -----------------------------------------------------------------------------
assurances <- c(.5, .7, .8, .9)
sizes <- sapply(assurances, function(a) {
  plan <- ss_buc_correlation(r_observed = .35, N = 100, assurance = a)
  plan$value[plan$term == "necessary_N"]
})
data.frame(assurance = assurances, necessary_N = sizes)

## -----------------------------------------------------------------------------
t_equivalent <- .35 * sqrt(98 / (1 - .35^2))
by_correlation <- ss_buc_correlation(t_observed = t_equivalent, N = 100)
by_regression  <- ss_buc_reg_coef(t_observed = t_equivalent, N = 100, p = 1,
                                  random_predictors = FALSE)
c(correlation = by_correlation$value[by_correlation$term == "necessary_N"],
  regression  = by_regression$value[by_regression$term == "necessary_N"])

## -----------------------------------------------------------------------------
alphas <- c(.05, .20, 1)
sizes <- sapply(alphas, function(ap) {
  plan <- ss_buc_correlation(r_observed = .35, N = 100, alpha_prior = ap)
  plan$value[plan$term == "necessary_N"]
})
data.frame(alpha_prior = alphas, necessary_N = sizes)

## -----------------------------------------------------------------------------
plan <- ss_buc_correlation(r_observed = .35, N = 100, assurance = .80)
ncp <- plan$value[plan$term == "ncp_adjusted"]
audit <- ss_buc_sensitivity(plan, true_ncp = ncp, replications = 2000,
                            seed = 2017)
audit

## -----------------------------------------------------------------------------
smaller <- ss_buc_sensitivity(plan, true_ncp = ncp * 0.6, replications = 2000,
                              seed = 2017)
rows <- c("attainment_with_refusals", "attainment", "refusal_rate",
          "size_at_true_effect")
data.frame(term = rows, value = smaller$value[match(rows, smaller$term)])


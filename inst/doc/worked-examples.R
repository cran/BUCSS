## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(BUCSS)

## -----------------------------------------------------------------------------
ss_buc_independent_t(t_observed = 3, n = 20, alpha_prior = .05, alpha_planned = .05,
               assurance = .80, desired_power = .80)

## -----------------------------------------------------------------------------
ss_buc_paired_t(t_observed = 3, N = 40, alpha_prior = .05, alpha_planned = .05,
                assurance = .80, desired_power = .80)

## -----------------------------------------------------------------------------
ss_buc_one_sample_t(t_observed = 3.2, N = 30, alpha_prior = .05,
                    alpha_planned = .05, assurance = .80, desired_power = .80)

## -----------------------------------------------------------------------------
ss_buc_one_way_anova(F_observed = 5, N = 120, levels_A = 4, alpha_prior = .05,
                     alpha_planned = .05, assurance = .80, desired_power = .80)

## -----------------------------------------------------------------------------
ss_buc_factorial_anova(F_observed = 5, N = 120, levels_A = 2, levels_B = 3,
                       effect = "factor_B", alpha_prior = .05,
                       alpha_planned = .05, assurance = .80, desired_power = .80)

## -----------------------------------------------------------------------------
ss_buc_factorial_anova_general(F_observed = 5, N = 120, cells = 6,
                               df_numerator = 2, df_denominator = 114,
                               alpha_prior = .05, alpha_planned = .05,
                               assurance = .80, desired_power = .80)

## -----------------------------------------------------------------------------
ss_buc_rm_anova(F_observed = 5, N = 60, levels_A = 2, levels_B = 3,
                effect = "factor_B", alpha_prior = .05, alpha_planned = .05,
                assurance = .80, desired_power = .80)

## -----------------------------------------------------------------------------
ss_buc_rm_anova_general(F_observed = 6.5, N = 80, df_numerator = 1,
                        alpha_prior = .05, alpha_planned = .05, assurance = .50,
                        desired_power = .80)

## -----------------------------------------------------------------------------
ss_buc_mixed_anova(F_observed = 5, N = 60, levels_between = 2, levels_within = 3,
                   effect = "within", alpha_prior = .05, alpha_planned = .05,
                   assurance = .80, desired_power = .80)

## -----------------------------------------------------------------------------
ss_buc_mixed_anova_general(F_observed = 5, N = 90, df_numerator = 2,
                           num_groups = 3, effect = "between_only",
                           alpha_prior = .05, alpha_planned = .05,
                           assurance = .80, desired_power = .80)

## -----------------------------------------------------------------------------
ss_buc_rm_anova(F_observed = 6.5, N = 30, levels_A = 3, epsilon = 1.73 / 2)

## -----------------------------------------------------------------------------
ss_buc_rm_anova(F_observed = 6.5, N = 30, levels_A = 3)

## -----------------------------------------------------------------------------
eps <- c(1, .95, .9, .85, .8, .75)
sapply(eps, function(e) {
  res <- ss_buc_rm_anova(F_observed = 6.5, N = 30, levels_A = 3, epsilon = e)
  res$value[res$term == "necessary_N"]
})

## -----------------------------------------------------------------------------
ss_buc_reg_coef(t_observed = 3, N = 150, p = 3, alpha_prior = .05,
                alpha_planned = .05, assurance = .80, desired_power = .80)

## -----------------------------------------------------------------------------
ss_buc_R2(F_observed = 5, N = 150, p = 4, alpha_prior = .05,
          alpha_planned = .05, assurance = .80, desired_power = .80)

## -----------------------------------------------------------------------------
ss_buc_reg_joint(F_observed = 5, N = 150, p = 4, p_joint = 2,
                 alpha_prior = .05, alpha_planned = .05, assurance = .80,
                 desired_power = .80)

## -----------------------------------------------------------------------------
ss_buc_R2(F_observed = 5, N = 150, p = 4, random_predictors = TRUE)

## -----------------------------------------------------------------------------
ss_buc_correlation(r_observed = .35, N = 100, alpha_prior = .05,
                   alpha_planned = .05, assurance = .80, desired_power = .80)

## -----------------------------------------------------------------------------
t_equivalent <- .35 * sqrt(98 / (1 - .35^2))
by_correlation <- ss_buc_correlation(t_observed = t_equivalent, N = 100)
by_regression  <- ss_buc_reg_coef(t_observed = t_equivalent, N = 100, p = 1,
                                  random_predictors = FALSE)
c(correlation = by_correlation$value[by_correlation$term == "necessary_N"],
  regression  = by_regression$value[by_regression$term == "necessary_N"])

## -----------------------------------------------------------------------------
ss_buc_ancova(F_observed = 6, N = 120, cells = 3, n_covariates = 2,
              alpha_prior = .05, alpha_planned = .05, assurance = .80,
              desired_power = .80)

## -----------------------------------------------------------------------------
ss_buc_manova(F_observed = 4.5, N = 80, p_variables = 3, alpha_prior = .05,
              alpha_planned = .05, assurance = .80, desired_power = .80)

## -----------------------------------------------------------------------------
ss_buc_chisq_diff(chisq_observed = 9.5, N = 250, df_full = 42, df_restricted = 43,
                  alpha_prior = .05, alpha_planned = .05, assurance = .80,
                  desired_power = .80)

## ----error = TRUE-------------------------------------------------------------
try({
ss_buc_chisq_diff(chisq_observed = 9.5, N = 250, df_full = 0, df_restricted = 43)
})

## -----------------------------------------------------------------------------
ss_buc_welch_t(t_observed = 3, n_1 = 40, n_2 = 55, sd_ratio = 1.5,
               alpha_prior = .05, alpha_planned = .05, assurance = .80,
               desired_power = .80)

## -----------------------------------------------------------------------------
by_ratio <- ss_buc_welch_t(t_observed = 3, n_1 = 40, n_2 = 55, sd_ratio = 1.5)
by_sds <- ss_buc_welch_t(t_observed = 3, n_1 = 40, n_2 = 55, sd_1 = 8.4,
                         sd_2 = 12.6)
by_vars <- ss_buc_welch_t(t_observed = 3, n_1 = 40, n_2 = 55, var_1 = 70.56,
                          var_2 = 158.76)
size_term <- "necessary_n_per_group"
c(ratio = by_ratio$value[by_ratio$term == size_term],
  sds   = by_sds$value[by_sds$term == size_term],
  vars  = by_vars$value[by_vars$term == size_term])

## -----------------------------------------------------------------------------
ratios <- c(1, 1.5, 2, 2.5, 3)
sapply(ratios, function(r) {
  res <- ss_buc_welch_t(t_observed = 3, n_1 = 40, n_2 = 55, sd_ratio = r)
  res$value[res$term == "necessary_n_per_group"]
})


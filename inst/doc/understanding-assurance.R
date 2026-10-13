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
assurances <- c(.5, .6, .7, .8, .9)
n_by_assurance <- sapply(assurances, function(a) {
  r <- ss_buc_independent_t(t_observed = 3, n = 20, assurance = a, desired_power = .80)
  r$value[r$term == "necessary_n_per_group"]
})
data.frame(assurance = assurances, necessary_n_per_group = n_by_assurance)

## ----eval = FALSE-------------------------------------------------------------
# # Not run: this stops with an informative error.
# ss_buc_independent_t(t_observed = 3, n = 20, assurance = .95, desired_power = .80)
# #> Error: Sample size planning is not possible with these settings. After
# #> correcting the prior study's effect for publication bias and uncertainty
# #> at the requested level of assurance, the corrected noncentrality
# #> parameter is zero, which means there is not enough information in the
# #> prior result to plan the sample size as desired. With this prior result
# #> and 'alpha_prior', the largest 'assurance' that still yields a usable
# #> plan is about .90; re-run with 'assurance' at or below that value, or
# #> raise 'alpha_prior' to lift the ceiling. Re-running at a lower
# #> 'assurance' is not free: the plan it returns carries that lower
# #> assurance, not the one you first asked for, so the long run proportion
# #> of replications reaching your target power falls accordingly.
# #> 'alpha_prior' does not change the prior study, which is what it is; it
# #> is your assumption about the publication threshold the study's
# #> literature faced, so raising it toward 1 assumes less publication bias
# #> to undo (for example, 'alpha_prior = .10' assumes the literature's
# #> journals would have published a result significant at the .10 level, a
# #> more lenient policy than .05, and 'alpha_prior = 1' assumes no
# #> publication bias at all). See Anderson, Kelley, and Maxwell (2017) and
# #> Anderson and Kelley (2024).

## ----ceiling-plot, fig.width = 9, fig.height = 4.2, fig.alt = "Two panels sharing an assurance axis. On the left, the adjusted noncentrality parameter falls from about 2.6 to zero as assurance rises from .5 to the ceiling at .905. On the right, the necessary sample size per group climbs from 25 to tens of thousands on a logarithmic scale over the same range. Both panels shade the region beyond the ceiling and label it as one where no plan exists."----
plan <- ss_buc_independent_t(t_observed = 3, n = 20, assurance = .80)
plot(plan)

## ----fig.show = "hide"--------------------------------------------------------
curve_values <- plot(plan, which = "size", n_points = 12)
head(curve_values, 4)

## ----no-filter----------------------------------------------------------------
ss_buc_independent_t(t_observed = 3, n = 20,
                     alpha_prior = "No Publication Filter", assurance = .80,
                     desired_power = .80)

## -----------------------------------------------------------------------------
alphas <- c(.05, .10, .20, .50, 1)
n_by_alpha <- sapply(alphas, function(ap) {
  r <- ss_buc_independent_t(t_observed = 3, n = 20, alpha_prior = ap, assurance = .80,
                   desired_power = .80)
  r$value[r$term == "necessary_n_per_group"]
})
data.frame(alpha_prior = alphas, necessary_n_per_group = n_by_alpha)

## ----safeguard----------------------------------------------------------------
t_obs <- 3
N <- 30
# The lower limit of a two-sided interval at confidence level C: the
# noncentrality parameter at which the observed t is the (1 + C)/2 quantile.
lower_limit <- function(C) {
  uniroot(function(ncp) pt(t_obs, df = N - 1, ncp = ncp) - (1 + C) / 2,
          c(0, 10), tol = 1e-10)$root
}
# Ordinary power of a paired t test with n pairs, at a noncentrality
# parameter stated for the prior study's N pairs.
power_at <- function(n, ncp) {
  crit <- qt(.975, df = n - 1)
  delta <- ncp * sqrt(n / N)
  (1 - pt(crit, df = n - 1, ncp = delta)) + pt(-crit, df = n - 1, ncp = delta)
}
safeguard_n <- function(ncp) {
  n <- 2
  while (power_at(n, ncp) < .80) n <- n + 1
  n
}
limit_60 <- lower_limit(.60)
plan_sg <- ss_buc_paired_t(t_observed = t_obs, N = N, alpha_prior = 1,
                           assurance = .80)
data.frame(method = c("safeguard power", "BUCSS, alpha_prior = 1"),
           ncp = c(limit_60, row_value(plan_sg, "ncp_adjusted")),
           necessary_N = c(safeguard_n(limit_60),
                           row_value(plan_sg, "necessary_N")))

## ----safeguard-levels---------------------------------------------------------
levels_C <- c(.60, .80, .90)
data.frame(
  confidence_level = levels_C,
  assurance = (1 + levels_C) / 2,
  interval_lower_limit = sapply(levels_C, lower_limit),
  ncp_adjusted = sapply(levels_C, function(C) {
    plan <- ss_buc_paired_t(t_observed = t_obs, N = N, alpha_prior = 1,
                            assurance = (1 + C) / 2)
    row_value(plan, "ncp_adjusted")
  }))

## ----safeguard-strength-------------------------------------------------------
p_prior <- c(.01, .05, .10, .15)
t_prior <- qt(1 - p_prior / 2, df = N - 1)
signed_limit <- sapply(t_prior, function(t) {
  uniroot(function(ncp) pt(t, df = N - 1, ncp = ncp) - .80, c(0, 10),
          tol = 1e-10)$root
})
plans_1 <- lapply(t_prior, function(t) {
  ss_buc_paired_t(t_observed = t, N = N, alpha_prior = 1, assurance = .80)
})
data.frame(
  p_value = p_prior,
  signed_limit = signed_limit,
  ncp_adjusted = sapply(plans_1, row_value, term = "ncp_adjusted"),
  safeguard_N = sapply(signed_limit, safeguard_n),
  bucss_N = sapply(plans_1, row_value, term = "necessary_N"))

## ----safeguard-unbalanced-----------------------------------------------------
n_prior <- c(4, 100)
# Ordinary power of an independent t test with n per group at a standardized
# mean difference d.
power_ind <- function(n, d) {
  crit <- qt(.975, df = 2 * n - 2)
  delta <- d * sqrt(n / 2)
  (1 - pt(crit, df = 2 * n - 2, ncp = delta)) +
    pt(-crit, df = 2 * n - 2, ncp = delta)
}
# Safeguard power with the 60% limit read on 'df' and restated as a
# standardized mean difference by the factor 'to_d'.
safeguard_ind <- function(df, to_d) {
  limit <- uniroot(function(ncp) pt(3, df = df, ncp = ncp) - .80, c(0, 10),
                   tol = 1e-10)$root
  n <- 2
  while (power_ind(n, limit * to_d) < .80) n <- n + 1
  n
}
harmonic <- 2 * prod(n_prior) / sum(n_prior)
plan_unequal <- ss_buc_independent_t(t_observed = 3, n = n_prior,
                                     alpha_prior = 1, assurance = .80)
data.frame(
  prior_read_as = c("groups of 4 and 100, df 102", "7 per group, df 12",
                    "8 per group, df 14", "BUCSS, alpha_prior = 1"),
  necessary_n_per_group = c(
    safeguard_ind(sum(n_prior) - 2, sqrt(sum(1 / n_prior))),
    safeguard_ind(2 * floor(harmonic) - 2, sqrt(2 / floor(harmonic))),
    safeguard_ind(2 * ceiling(harmonic) - 2, sqrt(2 / ceiling(harmonic))),
    row_value(plan_unequal, "necessary_n_per_group")))

## -----------------------------------------------------------------------------
plan <- ss_buc_paired_t(t_observed = 3, N = 30, assurance = .80)
ncp <- plan$value[plan$term == "ncp_adjusted"]
ncp
audit <- ss_buc_sensitivity(plan, true_ncp = ncp, replications = 2000,
                            seed = 2017)
audit


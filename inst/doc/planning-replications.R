## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", fig.width = 7,
                      fig.height = 4.5)

## -----------------------------------------------------------------------------
library(BUCSS)

## -----------------------------------------------------------------------------
set.seed(2017)

true_d <- 0.2     # the true (small) population effect
n      <- 30      # participants per group in each study
G      <- 1000    # number of studies the field runs

studies <- replicate(G, {
  control   <- rnorm(n, mean = 0,      sd = 1)
  treatment <- rnorm(n, mean = true_d, sd = 1)
  tt <- t.test(treatment, control, var.equal = TRUE)
  # For equal n per group, Cohen's d = t * sqrt(2 / n).
  c(t = unname(tt$statistic), p = tt$p.value,
    d = unname(tt$statistic) * sqrt(2 / n))
})
studies <- as.data.frame(t(studies))
studies$published <- studies$p < .05

## -----------------------------------------------------------------------------
power.t.test(delta = true_d, n = n, sig.level = .05)$power
p_published <- mean(studies$published)
c(proportion = p_published,
  mcse       = sqrt(p_published * (1 - p_published) / G))

## -----------------------------------------------------------------------------
d_published <- studies$d[studies$published]
rbind(all       = c(mean = mean(studies$d),
                    mcse = sd(studies$d) / sqrt(G)),
      published = c(mean = mean(d_published),
                    mcse = sd(d_published) / sqrt(length(d_published))))

## ----echo = FALSE, fig.alt = "Scatter plot of the observed d in each of the 1000 simulated studies. The unpublished studies, drawn as open gray points, scatter around the true value of 0.2, shown as a dotted red line, and the dashed gray line for the mean of all 1000 studies lies almost on top of it. The published studies, drawn as filled black points, sit almost all above about 0.5, and the solid black line for their mean lies near 0.6, far above the truth."----
plot(seq_len(G), studies$d,
     pch = ifelse(studies$published, 19, 1),
     col = ifelse(studies$published, "black", "grey70"),
     cex = 0.6,
     ylim = c(min(studies$d), max(studies$d) + 0.65),
     xlab = "Study", ylab = "Observed d",
     main = "Publication Bias Inflates the Observed Effect")
abline(h = true_d, lty = 3, lwd = 2, col = "red")
abline(h = mean(studies$d), lty = 2, col = "grey40")
abline(h = mean(d_published), lwd = 2)
legend("top", bty = "n", cex = 0.9, ncol = 2,
       legend = c("published (p < .05)", "not published",
                  "true d = 0.2", "mean of all", "mean of published"),
       pch = c(19, 1, NA, NA, NA),
       lty = c(NA, NA, 3, 2, 1),
       lwd = c(NA, NA, 2, 1, 2),
       col = c("black", "grey70", "red", "grey40", "black"))

## -----------------------------------------------------------------------------
t_obs  <- 2.52
n_prior <- 25
d_face  <- t_obs * sqrt(2 / n_prior)
d_face

## -----------------------------------------------------------------------------
n_naive <- ceiling(power.t.test(delta = d_face, power = .80, sig.level = .05)$n)
n_naive

## -----------------------------------------------------------------------------
power.t.test(delta = 0.2, n = n_naive, sig.level = .05)$power

## -----------------------------------------------------------------------------
ss_buc_independent_t(t_observed = 2.52, n = 25,
                     alpha_prior = .05, alpha_planned = .05,
                     assurance = .50, desired_power = .80)

## ----error = TRUE-------------------------------------------------------------
try({
ss_buc_independent_t(t_observed = 2.52, n = 25,
                     alpha_prior = .05, alpha_planned = .05,
                     assurance = .80, desired_power = .80)
})

## -----------------------------------------------------------------------------
plan <- ss_buc_independent_t(t_observed = 2.52, n = 25, assurance = .50)
tidy(plan)
tidy(plan)$estimate

## -----------------------------------------------------------------------------
legacy <- suppressWarnings(ss.power.it(t.observed = 2.52, N = 50, assurance = .50))
legacy[[1]]   # necessary sample size per group


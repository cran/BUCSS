# Dependent t test.

#' Necessary sample size to reach desired power for a dependent t-test using an
#' uncertainty and publication bias correction procedure
#'
#' @description \code{ss.power.dt} returns the necessary per-group sample size
#'   to achieve a desired level of statistical power for a planned study using
#'   a dependent t-test, based on information obtained from a previous study.
#'   The effect from the previous study can be corrected for publication bias
#'   and/or uncertainty to provide a sample size that will achieve more accurate
#'   statistical power for a planned study, when compared to approaches that use
#'   a sample effect size at face value or rely on sample size only.
#'
#' @details Researchers often use the sample effect size from a prior study as
#'   an estimate of the likely size of an expected future effect in sample size
#'   planning. However, sample effect size estimates should not usually be used
#'   at face value to plan sample size, due to both publication bias and
#'   uncertainty.
#'
#'   The approach implemented in \code{ss.power.dt} uses the observed
#'   \eqn{t}-value and sample size from a previous study to correct the
#'   noncentrality parameter associated with the effect of interest for
#'   publication bias and/or uncertainty. This new estimated noncentrality
#'   parameter is then used to calculate the necessary per-group sample size to
#'   achieve the desired level of power in the planned study.
#'
#'   The approach uses the following likelihood function: \deqn{
#'   f_{L}[f_{S};\nu_{1},\nu_{2},\omega]=\frac{f_{F}[f_{S}|\nu_{1},\nu_{2},
#'   \omega]}{1-F_{F}[f_{crit}(1-\alpha_{S})|\nu_{1},\nu_{2},\omega]} } to
#'   select an appropriate estimated noncentrality parameter. The formula
#'   represents the density of a truncated noncentral \eqn{F}-distribution,
#'   where \eqn{f_{F}} is the probability density function of the noncentral
#'   \eqn{F}-distribution from the original study, \eqn{f_{S}} is the observed
#'   \eqn{F}-value for the original study, \eqn{F_{F}} is the cumulative
#'   distribution function indicating the probability that a noncentral
#'   \eqn{F}-distribution exceeds the critical value, \eqn{f_{crit}} is the
#'   critical \eqn{F}-value for \eqn{\alpha_{s}}, the specified
#'   \eqn{\alpha}-level of the original study, \eqn{\nu_{1}} and \eqn{\nu_{2}}
#'   are the degrees of freedom of the previous study, and \eqn{\omega} is the
#'   noncentrality parameter being estimated for sample size planning. The
#'   numerator is the likelihood of obtaining the observed previous study
#'   \eqn{F}-value given the noncentrality parameter. The denominator can be
#'   thought of as the power of the test given the specified noncentrality
#'   parameter. In the two-group case, this formula reduces to the density
#'   of a truncated noncentral \eqn{t}-distribution.
#'
#'   Assurance is the proportion of times that power will be at or above the
#'   desired level, if the experiment were to be reproduced many times. For
#'   example, assurance = .5 means that power will be above the desired level
#'   half of the time, but below the desired level the other half of the time.
#'   Selecting assurance = .5 (selecting the noncentrality parameter at the 50th
#'   percentile of the likelihood distribution) results in a median-unbiased
#'   estimate of the population noncentrality parameter and corrects for
#'   publication bias only. In order to correct for uncertainty, assurance > .5
#'   can be selected, which corresponds to selecting the noncentrality parameter
#'   associated with the (1 - assurance) quantile of the likelihood
#'   distribution.
#'
#'   If the previous study of interest is unpublished (e.g., a pilot study),
#'   \code{alpha.prior} can be set to 1 to indicate no publication bias.
#'   Alternative \eqn{\alpha}-levels can also be accommodated to represent
#'   differing amounts of publication bias. For example, setting
#'   \code{alpha.prior}=.20 would reflect less severe publication bias than the
#'   default of .05. In essence, setting \code{alpha.prior} at .20 assumes that
#'   studies with \eqn{p}-values less than .20 are published, whereas those with
#'   alrger \eqn{p}-values are not.
#'
#'   In some cases, the corrected noncentrality parameter for a given level of
#'   assurance will be estimated to be zero. This is an indication that the
#'   previous study's effect cannot be accurately estimated due to high levels
#'   of uncertainty and bias. When this happens, subsequent sample size planning
#'   is not possible with the chosen specifications. Two alternatives are
#'   recommended. First, users can select a lower value of assurance (e.g. .5
#'   instead of .8). Second, users can reduce the influence of publciation bias
#'   by setting \code{alpha.prior} at a value greater than .05. It is possible
#'   to correct for uncertainty only by setting \code{alpha.prior}=1 and
#'   choosing the desired level of assurance.
#'
#' @param t.observed Observed \eqn{t}-value from a previous study used to plan
#'   sample size for a planned study
#' @param N Total sample size of the previous study
#' @param alpha.prior Alpha-level \eqn{\alpha} for the previous study or the
#'   assumed statistical significance necessary for publishing in the field; to
#'   assume no publication bias, a value of 1 can be entered (correct for
#'   uncertainty only)
#' @param alpha.planned Alpha-level (\eqn{\alpha}) assumed for the planned study
#' @param assurance Desired level of assurance, or the long run proportion of
#'   times that the planned study power will reach or surpass desired level
#'   (assurance of .5 corrects for publication bias only; assurance > .5
#'   corrects for uncertainty)
#' @param power Desired level of statistical power for the planned study
#' @param step Value used in the iterative scheme to determine the noncentrality
#'   parameter necessary for sample size planning (0 < step < .01) (users should
#'   not generally need to change this value; smaller values lead to more
#'   accurate sample size planning results, but unnecessarily small values will
#'   add unnecessary computational time)
#'
#' @return Suggested per-group sample size for planned study
#'
#' @export
#' @import stats
#'
#' @examples
#' ss.power.dt(t.observed=3, N=40, alpha.prior=.05, alpha.planned=.05,
#' assurance=.80, power=.80, step=.001)
#'
#' @author Samantha F. Anderson \email{sander10@nd.edu},
#' Ken Kelley \email{kkelley@@nd.edu}
#'
#' @references Anderson, S. F., & Maxwell, S. E. (conditionally accepted).
#'   Addressing the 'replication crisis': Using original studies to design
#'   replication studies with appropriate statistical power. \emph{Multivariate
#'   Behavioral Research}.
#'
#'   Anderson, S. F., Kelley, K., & Maxwell, S. E. (under review). The illusion
#'   of intended statistical power: How to avoid underpowered studies via
#'   effective sample size planning.
#'
#'   Taylor, D. J., & Muller, K. E. (1996). Bias in linear model power and
#'   sample size calculation due to estimating noncentrality.
#'   \emph{Communications in Statistics: Theory and Methods, 25,} 1595-1610.


ss.power.dt <- function(t.observed, N, alpha.prior=.05, alpha.planned=.05, assurance=.80, power=.80, step=.001)
{
if(alpha.prior > 1 | alpha.prior <= 0) stop("There is a problem with 'alpha' of the prior study (i.e., the Type I error rate), please specify as a value between 0 and 1 (the default is .05).")
if(alpha.prior == 1) {alpha.prior <- .999 }
if(alpha.planned >= 1 | alpha.planned <= 0) stop("There is a problem with 'alpha' of the planned study (i.e., the Type I error rate), please specify as a value between 0 and 1 (the default is .05).")

if(assurance >= 1)
{
assurance <- assurance/100
}

if(assurance<0 | assurance>1)
{
stop("There is a problem with 'assurance' (i.e., the proportion of times statistical power is at or above the desired value), please specify as a value between 0 and 1 (the default is .80).")
}

if(power >= 1) power <- power/100

if(power<0 | power>1) stop("There is a problem with 'power' (i.e., desired statistical power), please specify as a value between 0 and 1 (the default is .80).")

if(missing(N)) stop("You need to specify a sample size (i.e., the number of pairs) used in the original study.")

if(N <= 1) stop("Your total sample size is too small")

DF <- N-1

NCP <- seq(from=0, to=100, by=step)

d.density <- dt(t.observed, df=DF, ncp=NCP)

value.critical <- qt(1-alpha.prior/2, df=DF)

if(t.observed <= value.critical) stop("Your observed t statistic is nonsignificant based on your specfied alpha of the prior study. Please increase 'alpha.prior' so 't.observed' exceeds the critical value")

area.above.crit <- 1 - pt(value.critical, df=DF, ncp=NCP)
area.other.tail <- pt(-1*value.critical, df=DF, ncp=NCP)
power.values <- area.above.crit + area.other.tail
area.above.t <- 1 - pt(t.observed, df=DF, ncp = NCP)
area.above.t.opp <- pt(-1*t.observed, df = DF, ncp = NCP)
area.area.between <- (area.above.crit - area.above.t) + (area.other.tail - area.above.t.opp)

TM <- area.area.between/power.values
TM.Percentile <- min(NCP[which(abs(TM-assurance)==min(abs(TM-assurance)))])

if(TM.Percentile==0) stop("The corrected noncentrality parameter is zero. Please either choose a lower value of assurance and/or a higher value of alpha for the prior study (e.g. accounting for less publication bias)")

if (TM.Percentile > 0)
{
Nrep <- 2
denom.df <- Nrep-1
diff <- -1
while (diff < 0 )
{
  criticalT <- qt(1-alpha.planned/2, df = denom.df)
powers1 <- 1 - pt(criticalT, df = denom.df, ncp = sqrt(Nrep/N)*TM.Percentile)
powers2 <- pt(-1*criticalT, df=denom.df, ncp = sqrt(Nrep/N)*TM.Percentile)
powers <- powers1 + powers2
diff <- powers - power
Nrep <- Nrep + 1
denom.df = Nrep - 1
}
repN <- Nrep - 1
}
return(repN)
}
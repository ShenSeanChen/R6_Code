# remotes::install_github("RaphaelS1/distr6", dependencies = TRUE)
remotes::install_github("RaphaelS1/distr6", "dev")
library('distr6')
library('R6')
# Playground
# exp <- Exponential$new(rate = 0.6)
# x <- seq(0,15,0.05)
# plot(exp$pdf(x))
# plot(exp$cdf(x))
# aa <- Exponential$new(rate=2)
# aa$kurtosis(excess=FALSE)
# aa$setParameterValue(list(rate=3))

###################################################
# Questions by Raphael
###################################################

###################################################
# 1. Find the number 1.96 from the Normal distribution
###################################################
norm <- Normal$new(mean = 0, sd = 1)
norm$pdf(1.96)
norm$cdf(1.96)

Normal

###################################################
# 2. Plot the pmf of a Truncated Binomial distribution between 3 and 7
#    - Note: Think about the original size of the distribution
###################################################
truncBinom <- TruncatedDistribution$new(Binomial$new(prob = 0.5, size = 10), lower = 3, upper = 7)
truncBinom$getParameterValue("prob")
x1 <- seq(0, 10, 1)
plot(x1, sapply(x1, truncBinom$pdf), pch = 20)
plot(x1, truncBinom$pdf(x1), pch = 20, ylab = 'Probability Mass')
plot(x1, sapply(x1, truncBinom$cdf))

# decorate(truncBinom, CoreStatistics)
# truncBinom$cdf(x1)

###################################################
# 3. Sample from a Normal distribution with precision 5, mean 42
###################################################
normPrec <- Normal$new(mean = 42, prec = 5)
hist(normPrec$rand(1:10000))
plot(normPrec$pdf(seq(40, 44, 0.02)), type = 'l')
# library(magrittr)
# normPrec %>% pdf(40:44)

###################################################
# 4. Construct an Exponential distribution with rate 2, update the parameters to get scale = 4
###################################################
exp <- Exponential$new(rate = 2)
exp$getParameterValue("rate")

exp$initialize(scale = 4)
exp$parameters()

exp$setParameterValue(list(rate = 1/4)) # scale is not settable
# Note: The current code suggests that scale.bool will be TRUE only when rate is NULL
exp$getParameterValue("scale") # not sure how to update 'scale' directly

###################################################
# 5. Implement the Bernoulli distribution ^ But with the functions written yourself!!!
###################################################
Bernoulli <- R6::R6Class("Bernoulli", inherit = Distribution, lock_objects = F)
dbinom(0,size = 10, p = 0.5)

# Error with 'Binomial'
ber <- Binomial$new(prob=0.5, size = 1)
ber$pdf(0) # Error

###################################################
# 6. Harder! Chi-squared!
###################################################
source('Distribution_Chisq.R')
Chisq_try <- Chisquared$new(df = 1)
# plot pdf
plot(Chisq_try$pdf(seq(0, 8, 0.1)), type = 'l', lwd = 1, xlab = 'x1', ylab = 'Density')
plot_more <- function(n) {
  lines(Chisq_try$setParameterValue(list(df = n+1))$pdf(seq(0, 8, 0.1)),
         lty = 1, lwd = 1, col = n+1)
}
sapply(1:5, plot_more)
legend("topright",
       c("df = 1", "df = 2", "df = 3", "df = 4", "df = 5", "df = 6"),
       fill=c("black", 2:6))

# plot cdf
plot(Chisq_try$cdf(seq(0, 8, 0.1)), type = 'l', lwd = 1, xlab = 'x1',
     ylab = 'Cumulative Probability', ylim = c(0,1))
plot_more <- function(n) {
  lines(Chisq_try$setParameterValue(list(df = n+1))$cdf(seq(0, 8, 0.1)),
        lty = 1, lwd = 1, col = n+1)
}
sapply(1:5, plot_more)
legend("bottomright",
       c("df = 1", "df = 2", "df = 3", "df = 4", "df = 5", "df = 6"),
       fill=c("black", 2:6))


###################################################
# 7. Create your own custom distribution using the Distribution function
###################################################
distrSean <- R6::R6Class("distrSean", inherit = Distribution, lock_objects = F)

###################################################
# 8. Find the pdf 4-norm of the Standard Normal Distribution
#    between 4 and 9 (find the decorator!) (edited)
###################################################

###################################################
# 9. Plot the cdf of the Mixture_Norm_Binom with weights (0.1, 0.9) distribution.
###################################################
mixNormBinom <- MixtureDistribution$new(distlist = list(Normal$new(mean=5,sd=2),
                                                        # Exponential$new(rate = 1/5)),
                                                        Binomial$new(prob = 0.5, size = 10)),
                                        weights = c(0.1, 0.9))
par(mfrow = c(1,2))
x1 <- seq(-2, 10, 0.5)
# plot(x1, sapply(x1, mixNormBinom$pdf), type = 'l', ylab = 'Density', main = 'PDF')
# plot(x1, sapply(x1, mixNormBinom$cdf), type = 'l', ylab = 'Prob', main = 'CDF')
plot(x1, sapply(x1, mixNormBinom$pdf), pch = 20,  ylab = 'Density', main = 'PDF')
plot(x1, sapply(x1, mixNormBinom$cdf), pch = 20,  ylab = 'Prob', main = 'CDF')
par(mfrow = c(1,1))

# my.list
# is.list(my.list)
# my.vec <- c(0.1,0.9)
# is.list(my.vec)

###################################################
# 10. Find the analytic expression for the anti-derivative of the cdf of the Exponential Distribution.
#     Do this by pen & paper and also using Mathematica
###################################################

expModify <- R6::R6Class("expModify", inherit = Exponential)
# expModify$set("public", "AntiDerr", NULL)
expModify$set("public", "antiDerrivative", function(x1) {
    # You struggled a lot here last time because
    # you did not realise 'rate' is not a public item in the object.
    # You need to call 'rate' like the following
    rate = self$getParameterValue("rate")
    x1 + exp(-(rate*x1))/rate
    # invisible(self)
  }
  )
expMod <- expModify$new(rate=2)
plot(expMod$antiDerrivative(seq(0, 20, 0.5)))
lines(expMod$antiDerrivative(seq(0, 20, 0.5)))


###################################################
# 11. The above for Laplace
###################################################
Laplace


N <- Normal$new(mean = 0, sd = 1)
plot(N, rep="hazard",range=c(-1,1), type="l") # Plots a line plot of hazard function over (-1,1)
plot(N, plots = 2) # Plots line plots (suggested default for AbsContDist) for the first two possible representations (e.g. pdf and cdf) then 'Press ENTER to continue' to see more plots
qqplot(N)
hist(N)



## Problems
#  1. How to plot cdf of truncated distributions properly (Q2)
#  2. How to plot pdf and cdf of mixture distributions properly (Q9)
#  3. Scale is not settable in Exponential
#     Note: The current code suggests that scale.bool will be TRUE only when rate is NULL

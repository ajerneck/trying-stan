##################################################################
## Code to try out Stan.                                        ##
## https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started ##
##################################################################

library(rstan)
library(ggplot2)
library(stringr)
library(plyr)
library(reshape2)
set_cppo('fast')

###############################
## Example 1: eight schools. ##
###############################

schools_code <- "
  data {
    int<lower=0> J; // number of schools
    real y[J]; // estimated treatment effects
    real<lower=0> sigma[J]; // s.e. of effect estimates
}
parameters {
    real mu;
    real<lower=0> tau;
    real eta[J];
}
transformed parameters {
    real theta [J];
    for (j in 1: J)
       theta[j] <- mu + tau * eta[j];
}
model {
    eta ~ normal(0, 1);
    y ~ normal(theta, sigma);
}
"
schools_dat <- list(J = 8,
                    y = c(28, 8, -3, 7, -1, 1, 18, 12),
                    sigma = c(15, 10, 16, 11, 9, 11, 10, 18)
                    )

## fit the model
fit <- stan(model_code = schools_code, data=schools_dat, iter=1000, chains=4)

## inspecting the fitted model
print(fit)
plot(fit)

########################################
## Bernoulli example from the manual. ##
########################################
## ~/stan-reference-2.2.0.pdf

priors <- list(
      peaked = cbind(a=5, b=5)
    , skewed = cbind(a=1, b=9)
    , diffuse = cbind(a=1, b=1)
    )

bern_data <- list(
    N = 10
    , y = c(1,1,1,1,1,0,0,0,0,0)
    )

dt <- 'data {
  int<lower=0> N;
  int<lower=0, upper=1> y[N];
}'
pt <- 'parameters {
  real<lower=0, upper=1> theta;
}'
mt <- 'model {
  theta ~ beta(%A, %B);
  y ~ bernoulli(theta);
}'

mts <- llply(priors, function(x, s) {
    s <- str_replace(s, '%A', x[,'a'])
    s <- str_replace(s, '%B', x[,'b'])
    s
}, mt)

codes <- lapply(mts, function(x) paste(dt, pt, x, sep='\n'))

## fit the models.
ms <- lapply(codes, function(x) stan(model_code=x, data=bern_data, iter=2000, chains=4))

## examine fitted models.

msd <- ldply(ms, function(x) {
    sims <- melt(data.frame(extract(x, permuted=TRUE, pars='theta'), varname='sim'))
    sums <- melt(summary(x)$summary)
    names(sums) <- c('variable','varname','value')
    rbind(sims, sums)
})

msd <- msd[msd$variable == 'theta',]

msdd <- dcast(.id ~ varname, data=msd[msd$varname != 'sim',])
names(msdd) <- str_replace(names(msdd), '%', 'pc')
names(msdd) <- str_replace(names(msdd), '(^[0-9])', 'b\\1')

ggplot(msd) + geom_density(aes(value, color=.id), data=msd[msd$varname == 'sim',]) + facet_wrap(~.id) + geom_vline(aes(xintercept=mean), linetype='dotted', data=msdd) + geom_segment(aes(x=b2.5pc, xend=b97.5pc, y=0, yend=0), data=msdd) + theme_bw()
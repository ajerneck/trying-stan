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

###################################################################
## Bernoulli example from the manual, but with different priors. ##
###################################################################
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

model_data <- function(x) {
    sims <- melt(data.frame(extract(x, permuted=TRUE), varname='sim'))
    sums <- melt(summary(x)$summary)
    names(sums) <- c('variable','varname','value')
    rbind(sims, sums)
}

msd <- ldply(ms, model_data)

model_summary <- function(x) {
    msdd <- dcast(.id ~ varname, data=x[x$varname != 'sim',])
    names(msdd) <- str_replace(names(msdd), '%', 'pc')
    names(msdd) <- str_replace(names(msdd), '(^[0-9])', 'b\\1')
    msdd
}

msd <- msd[msd$variable == 'theta',]
msdd <- model_summary(msd)

ggplot(msd) + geom_histogram(aes(value, fill=.id), data=msd[msd$varname == 'sim',]) + facet_wrap(~.id) + geom_vline(aes(xintercept=mean), linetype='dotted', data=msdd) + geom_segment(aes(x=b2.5pc, xend=b97.5pc, y=0, yend=0), data=msdd) + theme_bw()


##########################
## Logistic regression. ##
##########################

d <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")

head(d)

m <- glm(admit ~ gre, data=d, family=binomial(logit))
summary(m)

log_code <- '
data {
  int<lower=0> N;
  int<lower=0, upper=1> admit[N];
  vector[N] gre;
  vector[N] gpa;
}
parameters {
  vector[3] beta;
}
model {
  admit ~ bernoulli_logit(beta[1] + beta[2] * gre + beta[3] * gpa);
}
'
log_data <- list(
      N = nrow(d)
    , admit = d$admit
    , gre = d$gre
    , gpa = d$gpa
    )

m <- glm(admit ~ gre + gpa, data=d, family=binomial(logit))
summary(m)

bm <- stan(model_code=log_code, data=log_data, iter=4000, chains=4)
summary(bm)$summary

msd.l <- model_data(bm)
msd.l$.id <- 'unsure'

msd.l <- msd.l[msd.l$variable != 'lp__',]

msd.l$variable <- str_replace(msd.l$variable, '\\[', '.')
msd.l$variable <- str_replace(msd.l$variable, '\\]', '')
msd.l$variable <- factor(msd.l$variable)

msdd.l <- dcast(.id + variable ~ varname, data=msd.l[msd.l$varname != 'sim',], value.var='value')
names(msdd.l) <- str_replace(names(msdd.l), '%', 'pc')
names(msdd.l) <- str_replace(names(msdd.l), '(^[0-9])', 'b\\1')

ggplot(msd.l) + geom_histogram(aes(value, fill=.id), data=msd.l[msd.l$varname == 'sim',]) + facet_wrap(~variable) + geom_vline(aes(xintercept=mean), linetype='dotted', data=msdd.l) + geom_segment(aes(x=b2.5pc, xend=b97.5pc, y=0, yend=0), data=msdd.l) + theme_bw()

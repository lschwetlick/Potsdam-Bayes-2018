# sampling from posterior distribution

library('rjags')

coinflips = c(0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1)

# prepare data and init list 
jags.data = list('y' = coinflips,
                 'N' = length(coinflips)) 

jags.inits <- list(theta = 0.1)

# set up model
jags <- jags.model(file = 'd4-coinflip.jags',
                   data = jags.data,
                   inits = jags.inits,
                   n.chains = 1)
                   
# sample theta values
mcmc.samples <- coda.samples(model = jags, 
                        variable.names = c('theta'),
                        n.iter = 2000)
summary(mcmc.samples)
# plot samples
plot(mcmc.samples)

# nice kruschke plots with HDI
source("kruschke-utilities.R")
plotPost(mcmc.samples[,"theta"], xlim=c(0,1))


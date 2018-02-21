library('rjags')

#read data
coinflips = c(0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1)

# sampling from posterior distribution

# prepare data and init list 
jags.data = list(y = coinflips,
                 N = length(coinflips)) 

# two init values (one for each chain
fix.inits <- list(
      list(theta = 0.1),
      list(theta = 0.9))

# .. or random init values by an init function
#random.inits = function() {
#  list(theta = runif(2, min=0, max=1))
#}  

# set up model
jags <- jags.model(file = 'd4-coinflip.jags',
                   data = jags.data,
                   inits = fix.inits, 
                   n.chains = 2)
                   
# sample theta values
mcmc.samples <- coda.samples(model = jags, 
                        variable.names = c('theta'),
                        n.iter = 20000)

# get mean and std
summary(mcmc.samples)

# plotting and HDI (using coda package)
library('coda')
HPDinterval(mcmc.samples)
densplot(mcmc.samples, xlim=c(0,1))

# nice Kruschke plots
source("kruschke-utilities.R")
plotPost(mcmc.samples[,"theta"][1], xlim=c(0,1))
DbdaDensPlot(mcmc.samples)

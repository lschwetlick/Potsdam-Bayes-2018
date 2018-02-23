# Basic inference.
library('rjags')
library('coda')
source("kruschke-utilities.R")

# read data

data = rnorm(15, 100, 15)

# prepare data and init list 
jags.data = list(x = data,
                 n = length(data))

# .. init values
myinits <-  list(
  list(mu = 50, sigma = 0.1), 
  list(mu = 10, sigma = 0.9), 
  list(mu = 90, sigma = 0.5))

# set up model
jags <- jags.model(file = 'd5x-estimate_descriptive.jags',
                   data = jags.data,
                   inits = myinits, # or fix.inits
                   n.chains = 3,
                   n.adapt=500)

# burnin
update(jags, n.iter= 1000)

# sample theta values
mcmc.samples <- coda.samples(model = jags, 
                             variable.names = c( "mu", "sigma"),
                             n.iter = 20000, thining=20)

### parameter evaluation ###
# HDI (using coda package)
HPDinterval(mcmc.samples)

# nice Kruschke plots
mcmc.mat = as.matrix(mcmc.samples, chain=TRUE)
summary(mcmc.mat)

plotPost(mcmc.mat[,"mu"], xlim=c(70, 130))
plotPost(mcmc.mat[,"sigma"], xlim=c(0, 135))

# Basic inference.
library('rjags')

#read data
coinflips = c(0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1)

# sampling from posterior distribution

# prepare data and init list 
jags.data = list(y = coinflips,
                 N = length(coinflips)) 

# .. or random init values by an init function
random.inits = function() {
  list(theta = runif(1, min=0, max=1))
}

# set up model
jags <- jags.model(file = 'd4-coinflip.jags',
                   data = jags.data,
                   inits = random.inits,
                   n.chains = 2,
                   n.adapt=10000)


# burn-in
update(jags, n.iter= 5000)

# sample theta values
mcmc.samples <- coda.samples(model = jags, 
                        variable.names = c('theta'),
                        n.iter = 20000)

#### inspecting mcmc samples ####

# HDI (using coda package)
library('coda')
HPDinterval(mcmc.samples)

### plotting ####
# from coda package
densplot(mcmc.samples, xlim=c(0,1))
gelman.plot(mcmc.samples)

# nice Kruschke plots
source("kruschke-utilities.R")
plotPost(mcmc.samples[,"theta"][1], xlim=c(0,1))
DbdaDensPlot(mcmc.samples)
diagMCMC(mcmc.samples, parName = "theta")

#### further analysing with mcmc samples #####
# accessing raw samples of chain 1 by converting to matrix
samples_chain_1 = as.matrix(mcmc.samples[,"theta"][1])
summary(samples_chain_1)
mean(samples_chain_1)
sd(samples_chain_1)
# standart error
SE = sd(samples_chain_1)/sqrt(length(samples_chain_1))


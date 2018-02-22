# linear regression
library('rjags')
library('coda')
source("kruschke-utilities.R")

# read data
data = read.csv("Ginzberg.csv")

md = lm(data$depression ~ data$fatalism)
summary(md)

plot(data$fatalism, data$depression)
abline(reg = md)

# prepare data and init list 
jags.data = list(N = nrow(data), 
                 y = data$depression,
                 x = data$fatalism)

# initis random intits
random.init = function() {
  list(alpha = runif(1, -100, 100),
       beta = runif(1, -100, 100),
       lambda = rgamma(1, 0.01, 0.01))
}

# set up model
jags <- jags.model(file = 'linear_regression.jags',
                   data = jags.data,
                   inits = random.init, # or fix.inits
                   n.chains = 3,
                   n.adapt=500)

# burnin
update(jags, n.iter= 1000)

# sample theta values
mcmc.samples <- coda.samples(model = jags, 
                             variable.names = c( "alpha", "beta", "sigma"),
                             n.iter = 30000, thining=20)
diagMCMC(mcmc.samples)

### parameter evaluation ###
# HDI (using coda package)
HPDinterval(mcmc.samples)

mcmc.mat = as.matrix(mcmc.samples, chain=TRUE)
summary(mcmc.mat)

plotPost(mcmc.mat[,"beta"])
title("Beta")

plotPost(mcmc.mat[,"alpha"])
title("alpha")

plotPost(mcmc.mat[,"sigma"])
title("Sigma")

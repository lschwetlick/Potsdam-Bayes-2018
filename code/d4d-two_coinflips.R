# Basic inference.
library('rjags')

#read data
df <- read.csv( 'two_coins.csv')

# sampling from posterior distribution

# prepare data and init list 
jags.data = list('ya' = df$coin_a,
                 'yb' = df$coin_b,
                 'N' = nrow(df)) 

# # two init values (one for each chain)
#inits <- list(
#     list(theta_a = 0.1, theta_b = 0.1),
#     list(theta_a = 0.9, theta_b = 0.9))

# .. or random init values by an init function
inits = function() {
  list(theta_a = runif(1, min=0, max=1),
       theta_b = runif(1, min=0, max=1))
}
  
# set up model
jags <- jags.model(file = 'd4d-two_coinflips.jags',
                   data = jags.data,
                   inits = inits, 
                   n.chains = 2,
                   n.adapt=500)
                   
# burn-in
update(jags, n.iter= 5000)

# sample theta values
mcmc.samples <- coda.samples(model = jags, 
                        variable.names = c('theta_a', 'theta_b'),
                        n.iter = 10000)

# HDI (using coda package)
library('coda')
HPDinterval(mcmc.samples)
densplot(mcmc.samples, xlim=c(0,1), show.obs=FALSE)

### convergence diagnositc  ####
plot(mcmc.samples)
gelman.plot(mcmc.samples)


#### nice Kruschke plots ####
source("kruschke-utilities.R")
plotPost(mcmc.samples[,"theta_a"], xlim=c(0,1))
plotPost(mcmc.samples[,"theta_b"], xlim=c(0,1))

### convergence diagnositc  ####
diagMCMC(mcmc.samples, parName = "theta_a")
diagMCMC(mcmc.samples, parName = "theta_b")


#### differences between two coins ####

# make matrix
mcmc.mat = as.matrix(mcmc.samples)
mcmc.mat.diff = mcmc.mat[,"theta_a"] - mcmc.mat[,"theta_b"]
plotPost(mcmc.mat.diff, xlim=c(-1,1))

# Basic inference.
library('rjags')
library('coda')
source("kruschke-utilities.R")

#read data
df <- read.csv( 'coinflip_nested.csv')


# prepare data and init list 
NCoins = length(unique(df$coin))
jags.data = list('y' = df$value,
                 'coin' = as.numeric(df$coin), # convert to numbers
                 'NTotalFlips' = nrow(df),
                 'NCoins' = NCoins) 

# .. or random init values by an init function
random.inits = function() {
  list(theta = runif(NCoins, min=0, max=1))
}
  
# set up model
jags <- jags.model(file = 'd4e-coinflip_nested.jags',
                   data = jags.data,
                   inits = random.inits, 
                   n.chains = 2,
                   n.adapt=500)
                   
# burnin
update(jags, n.iter= 5000)

# sample theta values
mcmc.samples <- coda.samples(model = jags, 
                        variable.names = c('theta'),
                        n.iter = 10000)

# HDI (using coda package)
HPDinterval(mcmc.samples)

densplot(mcmc.samples, xlim=c(0,1), show.obs=FALSE)

### convergence diagnositc  ####
plot(mcmc.samples)
gelman.plot(mcmc.samples)
diagMCMC(mcmc.samples, parName = "theta[2]")


# nice Kruschke plots
mcmc.mat = as.matrix(mcmc.samples, chain=TRUE)
summary(mcmc.mat)

plotPost(mcmc.mat[,"theta[1]"], xlim=c(0,1))
plotPost(mcmc.mat[,"theta[2]"], xlim=c(0,1))
plotPost(mcmc.mat[,"theta[3]"], xlim=c(0,1))
plotPost(mcmc.mat[,"theta[2]"] -mcmc.mat[,"theta[1]"], xlim=c(-1,1))

plot(mcmc.mat[,"theta[2]"], mcmc.mat[,"theta[3]"], col="skyblue")

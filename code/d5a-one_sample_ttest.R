# Baysian t-test
library('dplyr')
library('rjags')
library('coda')
library('polspline')
source("kruschke-utilities.R")

data(sleep)
plot(extra ~ group, data = sleep)
g1 = filter(sleep, group==1)
g2 = filter(sleep, group==2)
diff = g1$extra - g2$extra
t.test(diff) 

#------------------ Bayesian t-test
jags.data = list(N = length(diff), 
                 x = diff)

# .. init values
random.init = function() {
  list(delta = rnorm(1,0,3), 
       sigmatmp = rnorm(1,0,1))
  }


# set up model
jags <- jags.model(file = 'd5c-one_sample_ttest.jags',
                   data = jags.data,
                   inits = random.init, 
                   n.chains = 3)
# burnin
update(jags, n.iter= 10000)
# sample theta values
mcmc.samples <- coda.samples(model = jags, 
                             variable.names = c( "delta", "mu", "sigma"),
                             n.iter = 100000, thining=5)

# EXAMINE THE RESULTS
mcmc.mat = as.matrix(mcmc.samples, chain=TRUE)
delta_samples= mcmc.mat[,"delta"]
HPDinterval(mcmc.samples)
# nice Kruschke plot of posterior
plotPost(delta_samples)

# --------------- Bayes Factor with Savage-Dickey method --------------- 
# based on logspline fit
fit.posterior <- logspline(delta_samples)
posterior <- dlogspline(0, fit.posterior) # this gives the pdf at point delta = 0
prior     <- dcauchy(0)                   # height of order--restricted prior at delta = 0
BF10      <- prior/posterior
BF10

#  Plot Prior and Posterior
par(cex.main = 1.5, mar = c(5, 6, 4, 5) + 0.1, mgp = c(3.5, 1, 0), cex.lab = 1.5,
    font.lab = 2, cex.axis = 1.3, bty = "n", las=1)
xlow  <- -3
xhigh <- 3
yhigh <- 4
Nbreaks <- 80
y       <- hist(delta_samples, Nbreaks, plot=F)
plot(c(y$breaks, max(y$breaks)), c(0,y$density,0), type="S", lwd=2, lty=2,
     xlim=c(xlow,xhigh), ylim=c(0,yhigh), xlab=" ", ylab="Density", axes=F) 
axis(1, at = c(-4,-3,-2,-1,0,1,2,3,4), lab=c("-4","-3","-2","-1","0", "1", "2", "3", "4"))
axis(2)
mtext(expression(delta), side=1, line = 2.8, cex=2)
#now bring in log spline density estimation:
par(new=T)
plot(fit.posterior, ylim=c(0,yhigh), xlim=c(xlow,xhigh), lty=1, lwd=1, axes=F)
points(0, dlogspline(0, fit.posterior),pch=19, cex=2)
# plot the prior:
par(new=T)
plot ( function( x ) dcauchy( x, 0, 1 ), xlow, xhigh, ylim=c(0,yhigh), xlim=c(xlow,xhigh), lwd=2, lty=1, ylab=" ", xlab = " ", axes=F) 
axis(1, at = c(-4,-3,-2,-1,0,1,2,3,4), lab=c("-4","-3","-2","-1","0", "1", "2", "3", "4"))
axis(2)
points(0, dcauchy(0), pch=19, cex=2)


# Data
z = 30 
N = 100  

# Prior  
Theta = seq(0, 1, len=300) 
#prior = dnorm(Theta, mean=.7, sd = .1)
prior = dbeta(Theta, 30, 100)

# Likelihood  
likelihood = Theta^z * (1-Theta)^(N-z) # Bernoulli likelihood  

# Posterior  
evidence = sum( likelihood * prior )  
posterior = likelihood * prior / evidence # Bayes' rule!  

### Plotting

par(mfrow=c(3,1)) 

# plot prior  
plot(Theta , prior,   
     type="h", lwd=1, main="Prior",  
     xlim=c(0,1) , xlab=bquote(theta),  
     #ylim=c(0,1.1*max(posterior)),   
     ylab=bquote(p(theta)),  
     col="red")  

# plot Likelihood  
plot( Theta , likelihood ,   
      type="h" , lwd=1 , main="Likelihood",  
      xlim=c(0,1) , xlab=bquote(theta),  
      #ylim=c(0,1.1*max(likelihood)),   
      ylab=bquote(paste("p(D|",theta,")")),  
      col="red" )  


# plot posterior  
plot( Theta , posterior,   
      type="h" , lwd=1 , main="Posterior" ,  
      xlim=c(0,1) , xlab=bquote(theta),  
      #ylim=c(0,1.1*max(posterior)),   
      ylab=bquote(paste("p(",theta,"|D)")),  
      col="red") 

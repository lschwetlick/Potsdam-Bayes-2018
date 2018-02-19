# Data
Data = c(1,1,1,0,0,0,0,0,0,0,0,0) # order doesn't matter  
z = sum( Data )  
N = length( Data )  

# Prior  
Theta = c(.25, .5, .75)  
prior = c(.25, .5, .25)  

# Likelihood  
likelihood = Theta^z * (1-Theta)^(N-z) # Bernoulli likelihood  

# Posterior  
evidence = sum( likelihood * prior )  
posterior = likelihood * prior / evidence # Bayes' rule!  

print(posterior)  

### Plotting

# plot prior  
plot(Theta , prior,   
     type="h", lwd=10, main="Prior",  
     xlim=c(0,1) , xlab=bquote(theta),  
     ylim=c(0,1.1*max(posterior)),   
     ylab=bquote(p(theta)),  
     col="skyblue")  

# plot Likelihood  
plot( Theta , likelihood ,   
      type="h" , lwd=10 , main="Likelihood",  
      xlim=c(0,1) , xlab=bquote(theta),  
      ylim=c(0,1.1*max(likelihood)),   
      ylab=bquote(paste("p(D|",theta,")")),  
      col="skyblue" )  


# plot posterior  
plot( Theta , posterior,   
      type="h" , lwd=10 , main="Posterior" ,  
      xlim=c(0,1) , xlab=bquote(theta),  
      ylim=c(0,1.1*max(posterior)),   
      ylab=bquote(paste("p(",theta,"|D)")),  
      col="skyblue") 

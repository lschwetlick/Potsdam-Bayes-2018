# Install required packages and test jags installation
# Course: "Introduction to Bayesian statistic with R"
#
# O. Lindemann

packages = c("rjags", "runjags", "ggplot2", "coda", "dplyr", "afex")
have = packages %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( packages[!have] ) }

# test jags installation
library("runjags")
testjags()

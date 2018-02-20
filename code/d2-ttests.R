library("dplyr")
library("BayesFactor")

# paired t-test, 
data(sleep)
help(sleep) # info about the data
plot(extra ~ group, data = sleep)
g1 = filter(sleep, group==1)
g2 = filter(sleep, group==2)
t.test(g1$extra, g2$extra, paired = T)
t.test(g1$extra-g2$extra) # does the same

# Bayesian -t-test
bf = ttestBF(g1$extra-g2$extra)
samples = posterior(bf, iterations = 2000)
plot(samples[,1:2])


# independet samples t-test
data("chickwts")
plot(weight ~ feed, data=chickwts)

g1 = filter(chickwts, feed=='linseed')
g2 = filter(chickwts, feed=='horsebean')
t.test(g1$weight, g2$weight, var.equal=T, data=Kitchen_Rolls)

ttestBF(g1$weight, g2$weight, paired = F)


# paired t-test Kitchen Roll
##### import Kictchen_Rolls data ####
# library("data.tables")
# kitchen_rolls = fread("https://raw.githubusercontent.com/lindemann09/Potsdam-Bayes-2018/master/code/data/kitchen_rolls.csv")

t.test(mean_NEO ~ Rotation, var.equal=T, data=kitchen_rolls)

g1 = filter(kitchen_rolls, Rotation=='clock')
g2 = filter(kitchen_rolls, Rotation=='counter')
bf = ttestBF(g1$mean_NEO, g2$mean_NEO, paired = F, var.equal=T)
1/bf

bf2 = ttestBF(g1$mean_NEO, g2$mean_NEO, nullInterval = c(-Inf, 0))
bf2
1/bf2[2]

bf_all = c(bf, bf2)
plot(bf_all)

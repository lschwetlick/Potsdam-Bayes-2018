library("dplyr")
library("BayesFactor")
library("afex")

data("ToothGrowth")
# frequentist AVOVA (within - subjects)
ToothGrowth$dose = factor(ToothGrowth$dose)
ToothGrowth$ID = c(1:nrow(ToothGrowth))  # make ID variable

aov_ez(id="ID", dv="len", between=c("supp", "dose"), 
       anova_table=c("pes"), data=ToothGrowth)

bf = anovaBF(len ~ supp*dose, data=ToothGrowth)
bf

plot(bf)

bf[4]/bf[3]
bf/bf[2]
plot(bf[3:4]/bf[2])

bf = anovaBF(len ~ supp*dose, data=ToothGrowth, whichModels="top")
bf


bfMainEffects = lmBF(len ~ supp + dose, data = ToothGrowth)
bfInteraction = lmBF(len ~ supp + dose + supp:dose, data = ToothGrowth)
## Compare the two models
bf = bfInteraction / bfMainEffects
bf






# frequentist AVOVA (within - subjects)
data("puzzles")
#summary(aov(RT ~ shape*color + Error(ID/(shape*color)), data=puzzles))
aov_ez(id="ID", dv="RT", within=c("shape", "color"), 
       anova_table=c("pes"), data=puzzles)

bf = anovaBF(RT ~ shape*color + ID, data = puzzles, 
             whichRandom="ID")
bf

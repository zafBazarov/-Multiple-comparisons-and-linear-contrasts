##############################
## DOUGHNUTS exercisis
##########################
library("emmeans") # Provides: emmeans, test, contrast
library("multcomp") # Provides: cld
library("dplyr") # Provides: summarise

# data

amount <- c ( 64, 72, 68, 77, 56, 95,  # Aldi
              78, 91, 97, 82, 85, 77,  # Lidl
              75, 93, 78, 71, 63, 76,  # Mazaola
              55, 66, 49, 64, 70, 68)  # Palmin

type <- factor( c( rep("Aldi", times=6), rep("Lidl", times=6),
                    rep("Mazaola", times=6), rep("Palmin", times=6)))

doughnutData <- data.frame(fattype=type,
                         absorption=amount)


write.table(doughnutData, "doughnutsMpcData.csv")

doughnutData <- read.table(file="doughnutsMpcData.csv",  # name of input file
                  dec = ",",              # decimal separator
                  sep = ";",              # separator between values
                  header = T)             # data have headers
doughnutData

str(doughnutData)

attach(doughnutData)

# ANOVA

model.1 <- aov ( absorption ~ fattype, data=doughnutData)
summary (model.1)

# ANOVA test shows us that p-value is smaller than 0.05. For this reason,
# we reject H0 and assume at least one factor level has a different mean. 
# we have to find which factor level has a different mean. 

# Means and standard errors
###############################

# we calculate emmeans.
# Estimated marginal means (EMMs), a.k.a. least-squares means, are predictions 
# on a reference grid of predictor settings, or marginal averages thereof.

emm.f <- emmeans ( model.1, ~fattype ) # Means of factor levels

emm.f


# To calculate mpc we need to find sem and sed

# we need mean of squares (Mean Sq) of our model.1 data.
summary(model.1) [[1]] [2,3]

# Mean Sq = 100.9

# sem = standard error of treatment but we need a replication number

# replication number finding command
str(doughnutData)

(sem <- sqrt (100.9/5))

(sed <- sqrt (100.9/5) * sqrt(2))

# Tukey test
alpha <- 0.05
(hsd <- qtukey (1 - alpha, 5, 20) * sem)

(tukey.test.statistic <- 19.01/sem)
(p.tukey <- ptukey(tukey.test.statistic, nmeans= 5,
                   df=20, lower.tail = FALSE))

# The Tukey test for all pairwise comparisons
test( contrast (emm.f, "pairwise"),
      adjust = "tukey")

cld( emm.f, adjust = "tukey")

# Least significant difference (LSD) on basis of t-distribution
alpha <- 0.05
(lsd <- qt( 1- alpha/2, 20) * sed)
rm(list=ls())

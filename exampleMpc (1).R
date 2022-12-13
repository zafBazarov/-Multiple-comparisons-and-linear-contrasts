## 1.Pairwise comparisons
#########################

## To perform our analysis, we need to load these packages.

library("emmeans") # Provides: emmeans, test, contrast
library("multcomp") # Provides: cld
library("dplyr") # Provides: summarise

install.packages("emmeans")
install.packages("multcomp")
install.packages("dplyr")

## This time, the information is provided. 
## We need to just upload it to our working directory.

dueng <- read.table ( "v14-u22-01.csv", header=TRUE, sep=";", dec=",",
                      stringsAsFactors = TRUE)

## this command helps us to get brief information about our data
str (dueng)

#############################################################
### There are three types of information in our data: 
#### fertilizer, replication, and yield.
#########################################################

# we create a box plot for our data

boxplot( yield ~ fert, data=dueng )

## After that we need to calculate discreptive statistics
dueng %>% group_by(fert) %>%
  summarise ( n=n(), mean=mean(yield), var=var(yield),
              min=min(yield), max=max(yield))

## Analysis of variance ANOVA
# Global test of the ANOVA

model.1 <- aov ( yield ~ fert, data=dueng)
summary (model.1)
########################################################
# The data shows that the p-value is smaller than 0.05. 
# For this reason, we reject H0 and assume at least 
# one factor level has a different mean. 
# Which factor level has a different mean?
#########################################################

# Calculation of means and respective standard errors with the emmeans command.

emm.f <- emmeans ( model.1, ~fert ) # Means of factor levels
emm.f

# After that, we compare descriptive statistics results, and the means on the 
# tables are the same because our data is balanced.
# On unbalanced data, emmeans produces better results.

# We can take mean od squares from our summary (model.1) command, which was used before.

summary(model.1)[[1]][2,3]

# We can find n (the replication number) with this command.
str (dueng)

# sem
( sem <- sqrt(140.9447/5) ) # Standard error of a mean

# sed
( sed <- sqrt(140.9447/5) * sqrt(2) ) # Standard error of a difference between means

# sem = Standard error of a treatment mean
# sed = Standard error of difference of treatment means

# The value 140:9447 comes from the analysis of variance. 
# It is the residual variance.The residual variance is divided 
# by 5 because of the 5 replications. # Note that the equation we use 
#here is only valid if the data are balanced! If the treatment groups
# have different sample sizes, you need to use a different equation and 
# the SED will be different depending on which comparison you make 
# because of the different number of replications. Here, we only have 
# one SED for all comparisons.

# We need to do LSD to use the t-test.
alpha <- 0.05
error.df <- 20
( lsd <- qt(1-alpha/2, error.df) * sed)
( lsd <- qt(1-alpha/2, 20) * sed)

# This (15.6625) is the minimal distance between two emmenes of similar factor levels.

# emmeans of CCr-2 and Man.

31.6-51.6

# LSD betwen them is -20.

# t-test calculation
(t.test.statistic <- 19.994/sed)

# p-value
(p.t <- 2*pt (abs (-2.663), lower.tail = FALSE, df=20))

# t-tests for all pairwise comparisons of factor levels:

test (contrast(emm.f, "pairwise"), 
      adjust = "none")

# The p-value indicates that this difference has a 0.0149% chance
# of being observed. The factor levels are not similar.

# Least significant difference on basis of the Tukey-distribution

alpha <- 0.05
(hsd<- qtukey(1-alpha, 5, 20) *sem)
# Also for the HSD alpha is not divided by 2. Because the Tukey distribution
# is one sided so there is no need to divided by 2.

# Tukey test:
(tukey.test.statistic <- 19.994/sem)
(p.tukey <- ptukey (tukey.test.statistic, nmeans=5, df=20, lower.tail = FALSE))

# The tukey test for all pairwise comparisons:

test( contrast( emm.f, "pairwise"),
      adjust = "tukey")


# Compact letter display:

# The result of pairwise comparisons can be displayed in a compact letter display.
# Treatments which are not significantly different are in same group. They
# get the same letter or number.

cld (emm.f, adjust = "tukey")

# the most important part is the groups 

# CCr-1, Min-A, Min-B, are in the same group because they are not 
# significantly different from each other.


## Dunnett -Test
#################################
# comparisons with a control, adjustment for 
# multiple comparisons

levels( dueng$fert)
test( contrast( emm.f, "trt.vs.ctrl", ref=3),
      side= "<",
      adjust = "mvt") # makes use of the multivariate t distribution
# results are not exactly repeatable



# 2 Linear contrasts
###########################
# No adjustment necessary.
# Check the order of the factor levels first!

# This command helps us to choose what treatment we need.

levels( dueng$fert)

# The next step is to create a list

C1 <- list( "CC-1 vs CCr-2"  = c (1, -1, 0, 0, 0),
            "Min-A vs Min-B" = c (0, 0, 0, 1, -1),
            "Org vs Min"     = c (2, 2, 2, -3, -3),
            "CCr vs Man"     = c (1, 1, -2, 0, 0) )
# We multiply all factor levels with each other. If the result is 0, 
# that is orthogonal; otherwise, it is non-orthogonal.

c (1, -1, 0, 0, 0) %*% c (0, 0, 0, 1, -1)
c (2, 2, 2, -3, -3) %*% c (1, 1, -2, 0, 0)

# We don't need adjustment if the data is orthogonal.
test ( contrast (emm.f, C1), 
       adjust = "none")

# Also, when doing an orthogonal contrast you are limited
# to k-1 comparison. k is the number of treatments (=fertilizers)
# So as we have 5 fertilizers we can only put 4 comparisons in the list

# Manure vs other fertilizers (non orthogonal contrast)
# Hochberg adjustment

C2 <- list("Man vs CCr1" = c (-1, 0, 1, 0, 0),
           "Man vs CCr2" = c (0, -1, 1, 0, 0),
           "Man vs Min-A" = c (0, 0, 1, -1, 0),
           "Man vs Min-B" = c (0, 0, 1, 0, -1) )
test ( contrast ( emm.f, C2 ),
       adjust="hochberg")

# Two standards / three treatments
C3 <- list ( " Min-A vs CCr1" = c(-1, 0, 0, 1, 0 ) ,
             " Min-A vs CCr2" = c( 0,-1, 0, 1, 0 ) ,
             " Min-A vs Man " = c( 0, 0,-1, 1, 0 ) ,
             " Min-B vs CCr1" = c(-1, 0, 0, 0, 1 ) ,
             " Min-B vs CCr2" = c( 0,-1, 0, 0, 1 ) ,
             " Min-B vs Man " = c( 0, 0,-1, 0, 1 ) )
test ( contrast ( emm.f, C3 ),
       adjust="fdr")
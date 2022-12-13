## Carrots example
#########################3

## To perform our analysis, we need to load these packages.

library("emmeans") # Provides: emmeans, test, contrast
library("multcomp") # Provides: cld
library("dplyr") # Provides: summarise

install.packages("emmeans")
install.packages("multcomp")
install.packages("dplyr")


# data

amount <- c (28.3, 33.4, 29.5, 28.9, 33.2,
             24.1, 24.6, 25.7, 23.5, 26.0,
             29.6, 30.5, 31.2, 35.4, 34.5,
             27.3, 28.1, 28.6, 27.0, 26.8) 

variety <- factor ( c( rep("Mokum", times=5), rep("Julia", times=5), 
                        rep("Neptun", times=5), rep("Bolero", times=5))
                    )

production <- data.frame (carrotType= variety,
                           yield = amount                           ) 

# boxplot

attach(production)

boxplot( yield ~ carrotType, data = production)

# descriptive statistic

production %>% group_by(carrotType) %>%
  summarise( n= n(), mean= mean(yield), var= var(yield)  )

# ANOVA

model1 <- aov (yield ~ carrotType)

summary(model1)

# means and standard errors

emm.f <- emmeans( model1, ~ carrotType)

emm.f

summary( model1) [[1]] [2,3]

# sem
(sem <- sqrt ( 3.559/4)) # Standard error of a mean

# sed 
(sed <- sqrt (3.559/4) * sqrt(2)) #Standard error of a difference between means


# Least significant difference on basis of the t-distribution

alpha <- 0.05
error.df <- 20
( lsd <- qt(1-alpha/2, error.df) * sed)

# Least significant difference on basis of the Tukey distribution


# Honestly significant difference (HSD) or Tukey test

( hsd <- qtukey( 1-alpha, 4, error.df) * sem)

# t-test
test ( contrast ( emm.f, "pairwise"),
       adjust="none")

# this command shows us different factor levels by groups
cld ( emm.f, adjust="tukey")

# Orthogonal contrast
levels ()


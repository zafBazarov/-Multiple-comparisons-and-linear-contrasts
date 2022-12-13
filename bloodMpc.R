###
## Blood
################################

## To perform our analysis, we need to load these packages.

library("emmeans") # Provides: emmeans, test, contrast
library("multcomp") # Provides: cld
library("dplyr") # Provides: summarise

# data

time <- c (62, 67, 65, 64,
             62, 60, 63, 59,
             56, 62, 60, 61, 
             68, 66, 71, 67) 

factors <- factor ( c( rep("A", times=4), rep("B", times=4), 
                       rep("C", times=4), rep("D", times=4))  )

bloodData <- data.frame (additive = factors,
                         coagulationTime = time)

write.table(bloodData, "bloodData.csv") # variable name, "filename"

# boxplot

boxplot( coagulationTime ~ additive,
         range = 0,
         data = bloodData)

# descriptive statistics

bloodData %>% group_by(additive) %>%
  summarise( n= n(), mean= mean(coagulationTime), var= var(coagulationTime),
             min = min(coagulationTime), max= max(coagulationTime))

# ANOVA
attach(bloodData)

model1 <- aov ( coagulationTime ~ additive )

summary (model1)

# means and standard errors

emm.f <- emmeans( model1, ~ additive)

emm.f

summary( model1) [[1]] [2,3]

# Dunnett test

levels( bloodData $additive)

test( contrast( emm.f, "trt.vs.ctrl", ref=4), 
      side = "<",
      adjust = "mvt")

# Orthogonal contrast
C1 <- list( "A vs D" = c(1, 0, 0, -1),
            "B vs D" = c(0, 1, 0, -1),
            "C vs D" = c(0, 0, 1, -1) )
C1

str(C1)

# adjustment
levels( bloodData $additive)

test( contrast( emm.f, C1,
                adjust = "hochberg") )

      
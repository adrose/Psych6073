## Load library(s)
library(simr)
library(pwr)

## Load data
in.data <- read.csv('./Data/crimeRate.csv')

## Create a cor matrix
cor.mat <- cor(in.data)

## create standardized data
in.data.std <- scale(in.data)[,]
in.data.std <- as.data.frame(in.data.std)

## Train the model
mod <- lm(crate ~ ., data=in.data.std)

## Now do the power analyses
# First calculate the sample size needed to do a fixed effects model with a multiple cor value of .4
# alpha = .05; power is .8; predictors will be 3 and then 4
# This is with 3 predictors
power.est.fe.3 <- pwr.f2.test(u=3, v=NULL, f2=.16/(1-.16), sig.level = .05, power = .8)
# THis is with 4 predictors
power.est.fe.4 <- pwr.f2.test(u=4, v=NULL, f2=.16/(1-.16), sig.level = .05, power = .8)
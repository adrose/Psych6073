# ---- load-packages --------------------------------------------------------
library("tidyverse")
library("knitr")
library("kableExtra")
library("reshape2")
library("sjstats")
library("ppcor")
library("corpcor")
library("car")
library("dominanceanalysis")

# ---- print-da --------------------------------------------------------
## load data
tob.dat <- read.csv("../Data/crimeRate.csv")

## Now run a dominance analysis... although I am not sure the outcome of interest
mod.1 <- lm(crate~., tob.dat)
da.1 <- dominanceAnalysis(mod.1)

## Now obtain the DA table
sum.da.1 <- summary(da.1)
# Now isolate the table of interest
out.table <- sum.da.1$r2$summary.matrix
kable(out.table)
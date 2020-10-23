# ---- load-packages --------------------------------------------------------
library("tidyverse")
library("knitr")
library("kableExtra")
library("reshape2")
library("lavaan")
library("semPlot")
library("dagitty")

# ---- helpful-links --------------------------------------------------------
# http://biostat.mc.vanderbilt.edu/wiki/pub/Main/ContinuingEdu/CTSaunders_CausalDiagrams.pdf

# ---- draw-confound --------------------------------------------------------
m1 <- dagitty("dag {
  C -> X
  C -> Y
}")
plot(m1)
# ---- draw-collider --------------------------------------------------------
m2 <- dagitty("dag {
  X -> C
  Y -> C
}")
plot(m2)

# ---- draw-4-a --------------------------------------------------------
m2 <- dagitty("dag {
  Race -> SES
  SES -> Legal Defense
  Race -> Legal Defense
}")
plot(m2)

# ---- draw-4-b --------------------------------------------------------
m2 <- dagitty("dag {
  SES -> Legal Defense
  Race -> Legal Defense
}")
plot(m2)

# ---- declare-model-orig --------------------------------------------------------
lower <- '
1.00
0.49 1.00
0.53 0.57 1.00 
0.49 0.46 0.48 1.00 
0.51 0.53 0.57 0.57 1.00 
0.33 0.30 0.31 0.24 0.38 1.00'

mod.cov <- getCov(lower, names = c("x1", "x2", "x3", "x4", "x5", "x6"), sds=NULL)

model <- '
x1 ~~ x2
x4 ~ x1
x5 ~ x2
x3 ~ x4
x3 ~ x5
x6 ~ x3
'

model.fit <- sem(model, sample.cov = mod.cov, sample.nobs = 100000)

val1 <- fitted(model.fit)$cov[c("x1","x2","x3","x4","x5","x6"),c("x1","x2","x3","x4","x5","x6")] %>% round(., 2)

fitted(model.fit)$cov[c("x1","x2","x3","x4","x5","x6"),c("x1","x2","x3","x4","x5","x6")] %>% 
  round(., 2) %>% 
  kbl(caption = "Equivalent model predicted covariance matrix (Model Original)") %>% 
  kable_classic_2(full_width = F) %>% 
  kableExtra::kable_styling(latex_options = "hold_position")

semPlot::semPaths(model.fit)

g1 <- dagitty('dag {
  X1 -> X4
  X1 <-> X2
  X2 -> X5
  X3 -> X6
  X4 -> X3
  X5 -> X3
}')

impliedConditionalIndependencies(g1)

# ---- declare-model-equi --------------------------------------------------------
lower <- '
1.00
0.49 1.00
0.53 0.57 1.00 
0.49 0.46 0.48 1.00 
0.51 0.53 0.57 0.57 1.00 
0.33 0.30 0.31 0.24 0.38 1.00'

mod.cov <- getCov(lower, names = c("x1", "x2", "x3", "x4", "x5", "x6"), sds=NULL)

model <- '
x1 ~~ x2
x1 ~ x4
x5 ~ x2
x3 ~ x4
x3 ~ x5
x6 ~ x3
'

model.fit <- sem(model, sample.cov = mod.cov, sample.nobs = 100000)
model.fit2 <- model.fit

val2 <- fitted(model.fit)$cov[c("x1","x2","x3","x4","x5","x6"),c("x1","x2","x3","x4","x5","x6")] %>% round(., 2)

fitted(model.fit)$cov[c("x1","x2","x3","x4","x5","x6"),c("x1","x2","x3","x4","x5","x6")] %>% 
  round(., 2) %>% 
  kbl(caption = "Equivalent model predicted covariance matrix (Model Equivalent)") %>% 
  kable_classic_2(full_width = F) %>% 
  kableExtra::kable_styling(latex_options = "hold_position")

## Now plot it
semPlot::semPaths(model.fit2)

g2 <- dagitty('dag {
  X4 -> X1
  X1 <-> X2
  X2 -> X5
  X3 -> X6
  X4 -> X3
  X5 -> X3
}')

impliedConditionalIndependencies(g1)

# ---- dagitty-declare-model-equi --------------------------------------------------------



# ---- q-10 --------------------------------------------------------
# First load data
in.data <- read.delim('./Data/New Text Document.txt', sep= ' ', header = T)
## First run the naive analysis
model.naive <- glm(remission ~ treat, data = in.data, family=binomial())

## Now run a propensity score model
model.prop <- glm(treat ~ sex + age + spouse + work + phg1, data = in.data, family=binomial())
in.data$groupProb <- predict(model.prop)
model.prop2 <- glm(remission ~ treat + groupProb, data=in.data, family=binomial())

## Now run a model with covariates
model.cov <- glm(remission ~ treat + sex + age + spouse + work + phg1, data=in.data, family=binomial())

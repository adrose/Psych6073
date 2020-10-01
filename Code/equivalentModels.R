# ---- load-packages --------------------------------------------------------
library("tidyverse")
library("knitr")
library("kableExtra")
library("reshape2")
library("lavaan")
library("semPlot")

# ---- declare-model-orig --------------------------------------------------------
lower <- '
 1.00
  -.21   1.00
  -.18   0.56  1.00
  -.19   0.65   0.50   1.00
 -.09  0.44  0.36  0.50  1.00'

mod.cov <- getCov(lower, names = c("a", "b", "c", "d", "e"), sds=NULL)

model <- '
b ~ a
c ~ a
d ~ b
e ~ b
d ~ c
e ~ c
e ~ d
'

model.fit <- sem(model, sample.cov = mod.cov, sample.nobs = 50)

## Now plot it
semPlot::semPaths(model.fit)


## Now obtain the predicted cov mat
fitted(model.fit)$cov[c("a","b","c","d","e"),c("a","b","c","d","e")] %>% 
  round(., 2) %>% 
  kbl(caption = "Equivalent model predicted covariance matrix") %>% 
  kable_classic_2(full_width = F) %>% 
  kableExtra::kable_styling(latex_options = "hold_position")


# ---- declare-model-equiv --------------------------------------------------------
lower <- '
 1.00
  -.21   1.00
  -.18   0.56  1.00
  -.19   0.65   0.50   1.00
 -.09  0.44  0.36  0.50  1.00'

mod.cov2 <- getCov(lower, names = c("a", "b", "c", "d", "e"), sds=NULL)

model2 <- '
b ~ a
a ~ c
d ~ b
e ~ b
d ~ c
e ~ c
e ~ d
'

model.fit2 <- sem(model2, sample.cov = mod.cov2, sample.nobs = 50)




## Now plot it
semPlot::semPaths(model.fit2)


## Grab the model cov
fitted(model.fit2)$cov[c("a","b","c","d","e"),c("a","b","c","d","e")] %>%
  round(., 2) %>% 
  kbl(caption = "Equivalent model predicted covariance matrix") %>% 
  kable_classic_2(full_width = F) %>% 
  kableExtra::kable_styling(latex_options = "hold_position")



# ---- daggity-model-orig --------------------------------------------------------
testImplications <- function( covariance.matrix, sample.size ){
  library(ggm)
  tst <- function(i){ pcor.test( pcor(i,covariance.matrix), length(i)-2, sample.size )$pvalue }
  tos <- function(i){ paste(i,collapse=" ") }
  implications <- list(c("A","D","B","C"),
                       c("A","E","B","C"),
                       c("B","C","A"))
  data.frame( implication=unlist(lapply(implications,tos)),
              pvalue=unlist( lapply( implications, tst ) ) )
  
}




# ---- daggity-model-equiv --------------------------------------------------------
testImplications <- function( covariance.matrix, sample.size ){
  library(ggm)
  tst <- function(i){ pcor.test( pcor(i,covariance.matrix), length(i)-2, sample.size )$pvalue }
  tos <- function(i){ paste(i,collapse=" ") }
  implications <- list(c("A","D","B","C"),
                       c("A","E","B","C"),
                       c("B","C","A"))
  data.frame( implication=unlist(lapply(implications,tos)),
              pvalue=unlist( lapply( implications, tst ) ) )
  
}
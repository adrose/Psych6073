## read the data
in.data <- read.csv("./Data/rm hw.csv", na='.')


## Now train a MANOVA (emulate a proc glm statement)
mod.1 <- manova(cbind(fev11h, fev12h, fev13h, fev14h, fev15h, fev16h, fev17h, fev18h) ~ drug, data=in.data)


## Set orthogonal contrasts.
op <- options(contrasts = c("contr.helmert", "contr.poly"))

## Fake a 2nd response variable
npk2 <- within(npk, foo <- rnorm(24))
( npk2.aov <- manova(cbind(yield, foo) ~ block + N*P*K, npk2) )
summary(npk2.aov)

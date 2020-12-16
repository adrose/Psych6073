## This code will be used for the final
library(reshape2)
library(rstatix)
## Question 4 code here
# read in the data
in.dat.wide <- read.csv("./Data/set1.csv")
# convert factors to factors
in.dat.wide$subj <- factor(in.dat.wide$subj)
in.dat.wide$group <- factor(in.dat.wide$group)
in.dat.long <- melt(in.dat.wide, id.vars = c("subj", "group", "therapy"))
write.csv(in.dat.long, "./Data/set1Long.csv", quote=F, row.names = F)
# run the model
rm.anova <- anova_test(data = in.dat.long, dv = value, wid = subj, between = therapy, within = variable)

## Now run a mixed effects mod
mod.mixed <- lmerTest::lmer(value ~ variable*therapy + (1|subj), data=in.dat.long)


## Question 5 code here:
# read data
in.dat <- read.csv("./Data/set2.csv")
# Now calculate the correlations using complete cases
cor(in.dat[,-1], use='complete')
cor(in.dat[,-1], use='na.or.complete')
cor(in.dat[,-1], use='pairwise.complete.obs') ## This is the correct one to use when you want to get pariwise complete cases!

## Now try the EM method?
library(norm)
pre <- prelim.norm(as.matrix(in.dat[,-1]))
thetahat <- em.norm(pre)
getparam.norm(pre,thetahat, corr=T)$r

## Now try the imputation method
library(mi) ## ?? or MICE??
## I ran the sas code to get the MI cor vals; they are below
x.y <- matrix(c(1,0.60090,0.69455,
2,0.57156,0.64984,
3,0.59098,0.67917,
4,0.46530,0.50405,
5,0.53881,0.60248,
6,0.57918,0.66123,
7,0.59798,0.69000,
8,0.51182,0.56519,
9,0.57203,0.65054,
10,0.53607,0.59862), nrow=10, ncol=3, byrow = T)

z.x <- matrix(c(1,0.39716,0.42027,
                2,0.29265,0.30146,
                3,0.56926,0.64643,
                4,0.43092,0.46103,
                5,0.36833,0.38649,
                6,0.53049,0.59083,
                7,0.47672,0.51873,
                8,0.48665,0.53165,
                9,0.54468,0.61078,
                10,0.46809,0.50762), nrow=10, ncol=3, byrow = T)
z.y <- matrix(c(1,0.41519,0.44187,
                2,0.54331,0.60884,
                3,0.48401,0.52820,
                4,0.29834,0.30769,
                5,0.42114,0.44907,
                6,0.52979,0.58986,
                7,0.43863,0.47054,
                8,0.47159,0.51211,
                9,0.51047,0.56336,
                10,0.51439,0.56868), nrow=10, ncol=3, byrow=T)

## Now test the null for the z x distirbution == .2 (0.2027/z)
n <- 10
mean.val <- mean(x.y[,3])
sd.val <- 0.06126367
error <- qnorm(0.975)*sd.val/sqrt(n)
mean.val + error


## Question 3 down here
library(performance)
in.dat <- read.csv("./Data/set3.csv")
mod.1 <- lmerTest::lmer(lang ~ (1|schoolnr), data=in.dat, REML = TRUE)
performance::icc(mod.1, by_group = T)
mod.1

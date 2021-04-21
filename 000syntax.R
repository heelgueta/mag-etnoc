###step 1
###general setup
#rm(list = ls()) #sirve para borrar todo; usar con cuidado
options(scipen=999) #disables scientific notation
options(max.print=1000000) #enable long outputs
#install.packages("lavaan","psych")

###step 2
###load datasets while discarding data from studies that didn't use all the items
###dataframe with 5p
dfe5 <- subset(read.csv("etnoc.csv",fileEncoding="UTF-8-BOM"),
			 est == 10000 | est == 20000 | est == 30000 |
			 	est == 40000 | est == 60000 | est == 170000 |
			 	est == 180000 |	est == 200000 , 
			 select=c(sex,age,reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8))
#remove cases with missing data
dfe5 <- na.omit(dfe5)

###dataframe with 7p
dfe7 <- subset(read.csv("etnoc.csv",fileEncoding="UTF-8-BOM"),
							 est == 110000 | est == 120000 | est == 130000 |
							 	est == 140000 | est == 150000 | est == 160000 | est == 190000,
							 select=c(sex,age,reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8))
#remove cases with missing data
dfe7 <- na.omit(dfe7)

###step 3
###descriptive statistics for each sample

table(dfe5$sex)
round(mean(dfe5$age),2);round(sd(dfe5$age),2)

table(dfe7$sex)
round(mean(dfe7$age),2);round(sd(dfe7$age),2)

###step 4
#classic psychometric analyses
summary(psych::alpha(dfe5[03:10]))
summary(psych::alpha(dfe7[03:10]))

###step 5
#1 factor cfa
#define model
mod <- 'reg =~ reg1 + reg2 + reg3 + reg4 + reg5 + reg6 + reg7 + reg8'

#test with 5p dataframe
fit5p <- lavaan::cfa(mod, data=dfe5,estimator="MLR")
lavaan::fitMeasures(fit5p, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic"))
lavaan::standardizedSolution(fit5p)

#test with 7p dataframe
fit7p <- lavaan::cfa(mod, data=dfe7,estimator="MLR")
lavaan::fitMeasures(fit7p, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic"))
lavaan::standardizedSolution(fit7p)

###step 6
#invariance across sexes for 5p df
fitconfig <- lavaan::cfa(mod, data=dfe5,estimator="MLR", group="sex")
fitmetric <- lavaan::cfa(mod, data=dfe5,estimator="MLR", group="sex", group.equal = c("loadings"))
fitscalar <- lavaan::cfa(mod, data=dfe5,estimator="MLR", group="sex", group.equal = c("loadings","intercepts"))
fitresidu <- lavaan::cfa(mod, data=dfe5,estimator="MLR", group="sex", group.equal = c("loadings","intercepts","residuals"))
paste(round(lavaan::fitMeasures(fitconfig, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitmetric, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitscalar, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitresidu, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
lavaan::anova(fitconfig,fitmetric,fitscalar,fitresidu)

#invariance across sexes for 7p df
fitconfig <- lavaan::cfa(mod, data=dfe7,estimator="MLR", group="sex")
fitmetric <- lavaan::cfa(mod, data=dfe7,estimator="MLR", group="sex", group.equal = c("loadings"))
fitscalar <- lavaan::cfa(mod, data=dfe7,estimator="MLR", group="sex", group.equal = c("loadings","intercepts"))
fitresidu <- lavaan::cfa(mod, data=dfe7,estimator="MLR", group="sex", group.equal = c("loadings","intercepts","residuals"))
paste(round(lavaan::fitMeasures(fitconfig, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitmetric, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitscalar, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitresidu, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
lavaan::anova(fitconfig,fitmetric,fitscalar,fitresidu)

#step 7
#invariance across age (median split)
###add dichotomous age median split for each df for multigroup cfas 
dfe5$ag2 <- sjmisc::dicho(dfe5$age)
dfe7$ag2 <- sjmisc::dicho(dfe7$age)

#invariance across age (median split) for 5p df
fitconfig <- lavaan::cfa(mod, data=dfe5,estimator="MLR", group="ag2")
fitmetric <- lavaan::cfa(mod, data=dfe5,estimator="MLR", group="ag2", group.equal = c("loadings"))
fitscalar <- lavaan::cfa(mod, data=dfe5,estimator="MLR", group="ag2", group.equal = c("loadings","intercepts"))
fitresidu <- lavaan::cfa(mod, data=dfe5,estimator="MLR", group="ag2", group.equal = c("loadings","intercepts","residuals"))
paste(round(lavaan::fitMeasures(fitconfig, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitmetric, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitscalar, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitresidu, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
lavaan::anova(fitconfig,fitmetric,fitscalar,fitresidu)

#invariance across age (median split) for 7p df
fitconfig <- lavaan::cfa(mod, data=dfe7,estimator="MLR", group="ag2")
fitmetric <- lavaan::cfa(mod, data=dfe7,estimator="MLR", group="ag2", group.equal = c("loadings"))
fitscalar <- lavaan::cfa(mod, data=dfe7,estimator="MLR", group="ag2", group.equal = c("loadings","intercepts"))
fitresidu <- lavaan::cfa(mod, data=dfe7,estimator="MLR", group="ag2", group.equal = c("loadings","intercepts","residuals"))
paste(round(lavaan::fitMeasures(fitconfig, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitmetric, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitscalar, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
paste(round(lavaan::fitMeasures(fitresidu, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic")),3))
lavaan::anova(fitconfig,fitmetric,fitscalar,fitresidu)

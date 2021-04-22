###step 1
###general setup
#rm(list = ls()) #sirve para borrar todo; usar con cuidado
options(scipen=999) #disables scientific notation
options(max.print=1000000) #enable long outputs
#install.packages("lavaan","psych","sjmisc","GPArotation")

###step 2
###load datasets while discarding data from studies that didn't use all the items
###dataframe with 5p
dfe5 <- subset(read.csv("etnoc.csv",fileEncoding="UTF-8-BOM"),
			 est == 10000 | est == 20000 | est == 30000 |
			 	est == 40000 | est == 60000 | est == 170000 |
			 	est == 180000 |	est == 200000 , 
			 select=c(est,pid,sex,age,reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8))
#remove cases with missing data
dfe5 <- na.omit(dfe5)

###dataframe with 7p
dfe7 <- subset(read.csv("etnoc.csv",fileEncoding="UTF-8-BOM"),
							 est == 110000 | est == 120000 | est == 130000 |
							 	est == 140000 | est == 150000 | est == 160000 | est == 190000,
							 select=c(est,pid,sex,age,reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8))
#remove cases with missing data
dfe7 <- na.omit(dfe7)

###step 3
###descriptive statistics for each sample
#5p df
table(dfe5$sex)
round(mean(dfe5$age),2);round(sd(dfe5$age),2)
#7p df
table(dfe7$sex)
round(mean(dfe7$age),2);round(sd(dfe7$age),2)

#n por estudio
table(dfe5$est)
table(dfe7$est)
###step 4
#classic psychometric analyses
round(psych::alpha(dfe5[05:12])$total$std.alpha,3);round(psych::omega(dfe5[05:12],plot=FALSE)$omega.tot,3)
round(psych::alpha(dfe7[05:12])$total$std.alpha,3);round(psych::omega(dfe7[05:12],plot=FALSE)$omega.tot,3)

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

###step 7
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

###step 8
#obtain factor scores, plot them?
lavaan::lavPredict(fit5p)
lavaan::lavPredict(fit7p)

hist(lavaan::lavPredict(fit5p))
hist(lavaan::lavPredict(fit7p))

#etc?
dffac <- rbind(cbind(dfe5[1:3],lavaan::lavPredict(fit5p)),cbind(dfe7[1:3],lavaan::lavPredict(fit7p)))			
hist(dffac$reg)

t.test(dffac$reg ~ dffac$sex)

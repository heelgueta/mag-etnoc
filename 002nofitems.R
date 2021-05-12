#define model
mod <- 'reg8i =~ reg1 + reg2 + reg3 + reg4 + reg5 + reg6 + reg7 + reg8'

#test with 5p dataframe
fit5p <- lavaan::cfa(mod, data=dfe5,estimator="MLR")
lavaan::fitMeasures(fit5p, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic"))
lavaan::standardizedSolution(fit5p)

#test with 7p dataframe
fit7p <- lavaan::cfa(mod, data=dfe7,estimator="MLR")
lavaan::fitMeasures(fit7p, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic"))
lavaan::standardizedSolution(fit7p)

###step 8
#obtain factor scores, plot them?
fac5p8i <- lavaan::lavPredict(fit5p)
fac7p8i <- lavaan::lavPredict(fit7p)

#define model

mod <- 'reg6i =~ reg1 + reg2 + reg3 + reg4 + reg6 + reg8'
#test with 5p dataframe
fit5p <- lavaan::cfa(mod, data=dfe5,estimator="MLR")
lavaan::fitMeasures(fit5p, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic"))
lavaan::standardizedSolution(fit5p)

#test with 7p dataframe
fit7p <- lavaan::cfa(mod, data=dfe7,estimator="MLR")
lavaan::fitMeasures(fit7p, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic"))
lavaan::standardizedSolution(fit7p)

###step 8
#obtain factor scores, plot them?
fac5p6i <- lavaan::lavPredict(fit5p)
fac7p6i <- lavaan::lavPredict(fit7p)

head(fac5p6i)
head(cbind.data.frame(fac5p8i,fac5p6i))
cor(fac5p8i,fac5p6i)
cor(fac7p8i,fac7p6i)

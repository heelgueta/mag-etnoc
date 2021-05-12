###choose dataset
dfm5 <- read.csv("mag5p.csv",fileEncoding="UTF-8-BOM")
dfm7 <- read.csv("mag7p.csv",fileEncoding="UTF-8-BOM")
dfc5 <- read.csv("chi5p.csv",fileEncoding="UTF-8-BOM")
dfc7 <- read.csv("chi7p.csv",fileEncoding="UTF-8-BOM")


#excluir casos incompletos
dfm5 <- na.omit(dfm5)
dfm7 <- na.omit(dfm7)
dfc5 <- na.omit(dfc5)
dfc7 <- na.omit(dfc7)

#para dfs de 5p, excluir valores fuera de rango
dfm5 <- dfm5[which(dfm5$sex<2), ]
dfm5 <- dfm5[which(dfm5$age>18), ]
dfm5 <- dfm5[which(dfm5$id1>0 & dfm5$id1 < 6), ]
dfm5 <- dfm5[which(dfm5$id2>0 & dfm5$id2 < 6), ]
dfm5 <- dfm5[which(dfm5$id3>0 & dfm5$id3 < 6), ]
dfm5 <- dfm5[which(dfm5$id4>0 & dfm5$id4 < 6), ]
dfm5 <- dfm5[which(dfm5$id5>0 & dfm5$id5 < 6), ]
dfm5 <- dfm5[which(dfm5$id6>0 & dfm5$id6 < 6), ]
dfm5 <- dfm5[which(dfm5$id7>0 & dfm5$id7 < 6), ]
dfm5 <- dfm5[which(dfm5$id8>0 & dfm5$id8 < 6), ]
dfm5 <- dfm5[which(dfm5$id9>0 & dfm5$id9 < 6), ]

#para dfs de 7p, excluir valores fuera de rango
dfm7$sex[dfm7$sex==2] <- 0
dfm7 <- dfm7[which(dfm7$sex<2), ]
dfm7 <- dfm7[which(dfm7$age>18), ]
dfm7 <- dfm7[which(dfm7$id1>0 & dfm7$id1 < 8), ]
dfm7 <- dfm7[which(dfm7$id2>0 & dfm7$id2 < 8), ]
dfm7 <- dfm7[which(dfm7$id3>0 & dfm7$id3 < 8), ]
dfm7 <- dfm7[which(dfm7$id4>0 & dfm7$id4 < 8), ]
dfm7 <- dfm7[which(dfm7$id5>0 & dfm7$id5 < 8), ]
dfm7 <- dfm7[which(dfm7$id6>0 & dfm7$id6 < 8), ]
dfm7 <- dfm7[which(dfm7$id7>0 & dfm7$id7 < 8), ]
dfm7 <- dfm7[which(dfm7$id8>0 & dfm7$id8 < 8), ]
dfm7 <- dfm7[which(dfm7$id9>0 & dfm7$id9 < 8), ]
#para dfs de 5p, excluir valores fuera de rango
dfc5 <- dfc5[which(dfc5$sex<2), ]
dfc5 <- dfc5[which(dfc5$age>18), ]
dfc5 <- dfc5[which(dfc5$id1>0 & dfc5$id1 < 6), ]
dfc5 <- dfc5[which(dfc5$id2>0 & dfc5$id2 < 6), ]
dfc5 <- dfc5[which(dfc5$id3>0 & dfc5$id3 < 6), ]
dfc5 <- dfc5[which(dfc5$id4>0 & dfc5$id4 < 6), ]
dfc5 <- dfc5[which(dfc5$id5>0 & dfc5$id5 < 6), ]
dfc5 <- dfc5[which(dfc5$id6>0 & dfc5$id6 < 6), ]
dfc5 <- dfc5[which(dfc5$id7>0 & dfc5$id7 < 6), ]
dfc5 <- dfc5[which(dfc5$id8>0 & dfc5$id8 < 6), ]
dfc5 <- dfc5[which(dfc5$id9>0 & dfc5$id9 < 6), ]

#para dfs de 7p, excluir valores fuera de rango
dfc7$sex[dfc7$sex==2] <- 0
dfc7 <- dfc7[which(dfc7$sex<2), ]
dfc7 <- dfc7[which(dfc7$age>18), ]
dfc7 <- dfc7[which(dfc7$id1>0 & dfc7$id1 < 8), ]
dfc7 <- dfc7[which(dfc7$id2>0 & dfc7$id2 < 8), ]
dfc7 <- dfc7[which(dfc7$id3>0 & dfc7$id3 < 8), ]
dfc7 <- dfc7[which(dfc7$id4>0 & dfc7$id4 < 8), ]
dfc7 <- dfc7[which(dfc7$id5>0 & dfc7$id5 < 8), ]
dfc7 <- dfc7[which(dfc7$id6>0 & dfc7$id6 < 8), ]
dfc7 <- dfc7[which(dfc7$id7>0 & dfc7$id7 < 8), ]
dfc7 <- dfc7[which(dfc7$id8>0 & dfc7$id8 < 8), ]
dfc7 <- dfc7[which(dfc7$id9>0 & dfc7$id9 < 8), ]

colnames(dfe5)
colnames(dfm5)
dfe5[2:12]
dfm5[c(3,6:14)]

dfmerg5 <- merge(dfe5[2:12], dfm5[c(3,6:14)], by.x = "pid")
dfmerg7 <- merge(dfe7[2:12], dfm7[c(3,6:14)], by.x = "pid")

mod <- 'reg =~ reg1 + reg2 + reg3 + reg4 + reg5 + reg6 + reg7 + reg8
es1 =~ id1 + id2 + id3
es2 =~ id4 + id5 + id6
es3 =~ id7 + id8 + id9'

#test with 5p dataframe
fit5p <- lavaan::cfa(mod, data=dfmerg5,estimator="MLR")
lavaan::fitMeasures(fit5p, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic"))
lavaan::standardizedSolution(fit5p)

#test with 7p dataframe
fit7p <- lavaan::cfa(mod, data=dfmerg7,estimator="MLR")
lavaan::fitMeasures(fit7p, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic"))
lavaan::standardizedSolution(fit7p)

#obtain factor scores, plot them?
scores5p <- lavaan::lavPredict(fit5p)
scores7p <- lavaan::lavPredict(fit7p)

hist(lavaan::lavPredict(fit5p))
hist(lavaan::lavPredict(fit7p))

#etc?
dffac <- rbind(cbind(dfe5[1:3],lavaan::lavPredict(fit5p)),cbind(dfe7[1:3],lavaan::lavPredict(fit7p)))			
hist(dffac$reg)

t.test(dffac$reg ~ dffac$sex)

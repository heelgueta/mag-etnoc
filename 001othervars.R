###choose dataset
dfm5 <- read.csv("mag5p.csv",fileEncoding="UTF-8-BOM")
dfm7 <- read.csv("mag7p.csv",fileEncoding="UTF-8-BOM")


#excluir casos incompletos
dfm5 <- na.omit(dfm5)
dfm7 <- na.omit(dfm7)

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

colnames(dfe5)
colnames(dfm5)
dfe5[2:12]
dfm5[c(3,6:14)]

dfmerg5 <- merge(dfe5[2:12], dfm5[c(3,6:14)], by.x = "pid")
dfmerg7 <- merge(dfe7[2:12], dfm7[c(3,6:14)], by.x = "pid")

mod <- 'reg =~ reg1 + reg2 + reg3 + reg4 + reg5 + reg6 + reg7 + reg8
es1 =~ id1 + id2 + id3
es2 =~ id4 + id5 + id6
es3 =~ id7 + id8 + id9
gen =~ es1 + es2 + es3'

#test with 5p dataframe
fit5p <- lavaan::cfa(mod, data=dfmerg5,estimator="MLR")
lavaan::fitMeasures(fit5p, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic"))
lavaan::standardizedSolution(fit5p)

#test with 7p dataframe
fit7p <- lavaan::cfa(mod, data=dfmerg7,estimator="MLR")
lavaan::fitMeasures(fit7p, c("chisq.scaled","df","cfi.scaled","tli.scaled","rmsea.scaled","rmsea.ci.lower.scaled","rmsea.ci.upper.scaled","srmr","aic","bic"))
lavaan::standardizedSolution(fit7p)

#obtain factor scores, plot them?
scores5p <- as.data.frame(lavaan::lavPredict(fit5p))
scores7p <- as.data.frame(lavaan::lavPredict(fit7p))
scores <- rbind(scores5p, scores7p)

head(scores)
plot(scores)
hist(scores5p$reg)
hist(scores7p$reg)
hist(scores5p$reg)
hist(scores7p$reg)



plot(scores5p)


library(ggplot2)


ggplot(scores, aes(x=gen, y=reg)) + geom_point(position=position_jitter(width=0.15, height=0.15), alpha=5/10) +
	labs(y = "Regionalismo-Etnocentrismo") + labs(x = "Identificación regional")

scores <- as.data.frame(scale(scores))
hist(scores$reg)
hist(scores$gen)
scores <- scores[which(scores$reg>-2.5), ]
scores <- scores[which(scores$gen>-2.5), ]
scores <- scores[which(scores$reg<2.5), ]
scores <- scores[which(scores$gen<2.5), ]

ggplot(scores, aes(x=gen, y=reg)) + geom_point(position=position_jitter(width=0.15, height=0.15), alpha=5/10) +
	labs(y = "Regionalismo-Etnocentrismo") + labs(x = "Identificación regional")

cor(scores$reg,scores$gen)
#etc?
dffac <- rbind(cbind(dfe5[1:3],lavaan::lavPredict(fit5p)),cbind(dfe7[1:3],lavaan::lavPredict(fit7p)))			

t.test(dffac$reg ~ dffac$sex)

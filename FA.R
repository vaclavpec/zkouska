getwd()
setwd("C:/Users/Vaclav//Desktop/Projekt_psychometrie/pokusy")
cjl <- read.csv(file="CJaL_data.csv", sep=";")

cjl$total <- rowSums(cjl[ , 5:ncol(cjl)-1])
head(cjl)
nrow(cjl)

ItemCor <- polychoric(x = cjl[,5:38])
ItemCor$rho
ItemCor2 <- ItemCor$rho
ItemCor2

cjclust <- hclust(d=as.dist(1-ItemCor2), method = "ward.D2")
cjclust
library(ggdendro)
ggdendrogram(data=cjclust)
plot_corr(Data=cjl[, 5:38], cor= "poly", clust_method= "ward.D2")

library(psych)
FA1 <- fa(r=ItemCor2, cor="polychoric", nfactors=1, fm = "ml", n.obs=66766)
print(FA1$loadings, cutoff=0)
write.table(FA1$loadings, file="FA1_load.csv", sep=";", quote=FALSE, col.names=TRUE, row.names= TRUE
            )

ItemFA <- tcrossprod(FA1$loadings)+diag(FA1$uniquenesses)
ItemFA

ItemCor2
ItemFA.resid <- ItemCor2 - ItemFA
ItemFA.resid

FA2 <- fa(r=ItemCor2, cor="polychoric", nfactors=2, fm = "ml", n.obs=66766)
FA2
print(FA2$loadings)
write.table(FA2$loadings, file="FA2_load.csv", sep=";", quote=FALSE, col.names=TRUE, row.names=TRUE
)

FA2_obl <- fa(r=ItemCor2, cor="polychoric", nfactors=2, n.obs=66766, rotate = "oblimin")
print(FA2_obl$loadings)         

write.table(FA2_obl$loadings, file="FA2_obl_load.csv", sep=";", quote=FALSE, col.names=TRUE, row.names=TRUE
)


eigen_cjl <- eigen(ItemCor2)$values
scree(ItemCor2)
fa_parallel(Data=ItemCor2, n_obs = 66766)


FA3_obl <- fa(r=ItemCor2, cor="polychoric", nfactors=3, n.obs=66766, rotate = "oblimin")
print(FA3_obl$loadings, cutoff=0.3)

write.table(FA3_obl$loadings, file="FA3_obl_load.csv", sep=";", quote=FALSE, col.names=TRUE, row.names=TRUE
)

head(ItemCor2, n=2)
model3 <- "MR1=~ b1 + b2.1 + b2.2 + b8 + b9 + b12 + b13 + b15 + b18 + b19 + b20 + b22 + b29 + b31
MR3=~ b4 + b5+ b17 + b30
MR2=~ b6 + b28.1 + b28.2"
model3


library(lavaan)
fit_3 <- cfa(model3, data= ItemCor2)
parTable(fit_3)
summary(fit_3)
summary(fit_3, fit.measures=TRUE, standardized=TRUE)

library(psych)
lavaan.diagram(fit_3)
semPlot::semPaths(fit_3, what="stdest", rotation=4)


hier_model3 <- "CJL=~ MR1 + MR2 + MR3
MR1=~ b1 + b2.1 + b2.2 + b8 + b9 + b11 + b12 + b13 + b15 + b18 + b19 + b20 + b22 + b29 + b31 
MR2=~ b6 + b28.1 + b28.2
MR3=~ b4 + b5 + b17 + b23 + b30"

library(lavaan)

hierfit_3 <- cfa(hier_model3, data=ItemCor2)
partable(hierfit_3)
summary(hierfit_3, fit.measures=TRUE, standardized=TRUE)
parameterEstimates((hierfit_3))
semPlot::semPaths(hierfit_3, what = "est", rotation=4)


cjlfa <- data.frame(cjl$b1, cjl$b2.1, cjl$b2.2, cjl$b8,cjl$b9 , cjl$b11 , cjl$b12, cjl$b13 , cjl$b15 , cjl$b18, cjl$b19 , cjl$b20 , cjl$b22 , cjl$b29 , cjl$b31 , cjl$b6 , cjl$b28.1 , cjl$b28.2,cjl$b4 , cjl$b5 , cjl$b17, cjl$b23 , cjl$b30)
head(cjlfa)
cjlfa$total <- rowSums(cjlfa[ , 5:ncol(cjlfa)-1])

lmcjl30 <- lm(total~cjl.b30+cjl.b4+cjl.b5+cjl.b17+cjl.b23, data=cjlfa)
summary(lmcjl30)
ggplot(cjlfa, aes(x=total,y=cjl.b30))+geom_point()
head(lmcjl30$residuals)
mrcjl <- summary(lmcjl30)
mrcjl$r.squared

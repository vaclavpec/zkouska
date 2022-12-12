##directory setting##

getwd()
setwd("C:/Users/Vaclav//Desktop/Projekt_psychometrie/pokusy")

##addressing data##
cjl <- read.csv(file="CJaL_data.csv", sep=";")

##calculation of total score, checking data##
cjl$total <- rowSums(cjl[ , 5:ncol(cjl)-1])
head(cjl)
ncol(cjl)

##creation of abbreviated dataset with items only##
cjl_items <- cjl[, 5:38]

##calculation of Cronbach alpha manually (it didnt work for some reason, value is slightly off)##
b <- ncol(cjl_items) 
b
var(cjl$total)
VC <- var(cjl_items)
cronbach <- (b/(b-1)*(1-(sum(diag(VC)))/sum(VC)))
cronbach

##calculation of cronbach alpha with "psychometric" package, including CI##
a <- psychometric::alpha(cjl_items)
a
kCI <- psychometric::alpha.CI(a, N=nrow(cjl_items), k= ncol(cjl_items), level = 0.95)
kCI

##using psych package for alpha if item deleted analysis##
d <- psych::alpha(cjl_items)
d

##estimating alpha for doubling number of items##
f <- 2*a/(1+a)
f

##calculating number of items needed for alpha = 0.9##
g <- 0.9
h <- g*(1-a)/(a*(1-g)) 
h
ceiling(33*h)

##creating vector from item names##
cn <- colnames(cjl_items)
cn[1]
cjl_items
##crating table...##
write.table(d$alpha.drop$raw_alpha, file= "alpha_item_deleted.csv", sep=";", quote=FALSE, col.names=FALSE, row.names=cn)



##calculating split half coefficient (1st-2nd)##
df1 <- cjl_items[,1:17]
df2 <- cjl_items[,18:34]
rs1 <- rowSums(df1)
rs2 <- rowSums(df2)
cor.x <- cor(rs1, rs2)
cor_1_2 <- 2*cor.x/(1+cor.x)
cor_1_2

##calculating split half coefficient (even-odd)##
dfeven <- cjl_items[,seq(1,34,2)]
dfodd <- cjl_items[,seq(2,34,2)]
rseven <- rowSums(dfeven)
rsodd <- rowSums(dfodd)
cor.ed <- cor(rseven, rsodd)
cor.ed
cor.evenodd <- 2*cor.ed/(1+cor.ed)
cor.evenodd

##calculating split half coefficient (random)##
set.seed(123)
samp <- sample(1:34, 17)
df_rand1 <- cjl_items[,samp]
df_rand2 <- cjl_items[, setdiff(1:34, samp)]
rs_rand1 <- rowSums(df_rand1)
rs_rand2 <- rowSums(df_rand2)
cor.rand <- cor(rs_rand1,rs_rand2)
cor_rand <- 2*cor.rand/(1+cor.rand)
cor_rand


##transforming the cjl_items into long format##
library(tidyverse)
cjl_items <- mutate(cjl_items, id=row_number())
head(cjl_items)
cjl_long_items <- tidyr::pivot_longer(data=cjl_items, cols = starts_with("b"), names_to = "item", values_to="rating")
head(cjl_long_items)

##g-study with gtheory package## 
model <- rating ~ (1|id)+(1|item)
(gfit <- gtheory::gstudy(formula= model, data=cjl_long_items))
##printing estimated sources of variance for ?single item score?##
gfit$components
gfit_table <- data.frame(gfit$components)
gfit_table
write.table(gfit_table, file="gfit.csv", quote=FALSE, sep=";", col.names= TRUE, row.names=FALSE)
gfit$components$var

##here trying to calculate generalizability and dependability coefficients but failing for syntax reasons?##
gtheory::dstudy(gfit, colname.objects="id", colnames.scores="rating", data = cjl_long_items)

##another try with hemp package##
modelmer <- lme4::lmer(rating ~ (1|id)+(1|item), data=cjl_long_items)
(gpxir <- hemp::gstudy(modelmer))
gpxir

##and this time with more plausible values of g- and d- coefficients, probably because here I managed to tell the program th number of items and respondends, hoewver I hope it didnt read every response as being from different respondent##
dpxir <- hemp::dstudy(gpxir, n=c("item"=34), unit="id")
dpxir

hemp::dstudy_plot(gpxir, facets= list ("item"=seq(0,34,2)), unit="id", g_coef = TRUE)
hemp::dstudy_plot(gpxir, facets= list ("item"=seq(0,34,2)), unit="id", g_coef = FALSE)

##and once more with ANOVA##
##calculaing number of levels for items, id and means for item ratings##
head(cjl_long_items)
m <- nlevels(as.factor(cjl_long_items$item))         
m
n <- nlevels(as.factor(cjl_long_items$id))
n
mean.overall <- mean(cjl_long_items$rating)
mean.overall

##calculating mean values of individual items and mean scores per item for individual respondents##
mean.items <- tapply(cjl_long_items$rating, cjl_long_items$item, mean)
mean.persons <- tapply(cjl_long_items$rating, cjl_long_items$id, mean)
mean.items
mean.persons

##calculating sum of squares for item scores, person scores and item scores##
SStotal <- sum((cjl_long_items$rating - mean.overall)^2)
SStotal
SSP <- m * sum((mean.persons - mean.overall)^2)
SSI <- n * sum((mean.items - mean.overall)^2)

##calculating residual sum of squares##
SSe <- SStotal - SSP - SSI

##calculating mean square values (isnt it just variance? what is the diff?)##
MStotal <- SStotal / (n * m - 1)
MStotal
MSP <- SSP / (n - 1)
MSP
MSI <- SSI / (m - 1)
MSI
MSe <- SSe / ((n - 1) * (m - 1))
##variance components estimations - same as with the gtheory and hemp package##
(sigma2P <- (MSP - MSe) / m)

(sigma2I <- (MSI - MSe) / n)

(sigma2e <- MSe)

(sigma2total <- sigma2P + sigma2I + sigma2e)

##Alpha estimations from ANOVA##
(MSP-MSe)/MSP
sigma2P / (sigma2P + 1 / m * sigma2e)




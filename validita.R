getwd()
setwd("C:/Users/Vaclav//Desktop/Projekt_psychometrie/pokusy")
cjl <- read.csv(file="CJaL_data.csv", sep=";")

cjl$total <- rowSums(cjl[ , 5:ncol(cjl)-1])
head(cjl)
ncol(cjl)

polychoric(table(cjl[,5], cjl[,6]))

ItemCor <- polychoric(x = cjl[,5:38])
ItemCor$rho
ItemCor2 <- ItemCor$rho
ItemCor3 <- rowMeans(ItemCor2)
ItemCor3
ItemCor4 <- (ItemCor3 - 1/32)*(32/31)
ItemCor4
write.table(ItemCor4, file= "itemcor.csv", sep=";", quote=FALSE, col.names=FALSE, row.names=TRUE)
library(ShinyItemAnalysis)
plot_corr(Data=cjl[,5:38], cor ="polychoric")

bestcor1<- polychoric(table(cjl[,11], cjl[,33] ))
bestcor2 <- polychoric(table(cjl[,11], cjl[,34] ))
bestcor3<- polychoric(table(cjl[,33], cjl[,34] ))
bestcor1$rho
bestcor2$rho
bestcor3$rho

bestcor4<- polychoric(table(cjl[,6], cjl[,7] ))
bestcor4$rho
bestcor5<- polychoric(table(cjl[,22], cjl[,36] ))
bestcor5$rho

i <- 0
ii <- 0
cortablern <- NULL
cortablecn <- NULL
ItemCor2cn <- colnames(ItemCor2)
ItemCor2cn
ItemCor2rn <- rownames(ItemCor2)


bigcor <- NULL

for (ii in 1:34) {
  
for (i in 1:34){
     if(ItemCor2[i,ii] > 0.4 && ItemCor2[i,ii] <1) 
       
     
     bigcor <- append(bigcor, ItemCor2[i,ii])
      
     
   }}
bigcor


bccn <- NULL

for (ii in 1:34) {
  
  for (i in 1:34){
    if(ItemCor2[i,ii] > 0.4 && ItemCor2[i,ii] <1) 
      
      
      bccn<- append(bccn, ItemCor2cn[i])
    
    
  }}
bccn

bcrn <- NULL

  

bcrn
bigcortable <- matrix(data = NA, dimnames=(list(bcrn,bccn)), nrow=16, ncol=16)
  
for (i in 1:16){
  
  
  bigcortable[i,i] <- bigcor[i]
  
  
}
bigcortable
write.table(bigcortable, file= "bigcortable.csv", sep=";", quote=FALSE, col.names=TRUE, row.names=TRUE)

cortablern
cortablecn

warnings()
cora
best.cor <-  c(ItemCor2[2,3], ItemCor2[7,29], ItemCor2[7,30], ItemCor2[18,32], ItemCor2[29,30])
best.cor
writ.table(best.cor, file=best-cor.csf, sep=";", quote=FALSE, col.names="B2.1:B2.2", "B6:B28.1", B6:28.2)

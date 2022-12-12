cjl <- read.csv(file="CJaL_data.csv", sep=";")
head(cjl)
colnames(cjl)
dim(cjl)

cjl$total <- rowSums(cjl[ , 5:ncol(cjl)-1])
head(cjl$total)
colnames(cjl)

i <- 0
assign(paste0("Itemsum_", 1:3), 1)
Itemsum_1 <- c(summary(factor(cjl[,1])))
Itemsum_1
Itemsum_2 <-  c(summary(factor(cjl[,2])))
Itemsum_2
Itemsum_3 <-  c(summary(factor(cjl[,3])))
Itemsum_4 <-  c(summary(factor(cjl[,4])))
for(i in 5:NCOL(cjl)-1) {assign(paste0("Itemsum_", i), c(summary(cjl[,i])))}                                                     
Totalsum <- c(summary(cjl[,ncol(cjl)]))
Totalsum
summary_table <- data.frame(Itemsum_5, Itemsum_6, Itemsum_7, Itemsum_8, Itemsum_9, Itemsum_10, Itemsum_11, Itemsum_12, Itemsum_13, Itemsum_14, Itemsum_15, Itemsum_16, Itemsum_17, Itemsum_18, Itemsum_19, Itemsum_20, Itemsum_21, Itemsum_22, Itemsum_23, Itemsum_24, Itemsum_25, Itemsum_26, Itemsum_27, Itemsum_28, Itemsum_29, Itemsum_30, Itemsum_31, Itemsum_32, Itemsum_33, Itemsum_34, Itemsum_35, Itemsum_36,  Itemsum_37, Itemsum_38, Totalsum)
summary_table
item_names <- c(colnames(cjl))
good_item_names <- setdiff(item_names, c(item_names[1:4]))
good_item_names
p_vyskytu <- c("", "poèet výskytù")
write.table(summary_table, file="summary_table.csv", quote = FALSE, sep=";",col.names= good_item_names, row.names=TRUE)
write.table(Itemsum_1, file="Itemsum_1.csv", quote=FALSE, sep=";", col.names= c("poèet výskytù"), row.names=TRUE)
write.table(Itemsum_2, file="Itemsum_2.csv", quote=FALSE, sep=";", col.names= c("poèet výskytù"), row.names=TRUE)
write.table(Itemsum_3, file="Itemsum_3.csv", quote=FALSE, sep=";", col.names= c("poèet výskytù"), row.names=TRUE)
write.table(Itemsum_4, file="Itemsum_4.csv", quote=FALSE, sep=";", col.names= c("poèet výskytù"), row.names=TRUE)

hist(cjl$total, col = "light blue", breaks = 3:50, main= "Histogram of total score for: Cesky jazyk a literatura - jarni termin 2022", ylab = "Number of respondents", xlab = "Total score")        

zscore <- scale(cjl$total)
head(zscore)
tscore <- (10*zscore)+50
head(tscore)
s_rate <- 100*(cjl$total/max(cjl$total))
head(s_rate)
perc <- round(100 * ecdf(cjl$total)(cjl$total))
head(perc)
nazvy_radku <- c("Z-score", "T-score", "Success rate", "Percentile")

resp1 <- matrix(c(nazvy_radku, zscore[1,1], tscore[1,1], s_rate[1], perc[1]), ncol=4, byrow=TRUE)
  resp1 <- as.table(resp1, )
print(resp1)

write.table(resp1, file= "resp1.txt", sep=";", quote=FALSE, col.names=FALSE, row.names=FALSE)            
write.csv()            

print(cjl[,3])

      
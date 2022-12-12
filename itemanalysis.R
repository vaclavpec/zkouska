getwd()
setwd("C:/Users/Vaclav//Desktop/Projekt_psychometrie/pokusy")
cjl <- read.csv(file="CJaL_data.csv", sep=";")

##getting familiar with th esample again##
cjl$total <- rowSums(cjl[ , 5:ncol(cjl)-1])
head(cjl)
ncol(cjl)

##creating table matching item names and item order
cjl_itemnames <- colnames(cjl[,5:38])
cjl_itemnames
onetothirtyfour <- 1:34
i_ordernames <- c(cjl_itemnames, onetothirtyfour)
item_order <- matrix(i_ordernames, nrow=2, ncol=34, byrow=TRUE)
item_order

##creating vector of ordinary items by sorted by order = by collumn index##
more_than_one <- which(cjl[,5:38]>1, arr.ind = TRUE)
ordinary_items <- unique(more_than_one[,2])
ordinary_items

##some items have maximum points optained of 3 or more
more_than_two <- which(cjl[,5:38]>2, arr.ind = TRUE)
three_items <- unique(more_than_two[,2])
three_items

##some items have maximum points optained of 4 or more
more_than_three <- which(cjl[,5:38]>3, arr.ind = TRUE)
four_items <- unique(more_than_three[,2])
four_items

##NO items have maximum points optained of 5 or more
more_than_four <- which(cjl[,5:38]>4, arr.ind = TRUE)
five_items <- unique(more_than_four[,2])
five_items

##vector of binary items by order = collumn index
binary_items <- onetothirtyfour[!(onetothirtyfour %in% ordinary_items)]
binary_items

##names of ordinary items
ordinary_names <- NULL
for (i in ordinary_items) {
  ordinary_names <- append(ordinary_names, item_order[1,i])
  
}
ordinary_names

##names of binary items
binary_names <- NULL
for (i in binary_items) {
  binary_names <- append(binary_names, item_order[1,i])
  
}
binary_names

###########
########### preparations complete


##calculating means and SDs of binary items##
cjl_items <- cjl[,5:38]
binaryitem_means <- sapply(cjl_items[, binary_items], mean)
binaryitem_means
write.table(binaryitem_means, file= "binarydifficulty.csv", sep=";", quote=FALSE, col.names=TRUE, row.names=TRUE)

binaryitem_sds <- sapply(cjl_items[, binary_items], sd)
binaryitem_sds

##sorting binary items form easiest to hardest
sort(binaryitem_means, decreasing = TRUE)

##calculating means and SDs of ordinary items
cjl_items <- cjl[,5:38]
ordinaryitem_means <- sapply(cjl_items[, ordinary_items], mean)
ordinaryitem_means

ordinaryitem_sds <- sapply(cjl_items[, ordinary_items], sd)
ordinaryitem_sds

##item total correlation (RIT)
sapply(cjl[, 5:38], function(i) cor(i, cjl$total))
write.table(sapply(cjl[, 5:38], function(i) cor(i, cjl$total)), file="RIT.csv", sep=";", quote=FALSE)
)

##item rest correlation (RIR)
dataR <- cjl$total - cjl[, 5:38]
RIR <- diag(cor(cjl[, 5:38], dataR))
write.table(matrix(c(sapply(cjl[, 5:38], function(i) cor(i, cjl$total)), RIR), nrow=2, ncol=34, byrow=TRUE), file="RIR.csv", sep=";", quote=FALSE, col.names=colnames(cjl[,5:38]))
)

##upper-lower group index (ULI)
ShinyItemAnalysis::gDiscrim(Data = cjl[, 5:38])
ShinyItemAnalysis::DDplot(Data=cjl_items, discrim = "ULI")

cjl_items
cjl_items$total <- rowSums(cjl_items[1:ncol(cjl_items-1)])
cjl$b2.1
library(ggplot2)
ggplot2::ggplot(cjl_items, aes(b2.1, total)) + geom_line()

  
  
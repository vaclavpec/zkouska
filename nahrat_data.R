my_data <- read.csv ("CJL_data.csv", header=TRUE, sep=";");

my_data$total <- rowSums(my_data[ , 3:36])
min(my_data$total)

hist(my_data$total, breaks = 3:50, density = NULL, col = "light blue", main= "Histogram celkov?ho sk?re pro: ?esk? jazyk a literatura - jarn? term?n 2022", ylab = "Po?et respondent?", xlab = "Celkov? sk?re")
colnames(my_data)

summary(factor(my_data$?.?Druh.stredni.skoly))


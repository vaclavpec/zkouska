my_data <- read.csv ("CJL_data.csv", header=TRUE, sep=";");

my_data$total <- rowSums(my_data[ , 3:36])
min(my_data$total)

hist(my_data$total, breaks = 3:50, density = NULL, col = "light blue", main= "Histogram celkového skóre pro: Èeskı jazyk a literatura - jarní termín 2022", ylab = "Poèet respondentù", xlab = "Celkové skóre")
colnames(my_data)

summary(factor(my_data$ï.¿Druh.stredni.skoly))


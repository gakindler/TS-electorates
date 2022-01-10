setwd("~/Desktop/Additional_articles/Electorates/Analysis")
library(readr)
data <- read_csv("Species_intersect_elec_1_diss.csv")
View(data)

# Find most frequent word
Top10 <- sort(table(data$Elect_div), decreasing=T)[1:10]
View(Top10)
sum(Top10,na.rm=TRUE)
length(unique(data$CURRENT_NA))

1146/1667*100

# Find most frequent word in >50% range
data50 <- subset(data, Proportion >= 50)
View(data50)
Top50 <- sort(table(data50$Elect_div), decreasing=T)[1:10]
View(Top50)
sum(Top50,na.rm=TRUE)

652/1667*100

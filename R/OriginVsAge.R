```{r}
library(dplyr)

###Load the data
OriginFile <- file.choose(new = FALSE)
Origin <- read.csv2(OriginFile, header = TRUE) 

OriginVsAge <- mutate(Origin, Age = (2015 - as.numeric(paste("19", substr(Origin$per_hetu, 5, 6), sep = ""))))

write.table(OriginVsAge, file = "AgeDist.csv", sep = ",")


OriginVsAgeFile <- file.choose(new = FALSE)
OriginVsAgeAll <- read.csv2(OriginVsAgeFile, header = TRUE) 


hist(OriginVsAgeAll$CAREA_Age, breaks = 20, col = "grey", main = "CAREA age distribution", xlab = "Age", ylab = "Number of people" )
summary(OriginVsAgeAll$CAREA_Age)
```
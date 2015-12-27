#Install required packages
#install.packages("xlsx")
#install.packages("lubridate")

library(xlsx) #could have used readxl too
library(lubridate)
#source("AdSpend.R")

tulima.sales <- read.xlsx2("Tulima - sales and brand metrics.xlsx","Sales",as.data.frame=TRUE, header=TRUE,
                           colClasses=c("Date", "character"),stringsAsFactors = FALSE)
tulima.brandmetrics <- read.xlsx2("Tulima - sales and brand metrics.xlsx","Brand Metrics",as.data.frame=TRUE, header=TRUE,
                                  colClasses=c("Date", "character"),stringsAsFactors = FALSE)
tulima.storelocations <- read.xlsx2("Tulima - sales and brand metrics.xlsx","Store locations")
tulima.GAonlinecoversion <- read.xlsx("Tulima - sales and brand metrics.xlsx",
                                       "GA online conversion",as.data.frame=TRUE, header=TRUE,
                                       colClasses=c("Date", "character"))
tulima.GAonlinecoversion <- tulima.GAonlinecoversion[,-3]

#add new columns with dates converted into week numbers 
#to integrate data from other sources

#sales worksheet starts with week 0 (i.e. - "2014-12-29")
tulima.sales$viikko <- week(tulima.sales$Date)
tulima.sales[tulima.sales$Date=="2014-12-29",]$viikko <- 0

#Brand Metrics worksheet starts with week 1 (i.e. - "1/7/2015")
tulima.brandmetrics$viikko <- week(tulima.brandmetrics$Date)

#GAonlinecoversion worksheet starts with week 1 (i.e. - "1/1/2015")
tulima.GAonlinecoversion$viikko <- week(tulima.GAonlinecoversion$Date)


#Find ad spending per week for #Tulima's 
#dairy("MEIJERITUOTTEET") and juice("MEHUT")
tulima.adspend.dairyjuice.perweek <- aggregate(tulima.adspend.dairyjuice$Yhteensä, 
                                  by=list(tulima.adspend.dairyjuice$viikko), 
                                  FUN=sum)
names(tulima.adspend.dairyjuice.perweek) <- c("viikko", "Yhteensä")
png(file = "plot7.png", width = 580, height = 580)
plot(tulima.adspend.dairyjuice.perweek[,2] ~ 
        tulima.adspend.dairyjuice.perweek[,1],
        main = "Total ad spending on Tulima dairy and juice products per week",
        xlab = "Week number", 
        ylab = "Total ad spending")
lines(tulima.adspend.dairyjuice.perweek[,1], 
      tulima.adspend.dairyjuice.perweek[,2], 
      type="o",
      col="red")
dev.off()


#Find sales per week for #Tulima's 
#dairy("MEIJERITUOTTEET") and juice("MEHUT")
tulima.sales.perweek <- aggregate(as.numeric(tulima.sales$Tulima.sales), 
                                               by=list(tulima.sales$viikko), 
                                               FUN=sum)
names(tulima.sales.perweek) <- c("viikko", "Yhteensä")
png(file = "plot8.png", width = 580, height = 580)
plot(tulima.sales.perweek[,2] ~ 
         tulima.sales.perweek[,1],
     main = "Total sales of Tulima dairy and juice products per week",
     xlab = "Week number", 
     ylab = "Total sales")
lines(tulima.sales.perweek[,1], 
      tulima.sales.perweek[,2], 
      type="o",
      col="red")
dev.off()

#Turn dates into week number
#week(tulima.GAonlinecoversion$Date)
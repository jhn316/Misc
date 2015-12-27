#Weekly sales spending report analysis

#filename = "201512 TULIMA - VIIKKORAPORTTI LOPULLINEN - perusmediaryhmät 1000€.csv"
filename = "201512 TULIMAVIIKONRAPORTTI.csv"
weekly.report <- read.csv2(filename, 
                           header = TRUE, stringsAsFactors = FALSE
                           ,strip.white=TRUE)
weekly.report <- weekly.report[,-14]

#Remove "Viikko" and "2015", from the "viikko" column
weekly.report$viikko <- as.numeric(sub(" 2015", "", sub("Viikko ", 
                            "", weekly.report$viikko)))
tulima.weekly.report <- subset(weekly.report, 
                            weekly.report$mainostaja=="TULIMA")
#Loads templates for plotting 
source("makeplots.R")

#Total spending of all mainostajat
totspend.peradvertiser <- aggregate(weekly.report$Yhteensä, 
                            by=list(weekly.report$mainostaja), FUN=sum)
#Use makebarplot function from "makeplots.R"
makebarplot(x=totspend.peradvertiser[,1],
            y=totspend.peradvertiser[,2],
            title = "Top 10 Amount spent vs. Advertiser",
            xlabel = "Advertiser",
            ylabel = "Amount spent(kEUR)",
            las=0,
            col= "red")


#Tulima total spend per week over the year
tulima.spend.perweek <- aggregate(tulima.weekly.report$Yhteensä, 
                            by=list(tulima.weekly.report$viikko), FUN=sum)
png(file = "plot1.png", width = 580, height = 580)
makebarplot(x=tulima.spend.perweek[,1],
            y=tulima.spend.perweek[,2],
            title = "Amount spent by Tulima vs. week number",
            xlabel = "Week number",
            ylabel = "Amount spent(kEUR)", 
            las=0,
            col= "red")
dev.off()


#Find closest competitor(max that is not tulima)
tulimaindex <- match("TULIMA",totspend.peradvertiser[,1])
competeadvertiserindex <- which.max(totspend.peradvertiser[-tulimaindex,2])
competeadvertiser <- totspend.peradvertiser[competeadvertiserindex+1,1]
compete.weekly.report <- subset(weekly.report, weekly.report$mainostaja==competeadvertiser)

#Next max spender total spend per week
compete.spend.perweek <- aggregate(compete.weekly.report$Yhteensä, 
                                  by=list(compete.weekly.report$viikko), FUN=sum)
png(file = "plot2.png", width = 580, height = 580)
makebarplot(x=compete.spend.perweek[,1],
            y=compete.spend.perweek[,2],
            title = paste("Amount spent by",competeadvertiser,"vs. week number"),
            xlabel = "Week number",
            ylabel = "Amount spent(kEUR)",
            las=0,
            col= "red")
dev.off()

#Tulima total total spend per product category(tuoteryhmä)
#For top 10 products
tulima.spend.perprod <- aggregate(tulima.weekly.report$Yhteensä, 
          by=list(tulima.weekly.report$tuote), FUN=sum)
tulima.top10spend.perprod <- head(tulima.spend.perprod[with(tulima.spend.perprod, 
                                                            order(-x)), ],10)
png(file = "plot3.png", width = 580, height = 580)
makebarplot(x=tulima.top10spend.perprod[,1],
            y=tulima.top10spend.perprod[,2],
            title = "Amount spent by Tulima vs. Product",
            #xlabel = "Product",
            xlabel = "",
            ylabel = "Amount spent(kEUR)",
            las=2,
            col= "red")
dev.off()


#Tulima total spend per media type
tulima.spend.permedia <- aggregate(tulima.weekly.report$Yhteensä, 
                                  by=list(tulima.weekly.report$perusmediatv), FUN=sum)
png(file = "plot4.png", width = 580, height = 580)
makebarplot(x=tulima.spend.permedia[,1],
            y=tulima.spend.permedia[,2],
            title = "Amount spent by Tulima by media type",
            #xlabel = "Media type",
            xlabel = "",
            ylabel = "Amount spent(kEUR)",
            las=2,
            col= "red")
dev.off()

# Same data as above in Pie Chart with Percentages
slices <- tulima.spend.permedia[,2]
lbls <- tulima.spend.permedia[,1]
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
png(file = "plot5.png", width = 580, height = 580)
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Amount spent by Tulima by media type")
dev.off()

#Tulima total spend per brand (brädi)
tulima.spend.perbrand <- aggregate(tulima.weekly.report$Yhteensä, 
                                   by=list(tulima.weekly.report$brändi), FUN=sum)
png(file = "plot6.png", width = 580, height = 580)
makebarplot(x=tulima.spend.perbrand[,1],
            y=tulima.spend.perbrand[,2],
            title = "Amount spent by Tulima by brand",
            #xlabel = "brand name",
            xlabel = "",
            ylabel = "Amount spent(kEUR)",
            las=2,
            col= "red")
dev.off()

# Same data as above in Pie Chart with Percentages
slices <- tulima.spend.perbrand[,2]
lbls <- tulima.spend.perbrand[,1]
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Amount spent by Tulima by brand")

#Tulima's dairy("MEIJERITUOTTEET") and juice("MEHUT") ad spending
tulima.adspend.dairyjuice <- subset(weekly.report,
                                  (weekly.report$tuoteryhmä=="MEIJERITUOTTEET")|
                                      (weekly.report$tuoteryhmä=="MEHUT"))
#once all the visualization are built remove 
#all variables except the "tulima.spend.dairyjuice"
#rm(list=ls()[ls() != "tulima.adspend.dairyjuice"])

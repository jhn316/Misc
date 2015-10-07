#Authored by Y. Nissanka S. Wickremasinghe

#This script analyzes the data provided in "exercise.csv" 
#by InLineMarket and answers the 3 questions posed

#Download required packages from CRAN if necessary
install.packages("dplyr")
install.packages("reshape2")
install.packages("data.table")

#Install required packages
library(dplyr)
library(reshape2)
library(data.table)

#Load the data into a dataframe 
#The file "exercise.csv" should be saved in the working directory
#For ease of processing, we set empty fields in the input data as 
#NA and prevent conversion of string variables into factor variables
data.cars.raw <- read.csv2("exercise.csv", header = TRUE, sep = ",",
                  na.strings = "", stringsAsFactors = FALSE)
View(data.cars.raw)

#Find out all the types of cars given in mention1 
brands.of.cars <- as.character(unique(data.cars.raw[,4]))

#The following code snippet checks whether the cars in the 
#"mention1" column are the exact same car brands that are 
#in all the other columns. If they are then temp.var1 will
#be a vector with all TRUE values
temp.var1 <-c()
for (idx in 5:13){
    temp.var2 <- as.character(unique(data.cars.raw[,idx]))
    temp.var2 <- temp.var2[!is.na(temp.var2)]
    temp.var1 <- c(temp.var1,setequal(brands.of.cars,temp.var2))
}
temp.var1

#The following snippet checks if respondent_id goes 
#from 1 to 20000 without any breaks. If they do go 
#from 1 to 20000 without breaks then result.resp == TRUE
test.resp <- c(1:20000)
cars.resp <- data.cars.raw$respondent_id
result.resp <- setequal(test.resp,cars.resp)
result.resp

#Define a constant for the total number of respondents
num.respondents <- 20000


##---------Answer to Taks 1----------
#The raw data is in "wide" form and the required answer to Q1
#is in the "long" form. Thus we use the reshape command to 
data.cars.long <- reshape(data.cars.raw, 
             varying = c("mention1", "mention2", "mention3", "mention4", "mention5",
                         "mention6", "mention7", "mention8", "mention9", "mention10"), 
             v.names = "brand",
             timevar = "position", 
             times = c("1", "2", "3", "4", "5",
                       "6", "7", "8", "9", "10"), 
             direction = "long")

#Move the columns around to match the required output format
#and sort according to respondent_id and brand
data.cars.long <- data.cars.long[, c(1,2,3,5,4)]
data.cars.long <- arrange(data.cars.long,respondent_id, brand)
View(data.cars.long)


#NOTE - The processing of the following part took approx. 20 minutes 
#The following code will fill in the missing values and values that
#need to be edited in columns "brand" and "position"
#Subset by respondent_id and do required processing 
#for each respondent and row bind the resulting data frame one by one 
#to create the final output

data.cars.final <- data.table()
for (index.a in 1:num.respondents){
    temp.df2 <- subset(data.cars.long, 
                                 data.cars.long$respondent_id == index.a)
    
    add.end <- temp.df2[1,]         #There are 18 car types in total but only 
    add.end[,c(4,5)] <- NA          #10 possible mention opportunities. Since
    for (idy in 1:8){               #The final result result requires the response
        temp.df2 <-                 #to all 18 car types, we add 8 rows with the
            rbind(temp.df2,add.end) #same "respond_id", "gender", "age" as the 1st row
        }                           #to the bottom of each subsetted data frame and
                                    #fills in the "brand" and "position" 
                                    #(i.e. columns 4 and 5) with NA's, for later processing
    
    
    
    for (idz in 1:NROW(temp.df2)){              #Loops through each row of the
        if (is.na(temp.df2[idz,4])){            #subset and if "brand" (i.e. column 4) 
            temp.df2[idz,5] <- "not mentioned"  #is NA, then write the text 
        }                                       #"note mentioned" to "position" 
    }                                           #(i.e. column 5)
    
    
    #The below fills in the unmentioned car types to the 
    #"brand" column and sorts by "respondent_id" and "brand"
    temp.df2$brand[is.na(temp.df2$brand)] <- setdiff(brands.of.cars, temp.df2$brand)
    temp.df2 <- arrange(temp.df2,respondent_id, brand)
    
    #The following does a row bind to create the final output
    #Note that I have used the data.table package with rbindlist
    #since it is faster than the rbind form the base package 
    temp.df2 <- data.table(temp.df2)
    l <- list(data.cars.final, temp.df2)
    data.cars.final <- rbindlist(l)
}
View(data.cars.final)

#Write the final output data frame to "task1output.csv" 
write.csv(data.cars.final, file = "task1output.csv", row.names = FALSE)
##---------End of answer to Task 1----------


##---------Answer to Task 2----------
#Explore the gender data  
unique(data.cars.raw$gender)
table(data.cars.raw$gender)

#Average number of brands mentioned by Males
resp.male <- subset(data.cars.long, gender == "Male")
male.avg <- sum(!is.na(resp.male$brand))/NROW(unique(resp.male$respondent_id))
male.avg

#Average number of brands mentioned by Females?
resp.female <- subset(data.cars.long, gender == "Female")
female.avg <- sum(!is.na(resp.female$brand))/NROW(unique(resp.female$respondent_id))
female.avg

##The same can be done in many other ways as shown below for example
#male <- subset(data.cars.raw, gender == "Male",row.names = FALSE)[,c(4:13)]
#male.avg <- sum((apply(male,1,function(x){sum(!is.na(x))})))/nrow(male)
# female <- subset(data.cars.raw, gender == "Female",row.names = FALSE)[,c(4:13)]
# sum((apply(female,1,function(x){sum(!is.na(x))})))/nrow(female)
##---------End of answer to Task 2----------



##---------Answer to Task 3----------
#Brands together with their share of respondents which mentioned that 
#brand among their top 3 mentions.

top.brands <- as.data.frame(table(data.cars.raw$mention1) + table(data.cars.raw$mention2) 
        + table(data.cars.raw$mention3))

mentions.precent <- round(top.brands$Freq/sum(top.brands$Freq)*100, digits = 1)
top.brands <- cbind(top.brands,mentions.precent)
names(top.brands) <- c("Brand name", "Share of respondents (top 3 mentions)", 
                       "Percentage of top 3 respondents (top 3 mentions)")
View(top.brands)
write.csv(top.brands, file = "topbrands.csv", row.names = FALSE)
##---------End of answer to Task 3----------

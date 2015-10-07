library(lubridate)
library(dplyr)


## Load the data
InputData <- file.choose(new = TRUE)
QuesAnsGenRisk <- read.csv2(InputData, header = TRUE)
AccFormQuesAns <- subset(QuesAnsGenRisk, questionnaire_name == 'AcceptanceForm')



## Function for handling the time-stamps
## answer_createdate contains time-stamps as to when
## a given question was answred. The answers are in the
## "2015-02-04 08:31:08.943" format and we use the "2015-02-04 08:31"
## part of the time stamp to infer that it is one person who has
## filled in the answers

## Function to extract the "2015-02-04 08:31" part of the timestamp which is in the
## "2015-02-04 08:31:08.943" format
ExtractTime <- function(InputTime){
  return (as.character((OutputTime <- ymd_hms(InputTime, tz = "Europe/Helsinki"))))
}

AccFormQuesAns <- mutate(AccFormQuesAns,
                         answer_createdate_part = ExtractTime(AccFormQuesAns$answer_createdate))

ndx <- order(AccFormQuesAns$answer_createdate_part, decreasing=T)
AccFormQuesAnsSorted <- AccFormQuesAns[ndx,]



FullAnswerTimes <- as.data.frame(table(AccFormQuesAns$answer_createdate_part))
FullAnswerTimes <- subset(FullAnswerTimes, as.numeric(FullAnswerTimes$Freq) == 9)

SuitablePeople <- data.frame()

Output <- data.frame()
for (idx in FullAnswerTimes$Var1){
  Temp <- subset(AccFormQuesAnsSorted,
                 AccFormQuesAnsSorted$answer_createdate_part == idx)
  
  Temp$answer_booleanvalue <- as.logical(toupper(Temp$answer_booleanvalue))
  
  if ((Temp$answer_booleanvalue[1]==TRUE)
      & (Temp$answer_booleanvalue[2]==FALSE)
      & (Temp$answer_booleanvalue[3]==FALSE)
      & (Temp$answer_booleanvalue[4]==FALSE)
      & (Temp$answer_booleanvalue[5]==FALSE)
      & (Temp$answer_booleanvalue[6]==FALSE)
      & (Temp$answer_booleanvalue[7]==FALSE)
      & (Temp$answer_booleanvalue[8]==FALSE)){
    Suitability <- "Suitable"
  }else{
    Suitability <- "Not Suitable"
  }
  Output <- cbind(idx,Suitability,as.character(Temp$answer_textvalue[9]))
  SuitablePeople <- as.data.frame(rbind(SuitablePeople, Output))
}

write.csv(SuitablePeople, file = "SuitablePeopleAcceptanceForm.csv")

table(SuitablePeople$Suitability)

SuitableCarea <- subset(SuitablePeople,SuitablePeople$V3=="carea")
table(SuitableCarea$Suitability)
SuitableMehi <- subset(SuitablePeople,SuitablePeople$V3=="mehilainen")
table(SuitableMehi$Suitability)
SuitableVerip <- subset(SuitablePeople,SuitablePeople$V3=="veripalvelu")
table(SuitableVerip$Suitability)
SuitableNone <- subset(SuitablePeople,SuitablePeople$V3=="none")
table(SuitableNone$Suitability)

#Initialize variables
Output2 <- data.frame()
Carea <- list(0,0,0,0,0,0,0,0,"")
Mehilainen <- list(0,0,0,0,0,0,0,0,"")
Veripalvelu <- list(0,0,0,0,0,0,0,0,"")
None <- list(0,0,0,0,0,0,0,0,"")

AnswerCounter <- function(DF, A, Org){
  if (DF$answer_booleanvalue[1]==FALSE){A[[1]] <- A[[1]] + 1}
  if (DF$answer_booleanvalue[2]==TRUE){A[[2]] <- A[[2]] + 1}
  if (DF$answer_booleanvalue[3]==TRUE){A[[3]] <- A[[3]] + 1}
  if (DF$answer_booleanvalue[4]==TRUE){A[[4]] <- A[[4]] + 1}
  if (DF$answer_booleanvalue[5]==TRUE){A[[5]] <- A[[5]] + 1}
  if (DF$answer_booleanvalue[6]==TRUE){A[[6]] <- A[[6]] + 1}
  if (DF$answer_booleanvalue[7]==TRUE){A[[7]] <- A[[7]] + 1}
  if (DF$answer_booleanvalue[8]==TRUE){A[[8]] <- A[[8]] + 1}
  if (nchar(A[[9]]) == 0){A[[9]]<- Org}
  return(A)
}

for (idy in FullAnswerTimes$Var1){
  Temp2 <- subset(AccFormQuesAnsSorted,
                   AccFormQuesAnsSorted$answer_createdate_part == idy)
   
  Temp2$answer_booleanvalue <- as.logical(toupper(Temp2$answer_booleanvalue))
  # Computes the answers. Inputs are A-a list with the 9 elements
  # in the form e.g. Carea <- list(0,0,0,0,0,0,0,0,"")
  if (Temp2$answer_textvalue[9]=="carea"){Carea <- AnswerCounter(Temp2, Carea, "carea")}
  if (Temp2$answer_textvalue[9]=="mehilainen"){Mehilainen <- AnswerCounter(Temp2, Mehilainen, "mehilainen")}
  if (Temp2$answer_textvalue[9]=="veripalvelu"){Veripalvelu <- AnswerCounter(Temp2, Veripalvelu, "veripalvelu")}
  if (Temp2$answer_textvalue[9]=="none"){None <- AnswerCounter(Temp2, None, "none")}
}

rbind(Carea, Mehilainen, Veripalvelu, None)


library(lubridate)
library(dplyr)


###Load the data
QuesAnsGenRisk <- read.csv2("questionnaire_answers_v9.csv", header = TRUE) 


###Time taken to complete forms (with outliers)
TutKyselyQuesAns <- subset(QuesAnsGenRisk, questionnaire_name == 'GeneriskForm')
PersonsTime <- (subset(TutKyselyQuesAns, !duplicated(TutKyselyQuesAns$per_oid)))
PersonsTime <- mutate(PersonsTime, timetocomplete = (ymd_hms(questionnaire_answer_date, tz = "Europe/Helsinki") 
                                                     - ymd_hms(answer_createdate, tz = "Europe/Helsinki")))
PersonsTimeNoOutlie <- subset(PersonsTime, (0 < as.numeric(PersonsTime$timetocomplete)))
dotchart(as.numeric(PersonsTimeNoOutlie$timetocomplete), cex=.9, main="Time taken to complete subject form (with outilers)", xlab="Time in minutes")
summary(as.numeric(PersonsTimeNoOutlie$timetocomplete))



###Time taken to complete forms (times < 300 minutes)
PersonsTimeNoOutlie <- subset(PersonsTime, (0 < as.numeric(PersonsTime$timetocomplete)) & (as.numeric(PersonsTime$timetocomplete) < 300))
dotchart(as.numeric(PersonsTimeNoOutlie$timetocomplete), cex=.9, main="Time taken to complete subject form (times < 300min)", xlab="Time in minutes")
summary(as.numeric(PersonsTimeNoOutlie$timetocomplete))


###HETU analysis
```{r}
#Hetu <- "061275-179N"
#Hetu <- gsub("-", " ", Hetu, ignore.case = TRUE)
YearOfBirth <- as.numeric(paste("19", substr(Hetu, 5, 6), sep = ""))
Age <- 2015 - YearOfBirth
HetuMFPart <- as.numeric(substr(Hetu, 8, 10))

#Check if even or odd
is.even <- function(x){ x %% 2 == 0 }

MorF <- function(x){
  if (is.even(x)){
    "Nainen" <- y
  } else {
    "Mies" <- y
  }
  return(y)
}

Gender <- MorF(HetuMFPart) 
```

###Histograms of measured blood pressures
BloodPress1 <- QuesAnsGenRisk[grep("Verenpaine 1", QuesAnsGenRisk$question_title, ignore.case = TRUE),]
BloodPress2 <- QuesAnsGenRisk[grep("Verenpaine 2", QuesAnsGenRisk$question_title, ignore.case = TRUE),]
BloodPress3 <- QuesAnsGenRisk[grep("Verenpaine 3", QuesAnsGenRisk$question_title, ignore.case = TRUE),]


###Histograms of previous diastolic and systolic blood pressure 
Diastolinen <- QuesAnsGenRisk[grep("diastolinen", QuesAnsGenRisk$question_title, ignore.case = TRUE),]
Systolinen <- QuesAnsGenRisk[grep("systolinen", QuesAnsGenRisk$question_title, ignore.case = TRUE),]

hist(Diastolinen$answer_numbervalue, main="Viimeinen diastolinen verenpaine", 
     xlab="Verenpaine (mmHg)", col = "red")
summary(Diastolinen$answer_numbervalue)

hist(Systolinen$answer_numbervalue, main="Viimeinen systolinen verenpaine", 
     xlab="Verenpaine (mmHg)", col = "blue")
summary(Systolinen$answer_numbervalue)


###Histograms of BMI
BMI <- QuesAnsGenRisk[grep("painoindeksi", QuesAnsGenRisk$question_title, ignore.case = TRUE),]

hist(BMI$answer_numbervalue, main="BMI", 
     xlab="BMI (kg/m^2)", col = "red")
summary(BMI$answer_numbervalue)


# New stuff
TutTilQuesAns <- subset(QuesAnsGenRisk, questionnaire_name == 'ExaminationForm')

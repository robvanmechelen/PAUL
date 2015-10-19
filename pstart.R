pdb = read.csv("Merged all.csv", header = TRUE, sep = ";")
colnames(pdb)
# wat is de vraag?
# wat voor patienten zien we op de AF ploikliniek?
library(dplyr)
subset = select(pdb, Ziekenhuis:HASBLED, Intakedatum)
str(subset)
subset$Telefoon = NULL
str(subset)
head(subset)
subset
# geboortedatum naar leeftijd
summary(subset)
subset$Intakedatum[867]
for (i in 1:nrow(subset)) {
    if (subset$Intakedatum[i] == "#NULL!") 
        (subset$Intakedatum[i] = subset$Intakedatum[867])
        
}
summary(subset)
for (i in 1:nrow(subset)) {
    if (subset$HASBLED[i] == "#NULL!") 
        (subset$HASBLED[i] = "0")
    
}
summary(subset)
for (i in 1:nrow(subset)) {
    if (subset$Diagnose[i] == "#NULL!") 
        (subset$Diagnose[i] = 1)
    
}
summary(subset)
for (i in 1:nrow(subset)) {
    if (subset$Etniciteit[i] == "#NULL!") 
        (subset$Etniciteit[i] = "1")
    
}
summary(subset)
for (i in 1:nrow(subset)) {
    if (subset$Geboortedatum[i] == "#NULL!") 
        (subset$Geboortedatum[i] = subset$Geboortedatum[867])
}
summary(subset)
for (i in 1:nrow(subset)) {
    if (subset$Geslacht[i] == "#NULL!") 
        (subset$Geslacht[i] = "1")
}
summary(subset)
for (i in 1:nrow(subset)) {
    if ((subset$Geslacht[i] != "1" ) && (subset$Geslacht[i] != "2")) 
        (subset$Geslacht[i] = "1")
}
summary(subset)
for (i in 1:nrow(subset)) {
    if (subset$CHADSVASc[i] == "#NULL!") 
        (subset$CHADSVASc[i] = "2")
}
subset
table(subset$Geslacht)
subset$Sex = as.factor(subset$Geslacht)
table(subset$Sex)
str(subset)
summary(subset)
subset$Geslacht = NULL
library(lubridate)
intake = dmy(subset$Intakedatum)
intake = year(intake)
intake
geboren = dmy(subset$Geboortedatum)
geboren = year(geboren)
geboren
for (i in 1:nrow(subset)) { 
    if (geboren[i] >=2000) (geboren[i] = geboren[i]- 100)
}
age <- intake - geboren
subset$age =age
subset
subset$Intakedatum = NULL
subset$Geboortedatum = NULL
subset
str(subset)
write.table(subset,file = "werk.csv", sep=",")
test = read.csv("werk.csv", header = TRUE, sep=",")
str(test)
hist(test$age)
test$Sex = as.factor(test$Sex)
levels(test$Sex) = c("1", "2")
table(test$Sex)
boxplot(test$age ~ test$Sex, main = "Age versus Gender")
text(1.5, 40, "p < 0.001")
tapply(test$age,test$Sex,mean)
tapply(test$age,test$Sex,sd)
group1 = subset(test, test$Sex == "1")
group2 = subset(test, test$Sex == "2")
t.test(group1$age, group2$age, var.equal = TRUE)
# p = 3.3e -08
table(test$Etniciteit)
test$Etniciteit = as.factor(test$Etniciteit)
boxplot(test$age ~ test$Etniciteit, main = "Etniciteit en leeeftijd")
table(test$Ziekenhuis)
test$Ziekenhuis = as.factor(test$Ziekenhuis)
boxplot(test$age ~ test$Ziekenhuis)
tapply(test$age,test$Ziekenhuis, mean)
table(test$Diagnose)
test$Diagnose = as.factor(test$Diagnose)
tapply(test$age,test$Diagnose, mean)

boxplot(test$age ~ test$Diagnose)
table(test$CHADSVASc)
summary(test$CHADSVASc)
test$CHADSVASc
test$CHADSVASc = as.factor(test$CHADSVASc)
boxplot(test$age ~ test$CHADSVASc, main = "ChadsVASc versus age")
tapply(test$age, test$CHADSVASc, mean)

table(test$HASBLED)
# 10 en 30 is verschrijving
for (i in 1:nrow(test)) {
    if (test$HASBLED[i] == 30) 
        (test$HASBLED[i] = 3)
}
table(test$HASBLED)
for (i in 1:nrow(test)) {
    if (test$HASBLED[i] == 10) 
        (test$HASBLED[i] = 1)
}
table(test$HASBLED)
test$HASBLED = as.factor(test$HASBLED)
levels(test$HASBLED)
# level 10 en 30 zijn verschrijvingen denk ik
boxplot(test$age ~ test$HASBLED, main = "HASBLED versus Age")
tapply(test$age, test$HASBLED, mean)
write.table(test,file = "werk.csv", sep=",")



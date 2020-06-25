# fd <- read.csv("C:/Users/Charu/Downloads/fd.txt", header=FALSE,stringsAsFactors=FALSE)
# str(fd)

 library(readxl)
 fd <- read_excel("E:/Sem2-218256/ANLY 510-90 - O - 2018Spring - Analytics II Principles and Applications/Project/projdata.xlsx")
 View(fd)
 str(fd)
#colnames(fd) <- c("Season", "Age", "Childiseases", "AccidentorTrauma", "Surgiintervention",
 #                 "Highfevers", "alcoholconsumption", "Smokinghabit", "hoursspent", "Output")
names(fd)

View(fd)
#fd$Output[fd$Output == "N"] <- 1
#fd$Output[fd$Output == "O"] <- 0

#class(fd$Output)

#Except Age and Sitting Hours Spent all are categorical

fd$Diagnosis <- as.factor(fd$Diagnosis)
fd$Season <- as.factor(fd$Season)
fd$ChildhoodDeceases<- as.factor(fd$ChildhoodDeceases)
fd$Accident <- as.factor(fd$Accident)
fd$SurgicalIntervention <-as.factor(fd$SurgicalIntervention)
fd$Highfeverlastyear <- as.factor(fd$Highfeverlastyear)
#fd$alcohalconsumption <- as.factor(fd$alcohalconsumption)
fd$SmokingHabit <- as.factor(fd$SmokingHabit)
str(fd)

library(ggplot2)
install.packages("cowplot")
library(cowplot)

a=ggplot(data = fd, mapping = aes(x = Diagnosis)) +  geom_histogram(stat = "count")
b = ggplot(data = fd, mapping = aes(x = Season)) +  geom_histogram( stat = "count")
c = ggplot(data = fd, mapping = aes(x = ChildhoodDeceases)) +  geom_histogram( stat = "count")
d = ggplot(data = fd, mapping = aes(x = Highfeverlastyear )) +  geom_histogram( stat = "count")
e = ggplot(data = fd, mapping = aes(x = alcohalconsumption )) +  geom_histogram( stat = "count")
f = ggplot(data = fd, mapping = aes(x = SmokingHabit )) +  geom_histogram( stat = "count")
g = ggplot(data = fd, mapping = aes(x = SurgicalIntervention )) +  geom_histogram( stat = "count")
plot_grid(a,b,c,d,e,f,g)

library(car)
par(mfrow = c(2,1))
scatterplot(fd$Diagnosis, fd$Age)

scatterplot(fd$Diagnosis, fd$Sittinghours)
###########33
# Checking the significance of  categorical variable -Chi sqr test of independence

#output vs season
season_association = table(fd$Diagnosis,fd$Season)

season_association

chisq.test(season_association)
# As the p-value 0.2446 is greater than the .05 significance level,
# we do not reject the null hypothesis that the semem sample quality is independent of the season.

#output vs Childiseases
tbl = table(fd$Diagnosis,fd$ChildhoodDeceases)

tbl

chisq.test(tbl)
  # As the p-value 1 is greater than the .05 significance level,
# we do not reject the null hypothesis that the semem sample quality is independent of the Childiseases


#output vs AccidentorTrauma
tbl = table(fd$Diagnosis,fd$Accident)

tbl

chisq.test(tbl)
# As the p-value 0.2698 is greater than the .05 significance level,
# we do not reject the null hypothesis that the semem sample quality is independent of the AccidentorTrauma


#output vs Surgiintervention
tbl = table(fd$Diagnosis,fd$SurgicalIntervention)

tbl

chisq.test(tbl)
# As the p-value 0.815 is greater than the .05 significance level,
# we do not reject the null hypothesis that the semem sample quality is independent of the Surgiintervention


#output vs Highfevers
tbl = table(fd$Diagnosis,fd$Highfeverlastyear)

tbl

chisq.test(tbl)
# As the p-value 0.4618 is greater than the .05 significance level,
# we do not reject the null hypothesis that the semem sample quality is independent of the Highfevers



#output vs alcoholconsumption
tbl = table(fd$Diagnosis,fd$alcohalconsumption)

tbl

chisq.test(tbl)
# As the p-value  0.4025 is greater than the .05 significance level,
# we do not reject the null hypothesis that the semem sample quality is independent of the alcoholconsumption



#output vs Smokinghabit
tbl = table(fd$Diagnosis,fd$SmokingHabit)

tbl

chisq.test(tbl)
# As the p-value  0.898 is greater than the .05 significance level,
# we do not reject the null hypothesis that the semem sample quality is independent of the Smokinghabit

##################################



hec <- margin.table(HairEyeColor, 2:1)

tbl = table(fd$alcohalconsumption,fd$SmokingHabit)

tbl
barplot(tbl, beside = TRUE, legend = TRUE)
vcd::tile(tbl)
ftable(fd, row.vars="alcohalconsumption", col.vars = c("Diagnosis", "SmokingHabit"))
mosaic(tbl, labeling = labeling_values)

chisq.test(tbl)


tbl = table(fd$alcohalconsumption,fd$Season)

tbl

chisq.test(tbl)

interaction.plot(fd$alcohalconsumption, fd$SmokingHabit)
ggplot(fd, aes(x=factor(SmokingHabit), y = Diagnosis, group = factor(alcohalconsumption), color = factor(alcohalconsumption)))+ geom_point() + geom_line()+theme_bw()



tbl = table(fd$alcohalconsumption,fd$ChildhoodDeceases)

tbl

chisq.test(tbl)



tbl = table(fd$alcohalconsumption,fd$Accident)

tbl

chisq.test(tbl)



tbl = table(fd$alcohalconsumption,fd$Highfeverlastyear)

tbl

chisq.test(tbl)


tbl = table(fd$alcohalconsumption,fd$SurgicalIntervention)

tbl

chisq.test(tbl)


tbl = table(fd$SurgicalIntervention,fd$Season)
chisq.test(tbl)


tbl = table(fd$SurgicalIntervention,fd$Accident)
chisq.test(tbl)




tbl = table(fd$SurgicalIntervention,fd$ChildhoodDeceases)
chisq.test(tbl)

tbl = table(fd$SurgicalIntervention,fd$Highfeverlastyear)
chisq.test(tbl)
#X-squared = 7.8997, df = 2, p-value = 0.01926


tbl = table(fd$SurgicalIntervention,fd$SmokingHabit)
chisq.test(tbl)


tbl = table(fd$Season,fd$Highfeverlastyear)
chisq.test(tbl)

tbl = table(fd$Season,fd$Accident)
chisq.test(tbl)

Season and high fever
Season and accidental trauma

############################################33
#par(mfrow=c(2,2))
tbl = table(fd$alcohalconsumption,fd$SmokingHabit)
barplot(tbl, beside = TRUE, legend = TRUE, xlab = "Smoking",col = c("red","blue","green","black","yellow"), main = "Smoking & Alcohol consumption")
#vcd::tile(tbl)
#ftable(fd, row.vars="alcohalconsumption", col.vars = c("Diagnosis", "SmokingHabit"))
tbl = table(fd$SurgicalIntervention,fd$Highfeverlastyear)
barplot(tbl, beside = TRUE, legend = TRUE, xlab="High Fever",col = c("black","yellow"),main = " High Fever & SurgicalIntervention")


tbl = table(fd$Season,fd$Highfeverlastyear)
barplot(tbl, beside = TRUE, legend = TRUE, xlab="High Fever",col = c("green","yellow","black","blue"),main = " High Fever & Season")


tbl = table(fd$Season,fd$Accident)
barplot(tbl, beside = TRUE, legend = TRUE,xlab="Accident",col = c("green","yellow","black","blue"),main = " Accident & Season")



###############
model <- glm(fd$Diagnosis ~ Age * Season * ChildhoodDeceases*Accident*SurgicalIntervention*Highfeverlastyear*alcohalconsumption*SmokingHabit*SmokingHabit, family = binomial(logit), data = fd )

summary(model)
# alcohol - smoking
# SurgicalIntervention and Highfeverlastyear
# Season and high fever
# Season and accidental trauma

model1 <- glm(fd$Diagnosis ~ . + SmokingHabit:alcohalconsumption , family = binomial(logit), data = fd )
summary(model1)
model2 <- glm(fd$Diagnosis ~ . + SmokingHabit:alcohalconsumption - Season - SurgicalIntervention , family = binomial(logit), data = fd )
summary(model2)


####333 Try with Alcohal as continuous.
fd$alcohalconsumption <- scale(fd$alcohalconsumption,scale = F)

model1 <- glm(fd$Diagnosis ~ . + SmokingHabit:alcohalconsumption , family = binomial(logit), data = fd )
summary(model1)
model2 <- glm(fd$Diagnosis ~ . + SmokingHabit:alcohalconsumption - Season - SurgicalIntervention , family = binomial(logit), data = fd )
summary(model2)


######################################



install.packages("ROCR")
library(ROCR)

predict <- predict(model1, type = "response")

ROCRpred <- prediction(predict, fd$Diagnosis)

ROCRperf <- performance(ROCRpred, 'tpr','fpr')

plot(ROCRperf, colorize = TRUE)

##Area under the curve

auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]
auc


install.packages("pPOC")


predict <- predict(model2, type = "response")

ROCRpred <- prediction(predict, fd$Diagnosis)

ROCRperf <- performance(ROCRpred, 'tpr','fpr')

plot(ROCRperf, colorize = TRUE) 


library(pscl)
pR2(model1)
pR2(model2)

# prob=predict(model1,type=c("response"))
# fd$prob=prob
# library(pROC)
# 
# roc.data <-roc(GLM$y , GLM$fitted.values,ci=T)
# 
# g <- roc(Diagnosis ~ Age , data = fd )
# g
# 
# library(FrF2)
# auc(predict)
# g


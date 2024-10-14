rm(list=ls())
set.seed(18552)
anes<-read.csv("anes_pilot_2018.csv")
#clean data 
summary(anes$apppres)
summary(anes$fttrump)
View(anes$fttrump)#we see there are variables that lay beyond 0-100, so clean 
anes$fttrump[anes$fttrump<=-1]<-NA
anes$fttrump[anes$fttrump>=101]<-NA
table(anes$fttrump)
anes$pid7x[anes$pid7x<=0]<-NA
anes$lcself[anes$lcself<=0]<-NA
table(anes$race)
anes$race[anes$race<=0]<-NA
anes$gunsar[anes$gunsar<=-1]<-NA
anes$race_factor<-factor(anes$race, levels = 1:7, labels = c("White", "Black", "Hispanic", "Asian", "Native\n American", "Mixed", "Other"))
table(anes$race_factor)
summary(anes$integrity3) 
anes$integrity3[anes$integrity3<=0]<-NA
table(anes$integrity3)
summary(anes$vote16) 
table(anes$vote16)
anes$vote16_dummy<-NA
anes$vote16_dummy[anes$vote16==1]<-1 
anes$vote16_dummy[anes$vote16==2]<-0 
table(anes$vote16_dummy)
#Question1
#(A)
library(stargazer)
mod1<-lm(fttrump~apppres+finworry, data = anes)
stargazer(mod1,type = "text")
summary(mod1)
#(B)
library(margins)
library(ggplot2)
cplot(mod1, "apppres", main="Relationship between the feeling
thermometer and job approval variables", xlab="job approval", ylab="feeling thermometer", se.fill="red",lwd=2)
#Question2
#(A)
mod2<-lm(gunsar~pid7x+lcself+race_factor, data = anes)
stargazer(mod2, type = "text")
summary(mod2)
#(B)
install.packages("ggeffects")
library(ggeffects)
library(ggplot2)
p<-ggpredict(mod2,"pid7x")
plot(p)+
  ggtitle("The effect of party identification on support for a weapons ban.")+
  labs(y="Support for weapon ban", x="party identification")
#Question3
#(A)
anes$marstat_factor<-factor(anes$marstat)
anes$gender_factor<-factor(anes$gender)
logit<-glm(vote16_dummy~gender_factor+educ+pid7x+marstat_factor+race_factor, data = anes, family = binomial)
summary(logit)
stargazer(logit, type = "text")
#(B)
anes$gendername_factor<-factor(anes$gender, levels = c(1,2), labels = c("Male", "Female"))
logit2<-glm(vote16_dummy~gendername_factor+educ+pid7x+marstat_factor+race_factor, data = anes, family = binomial(link = logit))
cplot(logit2, "gendername_factor", main="Effect of gender on vote choice", xlab="Gender", ylab="Vote choice")
#(C)
cplot(logit2, "race_factor", main="Effect of gender on vote choice", xlab="Race", ylab="Vote choice")
#Question4
mod3<-loess(warmyou~lcself, data=anes)
#(A)
summary(mod3)
cplot(mod3, "lcself", main="Effect of political ideology on how important each respondent finds the issue of climate change", xlab="Political Ideology", ylab="Importance of Climate Change")
#Question5
mod5<-lm(integrity3~gendername_factor+race_factor+marstat_factor, data=anes)
summary(mod5)
cplot(mod5, "gendername_factor", main="Effective of gender on agreement of requiring showing ID when they vote", xlab="Gender", ylab="Integrity")
stargazer(mod5, type = "text")
cplot(mod5, "race_factor", main="Effective of gender on agreement of requiring showing ID when they vote", xlab="Race", ylab="Integrity")
cplot(mod5, "marstat", main="Effective of gender on agreement of requiring showing ID when they vote", xlab="Marital Status", ylab="Integrity")
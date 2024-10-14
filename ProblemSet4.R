rm(list=ls())
set.seed(18552)
library(tidyverse)
#Q1
dta<-LifeCycleSavings
?LifeCycleSavings
dta$country<-rownames(dta)
ggplot(data = dta, aes(x = reorder(country, ddpi), y = ddpi, fill = country)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Percentage Growth Rate for Per-Capita Disposable Income by Country, 1960-1970",
       y = "Country", x = "ddpi") +
  theme_minimal()+
  coord_flip()
#Q2
dta$pop15_dummy<-NA
dta$pop15_dummy[dta$pop15>=35]<-1
dta$pop15_dummy[dta$pop15<35]<-0
mod1<-lm(sr~dpi+ddpi+pop15_dummy,data=dta)
library(stargazer)
summary(mod1)
stargazer(mod1, type = "text")
library(margins)
cplot(mod1, "ddpi", main="Relationship between aggregate personal
savings and growth rate of disposable income", xlab="Growth rate of disposable income", ylab="Aggregate personal savings")
mod2<-mod1<-lm(sr~dpi+ddpi+pop15_dummy+(ddpi*pop15_dummy),data=dta)
summary(mod2)
stargazer(mod2, type = "text")
install.packages("interactions")
library(interactions)
interact_plot(mod2, pred=ddpi, modx=pop15_dummy, interval = TRUE)+
  labs(x="growth rate of disposable income", y="aggregate of personal savings in each country", title="interaction plot of the relationship
between aggregate personal savings and growth rate of disposable income
that is mediated by the population age dummy variable ")
#Q3
install.packages("dagitty")
library(dagitty)
install.packages("ggdag")
library(ggdag)
dag<-dagify(y~x+c+z+a+b, x~c, z~c, a~b, labels =c("y"="National Wealth", "x"="Personal Income", "z"="Personal Consumption Level", "c"="Economic policies", "a"="Infrastructure", "b"="Technology"))
ggdag(dag, text = FALSE, use_labels = "label")
#Part2
library(sf)
library(readxl)
library(tidyr)
library(zoo)
library(lubridate)
library(scales)
library(sp)
#install.packages("rgdal")
#library(rgdal)
library(cowplot)
library(ggpubr)
covid<-read_excel("Texas COVID-19 New Confirmed Cases by County.xlsx")
covid.data<-covid %>% gather(Date, Cases, -c(County, Population))
avg_daily_cases <- covid.data %>%
  group_by(County) %>%
  summarise(avg_cases = mean(Cases))
spatial_data$County<-spatial_data$CNTY_NM
map<-merge(spatial_data, avg_daily_cases, by='County')
spatial_data<-st_read("Texas County Shapefile")
ggplot(map)+
  geom_sf(aes(fill=avg_cases), color=NA)+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  ggtitle("Texas Counties' Daily COVID-19 Cases")+
  scale_fill_distiller(palette = "RdPu", direction=1)+
  labs(fill="Average Daily Cases \nper 1,000 Residents")


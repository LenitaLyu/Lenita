rm(list=ls())
set.seed(18552)
dta<-read.csv("Clean_Dataset.csv")
mod1<-lm(price~stops+airline+class, data = dta)
install.packages("ggeffects")
library(ggeffects)
install.packages("stargazer")
library(ggplot2)
library(margins)
library(stargazer)
library(ggeffects)
library(tidyr)
library(zoo)
library(readxl)
library(lubridate)
library(ggplot2)
library(scales)
library(sp)
library(cowplot)
library(ggpubr)
library(maps)
library(sf)
cplot(mod1, "stops")
summary(mod1)

#stops
plot(ggpredict(mod1, "stops"))
plot(ggpredict(mod1, "stops")) +
  geom_point(color = "orange", size=6) +
  theme_minimal()+
labs(
  x = "Number of Stops", 
  y = "Flight Price", 
  title = "Relationship Between Stops and Flight Price" 
)+
  scale_x_discrete(
    labels = c("Zero Stop", "One Stop", "Two or More Stops"),
    limits = c("Zero Stop", "One Stop", "Two or More Stops")
  )+  
  theme(
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),  
    axis.title.x = element_text(size = 16, face = "bold"), 
    axis.title.y = element_text(size = 16, face = "bold") 
  )
#fixed
pred_data <- ggpredict(mod1, terms = "stops")
View(pred_data)
library(dplyr)
mean_data <- dta %>%
  group_by(stops) %>%
  summarise(mean_price = mean(price, na.rm = TRUE))
ggplot(mean_data, aes(x = stops, y = mean_price)) +
  geom_point(color = "orange", size = 3) + 
  geom_errorbar(aes(ymin = mean_price - sd(mean_price), ymax = mean_price + sd(mean_price)), width = 0.2) + 
  scale_x_discrete(labels = c('Two or More','One','Zero'))+
  labs(
    x = "Number of Stops",  
    y = "Mean Price", 
    title = "Mean Comparison of Number of Stops and Price" 
  ) +
  theme_minimal() +  
  theme(
    axis.text.x = element_text(size = 14),  
    axis.text.y = element_text(size = 14), 
    axis.title.x = element_text(size = 16, face = "bold"), 
    axis.title.y = element_text(size = 16, face = "bold")
  )
#class
library(dplyr)
grouped_data <- dta %>%
  group_by(airline) %>%
  summarize(MeanPrice = mean(price))
airline_colors <- c("gold1", "gold2", "gold4", "orange2", "orange3", "goldenrod1")
ggplot(grouped_data, aes(x = airline, y = MeanPrice)) +
  geom_bar(stat = "identity", fill = airline_colors, alpha = 0.7) +
  scale_x_discrete(labels = c('Air India','Air Asia','Go First', 'Indigo', 'Spice Jet','Vistara'))+
  labs(x = "Airline", y = "Mean Price", title = "Mean Price Distribution Across Airlines")+
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold")) 
  theme_classic()
#animation(didn't use)
  install.packages(c("gifski","gapminder", "gganimate", "ggimage", "imager"))
  library(imager)
  library(ggimage)
  library(magick)
  library(tidyverse)
library(gganimate)
  library(png)
  library(gapminder)
  library(gifski)
  library(reshape2)
  library(Rcpp)
dta$departure_time <- as.factor(dta$departure_time)
#not using
animated_boxplot <- ggplot(data = dta, aes(x = duration, y = price, fill = class)) +
  geom_boxplot() +
  labs(title = "Relationship between Price and Departure Time by Class", x = "Duration", y = "Price", fill = "Class") +
  transition_states(class, transition_length = 2, state_length = 1) +
  enter_fade() +
  exit_fade() +
  theme_minimal()
animated_boxplot<-animated_boxplot+transition_states(class)  
animate(animated_boxplot, renderer = gifski_renderer())

#departure time
bar_plot <- ggplot(data = dta, aes(x = reorder(departure_time, price, FUN = mean), y = price)) +
  geom_bar(stat = "summary", fun = "mean", fill = "darkorange1") +
  scale_x_discrete(labels = c('Late Night','Afternoon','Early Morning', 'Evening', 'Morning','Night'))+
  labs(title = "Relationship between Departure Time and Price",
       x = "Departure Time",
       y = "Average Price") +
  theme_classic() +
  theme(
    plot.title = element_text(size = 20, face = "bold"), 
    axis.title.x = element_text(size = 16), 
    axis.title.y = element_text(size = 16),  
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14) 
  )
print(bar_plot)

#days_left
dta$days_left<-as.numeric(dta$days_left)
dta$days_left <- cut(dta$days_left, breaks = c(0, 16, 32, 49), labels = c("1-16", "17-32", "33-49"), right = TRUE)
dta$days_left <- factor(dta$days_left, levels = c("1-16", "17-32", "33-49"))

stacked_bar_plot <- ggplot(data = dta, aes(x = class, y = price, fill = days_left)) +
  geom_bar(stat = "identity") +
  labs(title = "Stacked Bar Plot of Price by Class and Days Left",
       x = "Class",
       y = "Price",
       fill = "Days Left") +
  theme_minimal() +
  scale_y_continuous(labels = label_number(big.mark = ",", decimal.mark = "."))  

#heatmap
library(plotly)
library(dplyr)
orange_colors <- c("orange", "yellow", "salmon3", "#FF4500")  
orange_palette <- brewer.pal(n = 9, name = "Oranges")
library(ggplot2)
library(RColorBrewer)
orange_palette <- brewer.pal(n = 9, name = "Oranges")
heatmap_plot <- ggplot(dta, aes(x = source_city, y = destination_city, fill = price)) +
  geom_tile() +
  scale_fill_gradientn(colors = orange_palette, limits = c(0, max(dta$price))) +
  labs(title = "Price Changes with Source and Destination",
       x = "Source City",
       y = "Destination City",
       fill = "Price") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  
        axis.text.y = element_text(size = 12),  
        axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16))  
print(heatmap_plot)
#scatter plot
model <- lm(price ~ duration + days_left, data = dta)
summary(model)
install.packages("sjPlot")
library(sjPlot)
plot_model(model, type = "std", title = "Comparative Standardized Effect")
library(tidyverse)
library(dotwhisker)
dwplot(model, y.name = "Price", title = "Coefficient Plot: Price V.S Duration + Days Left", font.size = 12, point.color="orange")
plot <- dwplot(model)
plot + 
  labs(x = "Price") + 
  theme(axis.title.y = element_text(size = 16))  
library(margins)
?cplot
margins(model)
cplot(model, "duration")
cplot(model, "days_left", main="Effect of Days Left on Flight Ticket Price", xlab = "Days Left", ylab="Price", sub="Calculated by subtracting the trip date by the booking date", se.fill="orange2",)

#class
ggplot(dta, aes(x = class, y = price, fill = class)) +
  geom_boxplot(outlier.size = 0.1) +
  labs(title = "Box Plot of Price by Class",
       x = "Class",
       y = "Price") +
  scale_fill_manual(values = c("Business" = "orange", "Economy" = "gold"))+
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14)
  )
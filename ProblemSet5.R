rm(list=ls())
set.seed(18552)
#Q1
movies<-read.csv("blockbuster-top_ten_movies_per_year_DFE.csv")
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
library(scales)
movies$WWG2 <- as.numeric(gsub('[$,]', '', movies$worldwide_gross))
movies$subset_check <- ifelse(movies$Genre_1 %in% c("Action", "Drama", "Comedy", "Romance", "Sci-Fi"), 1, 0)
movies2 <- subset(movies, year >= 1990 & subset_check == 1)
p1 <- ggplot(movies2, aes(x = year, y = WWG2, group = Genre_1, color = Genre_1)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +  
  labs(color = "Genre", x = "Year", y = "Total Worldwide Gross", subtitle = "{frame_along}") +
  ggtitle("Worldwide gross for genres of movies across time") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.text.x = element_text(angle = 90))
print(p1)
p1_anim <- p1 + transition_reveal(year)
animate(p1_anim, renderer = gifski_renderer())
anim_save("animated_plot.gif", animate(p1_anim, renderer = gifski_renderer()))
#Q2
library(htmlwidgets)
library(plotly)
library(crosstalk)
movies2$WWG2 <- as.numeric(gsub('[$,]', '', movies2$worldwide_gross))
p2 <- ggplot(movies2, aes(x = year, y = WWG2, group = Genre_1, color = Genre_1, frame = Genre_1)) +
  geom_point(aes(text = title, text2 = year, text3 = worldwide_gross)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) + 
  labs(color = "Genre", x = "Year", y = "Total Worldwide Gross") +
  ggtitle("Total Worldwide Gross by Genre \n for Most Popular Movies 1990-2014") +
  theme(plot.title = element_text(hjust = 0.5, size = 20), axis.text.x = element_text(angle = 90))
ggplotly(p2, tooltip = c("text", "text2", "text3"))
saveWidget(p2, file = "plotly_plot.html")
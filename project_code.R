###########################
# DEFAULT SETTINGS
###########################

knitr::opts_chunk$set(message = FALSE) # include this if you don't want markdown to knit messages
knitr::opts_chunk$set(warning = FALSE) # include this if you don't want markdown to knit warnings
knitr::opts_chunk$set(echo=TRUE) # set echo=FALSE to hide code from html output
options(digits = 4)
###########################
# LIBRARIES
###########################

library(tidyverse)
library(GGally)

MovieData = read_csv("MovieData - MovieData (3).csv")
MovieData

SuperHero_MovieData <- filter(MovieData, creative_type == "Super Hero")
SuperHero_MovieData

Action_MovieData <- filter(MovieData, genre == "Action")
Action_MovieData

Marvel_SuperHero_MovieData <- filter(MovieData, marvel == "Yes") 
Marvel_SuperHero_MovieData


#Action Movie Data

budget <- Action_MovieData$production_budget
domestic <- Action_MovieData$domestic_box_office
international <- Action_MovieData$international_box_office
plot(budget,domestic,
     main = "Domestic Budget vs Box office for action movies",
     xlab = "Movie Budget",
     ylab = "Domestic Box Office",
     pch = 20)
abline(lm(domestic~budget),
       col="red",
       lwd = 2)
cor(budget,domestic)
Action_lm <- lm(domestic~budget)
lm(international~budget)
summary(Action_lm)

qt(.05, df = 309)
1.40e+00 + c(-2,2)*6.41e-02

#Marvel Movie Data

budget <- Marvel_SuperHero_MovieData$production_budget
domestic <- Marvel_SuperHero_MovieData$domestic_box_office
international <- Marvel_SuperHero_MovieData$international_box_office
plot(budget,domestic,
     main = "Domestic Budget vs Box office for marvel movies",
     xlab = "Movie Budget",
     ylab = "Domestic Box Office",
     pch = 20)
abline(lm(domestic~budget),
       col="red",
       lwd = 2)
cor(budget,domestic)
Marvel_lm <-lm(domestic~budget)
summary(Marvel_lm)

qt(.025, df = 18)
2.30e+00 + c(-2.1,2.1)*6.41e-02


#Super Hero Movies

budget <- SuperHero_MovieData$production_budget
domestic <- SuperHero_MovieData$domestic_box_office
international <- SuperHero_MovieData$international_box_office
plot(budget,domestic,
     main = "Domestic Budget vs Box office for super hero movies",
     xlab = "Movie Budget",
     ylab = "Domestic Box Office",
     pch = 20)
abline(lm(domestic~budget),
       col="red",
       lwd = 2)
cor(budget,domestic)
cor(budget,international)
lm(domestic~budget)
SuperHero_lm <- lm(international~budget)
summary(SuperHero_lm)

qt(.025, df = 58)
2.76e+00 + c(-2,2)*3.69e-01

MovieDataMR = read_csv("MovieDataMR - MovieData.csv")
MovieDataMR

#Multiple Regression with all

movies_lm_domestic = lm(domestic_box_office ~ production_budget + I(creative_type == "Super Hero")+ I(genre == "Action")  + I(marvel == "Yes"), data = MovieDataMR)
summary(movies_lm_domestic)

#Multiple Regression with only Marvel

marvel_lm_domestic = lm(domestic_box_office ~ production_budget + I(marvel == "Yes"), data = MovieDataMR)
summary(marvel_lm_domestic)

MovieDataMR %>%
  ggplot(., aes(x = production_budget, y = domestic_box_office, group = marvel, col = marvel)) +
  geom_point() +
  geom_line(aes(y = marvel_lm_domestic$fitted.values)) +
  theme_bw() 
labs(
  x = "Production Budget ($)", 
  y = "Domestic Box Office ($)",
  title = "Domestic Box Office by Movie", 
  col = "Marvel Movie?" 
)
theme(text = element_text(size=24))

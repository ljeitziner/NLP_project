####################################
######### NLP Group project ########
### Sentiment analysis TED talks ###
####################################

###################
### Preparation ###
###################

# load packages
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(wordcloud)
library(stopwords)
library(tidyverse)
library(tidytext)
library(textdata)
library(PoSI)


# load data
data = read_csv("1_Data/ted.csv", locale = locale(encoding = "UTF-8"))


#exclude NA talks
data = data %>% filter(!transcript=="N/A", .preserve = TRUE)

#rename columns with strange names
data = data %>% rename(views=views_as_of_06162017)

#Selecting all the talks that were filmed from 2009 to 2016 and divide views by 1000.
### !!! USE date_published INSTEAD OF year_filmed !!! ###
talks <- data %>% filter(year_filmed >= 2009 & year_filmed < 2017) %>% 
  mutate(views_thousand = views/1000)


### analyses with log(views) or poisson ###
tilt_theme <- theme(axis.text.x=element_text(angle=45, hjust=1))

# normal (not log)
affect_normal <- ggplot(talks, aes(affect, views)) +
  geom_point() +labs(title= "Log(views) by % of affect words") + tilt_theme

affect_normal 


# using log(views)
affect <- ggplot(talks, aes(affect, log(views))) +
  geom_point() +labs(title= "Log(views) by % of affect words") + tilt_theme +
  labs(x = "% of affect words", y = "log(views)")

affect 

# positive emotion
pos <- ggplot(talks, aes(posemo, log(views))) +
  geom_point() +labs(title= "Log(views) by % of positive affect words") + tilt_theme

pos 


# poisson regression
poiss_aff <- glm(views ~ affect, family = "poisson", data = talks)
summary(poiss_aff)

# add predicted log(views) to talks
talks <- cbind(talks, pred_affect = predict(poiss_aff))


# plot the points (actual observations), regression line
plot_affect <- ggplot(talks, aes(affect,log(views))) + 
  geom_point() +
  geom_smooth(aes(affect, pred_affect)) +
  labs(title= "Log(views) by % of affect words", x = "% of affect words", y = "log(views)")

plot_affect


# poisson regression of positive affect
poiss_pos <- glm(views ~ posemo, family = "poisson", data = talks)
summary(poiss_pos)

# add predicted log(views) to talks
talks <- cbind(talks, pred_posemo = predict(poiss_pos))

# plot the points (actual observations), regression line
plot_pos <- ggplot(talks, aes(posemo,log(views))) + 
  geom_point() +
  geom_smooth(aes(affect, pred_posemo)) +
  labs(title= "Log(views) by % of positive affect words", x = "% of positive affect words", y = "log(views)")

plot_pos

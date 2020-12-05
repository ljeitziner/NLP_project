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
data = read_csv("Data/ted.csv", locale = locale(encoding = "UTF-8"))


#exclude NA talks
data = data %>% filter(!transcript=="N/A", .preserve = TRUE)

#rename columns with strange names and divide views by 1'000 
data = data %>% rename(views=views_as_of_06162017) %>% mutate(views_thousand = views/1000)

### WORKING ON THIS PART ### 

#Convert the variable "date_published" into an actual date 
data$date_published <- as.Date(data$date_published, format = "%m/%d/%y") 

#Select all the talks published between 2006 and 2016. 
#We use date_published instead of year_filmed. 

data %>% filter(data$date_published != "2017")
View(data)
talks <- data 
### UNTIL HERE ### 



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


### 10% TALKS AND AFFECT 

top_10talks<- talks %>% select("id", "headline", "speaker", "date_published", "duration", "views") %>% arrange(desc(talks$views_thousand)) %>% head(123)

tilt_theme <- theme(axis.text.x=element_text(angle=45, hjust=1))

top <- ggplot(top_10talks, aes(headline, views, fill= views)) +
  geom_col() +labs(title= "10 most viewed talks") + tilt_theme






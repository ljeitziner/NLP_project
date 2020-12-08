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
library(RColorBrewer)


# load data
data = read_csv("data/ted.csv", locale = locale(encoding = "UTF-8"))


#exclude NA talks
data = data %>% filter(!transcript=="N/A", .preserve = TRUE)

#rename columns with strange names and divide views by 1'000 
data = data %>% rename(views=views_as_of_06162017) %>% mutate(views_thousand = views/1000)

### WORKING ON THIS PART ### 

#Convert the variable "date_published" into an actual date 
data$date_published <- as.Date(data$date_published, format = "%m/%d/%y") 

#Select all the talks published between 2006 and 2016. 
#We use date_published instead of year_filmed. 


talks = data %>% filter(date_published > "2005-12-31" & date_published < "2017-01-01")

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
  labs(x = "% of affect words in a talk", y = "log(views)")

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
  labs(title= "Log(views) by % of affect words", x = "% of affect words in a talk", y = "log(views)")

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
  labs(ttopitle= "Log(views) by % of positive affect words", x = "% of positive affect words", y = "log(views)")

plot_pos


### new variable for number of days online
talks = talks %>% 
  mutate(days_online = as.Date("2017-06-16")-date_published)

range(talks$days_online)
# 175-4007d

# control for days_online
poiss_aff_t = glm(views ~ affect + days_online, data = talks, family = "poisson")
summary(poiss_aff_t)

# only days_online as predictor
poiss_t = glm(views ~ days_online, data = talks, family = "poisson")
summary(poiss_t)

# add predicted log(views) to talks
talks <- cbind(talks, pred_t = predict(poiss_t))


plot_t = ggplot(talks, aes(days_online,log(views))) + 
  geom_point() + geom_smooth(aes(days_online, pred_t)) +
  labs(title = "Log(views) by number of days since online publication", x = "Number of days since online publication")

plot_t


## from log (which is natural logarithm) to number: e^log
# e = 2.71828189


### 10% TALKS AND AFFECT 

# top_10talks <- talks %>% 
#   select("id", "headline", "speaker", "date_published", "duration", "views_thousand", "affect") %>% 
#   arrange(desc(talks$views_thousand)) %>% head(123)


# alternative to choosing top/bottom 10%
top_10talks <- talks %>% 
  select("id", "headline", "speaker", "date_published", "year_filmed", "duration", "views", "views_thousand", "affect") %>% 
  arrange(desc(views)) %>% head(0.1*nrow(talks))

top <- ggplot(top_10talks, aes(affect, log(views))) +
  geom_point() +labs(title= "10 percent most viewed talks", x= "% of affect words in a talk", y = "log(views")


bottom_10talks <- talks %>% 
  select("id", "headline", "speaker", "date_published", "year_filmed", "duration", "views", "views_thousand", "affect") %>% 
  arrange(views) %>% head(0.1*nrow(talks))


bottom <- ggplot(bottom_10talks, aes(affect, log(views))) +
  geom_point() + labs(title= "10 percent least viewed talks", x= "% of affect words in a talk", y = "log(views)")


# plot with top and bottom combined
topbottom = ggplot() +
  geom_point(data = top_10talks, aes(affect, log(views), color = "Top 10%")) +
  geom_point(data = bottom_10talks, aes(affect, log(views), color = "Bottom 10%")) +
  labs(color = " ", title = "Top and bottom 10% of talks", x = "% of affect words in a talk")


# statistical analysis of top vs. bottom
t.test(top_10talks$affect, bottom_10talks$affect, alternative = "two.sided", mu = 0)






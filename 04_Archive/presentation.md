Markdown presentation
================
Loris Jeitziner, Olivia Fischer and Nieves Schwab
11/24/2020

## Emotional Arcs in TED Talks

What does a great commercial, a sales pitch and a scientific
presentation have in common?

They all try to tell (or sell) a compelling story.

Stories are appealing to us, as they use a tight and logical structure,
they help us organize information. It has a logical progression, the
classic story structure always looks the same pattern, first the context
is set, and the hero is introduced. The hero has a problem or a
challenge he has to solve. He struggles, then struggles more and finally
he resolves the crises. Happy end.

This ups and downs in the stories are an important element, they capture
the audience’s interest and take them on an emotional journey. Kurt
Vonnegut, proposed that there exist six “universal shapes” of stories,
or emotional arcs, that follow specific pattern in stories.

We propose that emotion plays an important role in storytelling. And we
wanted to analyze to which extent. To this purpose we have been looking
at data from the masters of storytelling, the speakers of TED Talks.

According to Wikipedia: TED conferences (Technology, Entertainment,
Design) is a media organization that posts talks online for free
distribution, under the slogan “ideas worth spreading”. They address a
wide range of topics within the research and practice of science and
culture, often thorugh storytelling. The speakers are given a maximum of
18 minutes to present ideas in the most innovative and engaging way as
they can.

The dataset that we analyze contains 2,386 TED Talks from the years 1972
to 2017. It contains variables like the speaker, headline of the talk,
duration, year of publication and… the number of views\!

In the last two weeks we have been analyzing this dataset in a
data-driven manner, exploring if emotion/valence has an influence on the
number of views a talk has. For this purpose we have defined the
variable of views as our DV, defining talks with more views are more
successful.

Additional variables contained in the dataset stem from LIWC. They are,
for example: word count, tone, number of pos (e.g. ppron), affect,…

\#number of emotional words in the headline or description of the video

# 1 - Overview: Let’s look at the data:

Actually, there are 45 TED Talks that contain the word “story” in their
title. Coincidence? We don’t think so\! Stories are everywhere, and the
emotions conveyed in a story are the essence of a talk.

``` r
storytelling <- talks %>% select("headline", "id") %>% 
  filter(str_detect(headline, "story"))
```

What are the 25 most popular/viewed talks? Ken Robinson’s talk on “Do
Schools Kill Creativity?” is the most popular talk of all time with
impressive 45’622’906 views. There are 6 talks that have crossed the 20
million mark.

``` r
summary(data$views)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##   138915   756080  1125707  1695917  1702883 45622906

``` r
top_10talks<- talks %>% select("id", "headline", "speaker", "year_filmed", "duration", "views") %>% arrange(desc(talks$views_thousand)) %>% head(10)
last_10talks<- talks %>% select("id", "headline", "speaker", "year_filmed", "duration", "views") %>% arrange(talks$views_thousand) %>% head(10)

top_worst_talks = rbind(top_10talks, last_10talks)
top_worst_talks$top = "top"
top_worst_talks[11:20,"top"] = "worst"

tilt_theme <- theme(axis.text.x=element_text(angle=45, hjust=1))

top <- ggplot(top_10talks, aes(headline, views, fill= views)) +
  geom_col() +labs(title= "10 most viewed talks") + tilt_theme

top
```

![](presentation_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

The least viewed talk “Plant fuels that could power a jet” has 138’915
views.

``` r
last<- ggplot(last_10talks, aes(headline, views, fill= views)) +
  geom_col() +labs(title= "10 less viewed talks") + tilt_theme
last
```

![](presentation_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# comp<- ggplot(top_worst_talks, aes(headline, views, fill= views)) +
#   geom_col() +labs(title= "10 less viewed talks") + tilt_theme + facet_grid(.~top)
# comp


# talks_hist = talks %>% select("id", "headline", "speaker", "year_filmed", "duration", "views") %>% arrange(desc(talks$views_thousand))
# comp<- ggplot(talks_hist, aes(id, id)) +
#   geom() +labs(title= "Histogram of views") + tilt_theme
# comp
```

## Does sentiment matter? Does it impact succes (views)?

Positive Emotions (love, nice, sweet) –\> The value of positive emotion
of a talk is calculated as a percentage of all the words in that talk.
For example, 4.2 % of all words in the talk were positive emotions.
![](presentation_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->![](presentation_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

## What’s the connection to number of views?

![](presentation_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->![](presentation_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->
\#\# The histogramm of the views tells us, that the data is heavily
skewed

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](presentation_files/figure-gfm/unnamed-chunk-9-1.png)<!-- --> \#\#
*Conclusion*: We cannot find a connection between sentiment and success
solely by a descriptive analysis\!

# 2 - Do Emotional Arcs in TED Talks predict success?

We are interested in discovering if there are distinct emotional arcs
and if they can predict the number of views a TED Talk will have. To
plot the sentiment arcs first we have to tokenize the transcripts of the
talks and add the sentiment values.

## Tokenize the data and add sentiment

``` r
#tokenize the transcripts 
token_tbl = unnest_tokens(data, input = "transcript", token = "words", to_lower = TRUE, output = "word")

#relative position of the word for the sentiment analysis -> arc
token_tbl = token_tbl %>% group_by(id) %>% mutate(
  pos = 1:n(),
  rel_pos = (pos-1)/max(pos-1)
)

#load the sentiment word from the "afinn" repository
afinn = get_sentiments("afinn")

#join the tokens with the sentiment words
token_tbl <- inner_join(y = afinn,x = token_tbl)
```

## Smooth sentiment arcs

``` r
# smoothing function for smoothing the sentiment values relative to the position
smooth = function(pos, value){ 
  sm = sapply(pos, function(x) {
    weights = dnorm(pos, x, max(pos) / 10)
    sum(value * (weights / sum(weights)))
    })
  }

# define the sentiment values as sent_values per talk
token_tbl = token_tbl %>% group_by(id) %>% mutate(
  sent_values = smooth(pos, value)
)
```

![](presentation_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

![](presentation_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

# 3 - Cluster Analysis: We tried to analyze TED Talks inspired by Reagan (2016)

By cluster analysis we try to find a certain “arciness” and want to
examine, whether some arcs are more successful than others.

We used the daisy function for calculating euclidian distances.
Furthermore, with the k-mediods approach, we built clusters.

``` r
#setting seed for clustering
set.seed(240)  

#calculate euclidian distances
dist_data <- daisy(t_wide, stand=TRUE)

# cluster analysis with fast k mediod method -> 10 clusters
# cluster_analysis_10 <- fastkmed(dist_data, ncluster = 10, iterate = 5000)
# cluster_analysis <- fastkmed(dist_data, ncluster = 8, iterate = 5000)
cluster_analysis <- pam(dist_data, k=8 )
```

The clusters actually show some tendencies, that there are multiple arcs
in TED - Talks.

![](presentation_files/figure-gfm/visualize%20clusters-1.png)<!-- -->

## Let’s take a closer look at two clusters.

Cluster 7 shows more of a positive linear function, whereas cluster 2
looks like a negative quadratic function.

![](presentation_files/figure-gfm/visualize%20clusters2-1.png)<!-- -->

## What’s the distribution of views in the seperate clusters?

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](presentation_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

*Conclusion:* They are still heavily skewed.

## But do clusters predict number of views?

|                 | Estimate | Std. Error | t value  |       Pr(\>|t|)        |
| :-------------: | :------: | :--------: | :------: | :--------------------: |
| **(Intercept)** | 1726805  |   193262   |  8.935   | 0.00000000000000000134 |
|  **cluster2**   | \-87102  |   285021   | \-0.3056 |          0.76          |
|  **cluster3**   | \-63526  |   291639   | \-0.2178 |         0.8276         |
|  **cluster4**   |  346968  |   330599   |   1.05   |         0.2941         |
|  **cluster5**   |  42283   |   374984   |  0.1128  |         0.9102         |
|  **cluster6**   |  76459   |   313534   |  0.2439  |         0.8074         |
|  **cluster7**   |  866016  |   295333   |  2.932   |        0.003422        |
|  **cluster8**   | \-135270 |   293450   | \-0.461  |         0.6449         |

| Observations | Residual Std. Error | \(R^2\) | Adjusted \(R^2\) |
| :----------: | :-----------------: | :-----: | :--------------: |
|     1326     |       2962653       | 0.01153 |     0.006277     |

Fitting linear model: views \~ cluster

*Conclusion:* This preliminary analysis actually indicates, that cluster
7 significantly differs form the other clusters. But do we believe, that
some clusters are more predictive of success (numbers of views) than
others? *NO*

## Possible reason for this outcome

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](presentation_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

*Conclusion:* The data is heavily skewed. In Cluster 7 we see some
really viral TED talks, which most likely explains the results of the
linear model.

# 4 - Autocorrelation: Maybe there is another way examine, whether there is a connection between sentiment and success?

(Mean) Autocorrelation of consecutive sentiment values to predict views
would be another approach.

## Autocorrelation instead of arcs as a measure of “emotional structure”?

Autocorrelation here: A higher-than-chance correlation between 2
consecutive sentiment values due to their temporal dependence in the TED
talk.

``` r
# calculate new position variable from 1 to however many sentiment words a talk has
data_cut = data_cut %>% group_by(id) %>%
  mutate(sent_pos = 1:length(word))
# add new column `chunk2` to data_cut (currently empty)
data_cut = data_cut %>% add_column(chunk = 0)
# loop to calculate chunks of equal length within a talk
# using sent_pos as indicator of what row to start at
for(i in unique(data_cut$id)){
  #number of rows for each talk
  a = length(data_cut$id[data_cut$id == i])[[1]]
  # split talk into 10% chunks (arbitrary %)
  data_cut[("sent_pos" == 1):(0.1*a), "chunk"][[1]] = 1
  data_cut[(0.1*a+1):(0.2*a), "chunk"][[1]] = 2
  data_cut[(0.2*a+1):(0.3*a), "chunk"][[1]] = 3
  data_cut[(0.3*a+1):(0.4*a), "chunk"][[1]] = 4
  data_cut[(0.4*a+1):(0.5*a), "chunk"][[1]] = 5
  data_cut[(0.5*a+1):(0.6*a), "chunk"][[1]] = 6
  data_cut[(0.6*a+1):(0.7*a), "chunk"][[1]] = 7
  data_cut[(0.7*a+1):(0.8*a), "chunk"][[1]] = 8
  data_cut[(0.8*a+1):(0.9*a), "chunk"][[1]] = 9
  data_cut[(0.9*a+1):a, "chunk"][[1]] = 10
}
```

### Error message: Any tips?

**How can we partition talks (rather: sentiment words in talks) into 10
(or 20, 30…) chunks where all the chunks *within* a talk are of equal
length?** (Equal length of vectors within a talk necessary to calculate
autocorrelation) \#\# *If* the loop had worked… Autocorrelations
between: \* chunks 1 and 2 \* chunks 2 an 3 \* … \* chunks 9 and 10
**The higher the mean autocorrelation, the more views a TED talk?**

``` r
# calculate autocorrelations
autocor = data_cut %>% group_by(id) %>%
  mutate(cor12 = cor(sent_values[chunk2 == 1], sent_values[chunk2 == 2]),
         cor23 = cor(sent_values[chunk2 == 2], sent_values[chunk2 == 3]),
         cor34 = cor(sent_values[chunk2 == 3], sent_values[chunk2 == 4]),
         cor45 = cor(sent_values[chunk2 == 4], sent_values[chunk2 == 5]),
         cor56 = cor(sent_values[chunk2 == 5], sent_values[chunk2 == 6]),
         cor67 = cor(sent_values[chunk2 == 6], sent_values[chunk2 == 7]),
         cor78 = cor(sent_values[chunk2 == 7], sent_values[chunk2 == 8]),
         cor89 = cor(sent_values[chunk2 == 8], sent_values[chunk2 == 9]),
         cor910 = cor(sent_values[chunk2 == 9], sent_values[chunk2 == 10]))
# calculate mean correlations
autocor = autocor %>% group_by(id) %>%
  mutate(cor_mean = mean(c(cor12, cor23, cor34, cor45, cor56, cor67, cor78, cor89, cor910)))
# one row per talk with only relevant variables
autocor = autocor %>%
  filter(sent_pos == 1) %>%
  select(id, speaker, views, cor12, cor23, cor34, cor45, cor56, cor67, cor78, cor89, cor910, cor_mean)
# visualize
plot(autocor$cor_mean, autocor$views)
# predictive power of mean autocorrelation (`cor_mean`)?
lm_autocor <- lm(views ~ cor_mean, data = autocor)
summary(lm_autocor)
```

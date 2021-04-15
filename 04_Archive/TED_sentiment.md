TED\_sentiment
================
Jeitziner Loris
02/11/2020

# Load texts and inner join them

``` r
#load data
data = read.csv(file="data/ted.csv", encoding = "UTF-8")
```

# Tokenize and add sentiment

``` r
#exclude NA talks
data = data %>% filter(!transcript=="N/A", .preserve = TRUE)

#rename columns with strange names
data = data %>% rename(id = X.U.FEFF.id)
data = data %>% rename(views=views_as_of_06162017)

#tokenize the transcripts 
token_tbl = unnest_tokens(data, input = "transcript", token = "words", to_lower = TRUE, output = "word")



#relative position of the word for the sentiment analysis -> ark
token_tbl = token_tbl %>% group_by(id) %>% mutate(
  pos = 1:n(),
  rel_pos = (pos-1)/max(pos-1)
)


#load the sentiment word from the "afinn" repository
afinn = get_sentiments("afinn")

#join the tokens with the sentiment words
token_tbl <- inner_join(y = afinn,x = token_tbl)
```

    ## Joining, by = "word"

``` r
# smoothing function for smoothing the sentiment values relative to the position
smooth = function(pos, value){ 
  sm = sapply(pos, function(x) {
    weights = dnorm(pos, x, max(pos) / 10)
    sum(value * (weights / sum(weights)))
    })
  }

# define the sentiment values as sent_valuees per talk
token_tbl = token_tbl %>% group_by(id) %>% mutate(
  sent_values = smooth(pos, value)
)
```

# Plot sentiment arks

## the following plots illustrate examples for TED talks from Al Gore.

``` r
# plot sentiment arcs not z transformed to compare, color by title
token_tbl %>% filter(speaker=="Al Gore"  ) %>% ggplot(aes(rel_pos, sent_values,color=event)) +
  geom_line(lwd=2) + 
  labs(x = "Position", y = 'Sentiment') + 
  theme_minimal()
```

![](TED_sentiment_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

# Calculate models for arcs

The idea here is, that all 6 arcs can be illustrated as models. The
first two arcs are linear models (positive/negative). The third and
forth are quadratic (positive/negative), and the last two are cubic
(positive/negative) or sinus/cosinus functions (still unclear)

We just have to calculate all the models for each sentiment sequence to
see, which model fits best. Then we have the best model for each TED
talk.

``` r
# remove NA talks
token_tbl = token_tbl[!is.na(token_tbl$sent_values),]


# unique IDs in vector for loop through talks
talks = unique(token_tbl$id)

#create matrix for r-squared values
model_matrix = as.data.frame(matrix(data=NA, nrow= length(talks), ncol = 1))

#rename the rownames in the model matrix by the talks ID's
rownames(model_matrix)=talks


# -> zusätzlich noch steigung (beta) in model_matrix einfügen, damit wir zwischen negativ/positiv unterscheiden können


#loop through talks an calculate models
#extract r squared values und write them into the model matrix
for(i in talks){
  #linear pos
  x=token_tbl[token_tbl$id==i,"rel_pos"][[1]]
  y=token_tbl[token_tbl$id==i,"sent_values"][[1]]
  # print(i)
  model = lm(formula="y ~ x")
  model_matrix[i,1]= summary(model)[["r.squared"]][1]
  
  #quadratisch
  model2 = lm(y ~ x + I(x^2))
  model_matrix[i,2]= summary(model2)[["r.squared"]][1]
  
  #kubisch -> möglichkeit abzunändern zu sinus/cosinus funktion
  # x3 = poly(x,degree=3))
  model3 = lm(y ~ x + I(x^2) + I(x^3))
  model_matrix[i,3]= summary(model3)[["r.squared"]][1]
}
```

    ## Warning in summary.lm(model): essentially perfect fit: summary may be unreliable

    ## Warning in summary.lm(model2): essentially perfect fit: summary may be
    ## unreliable

    ## Warning in summary.lm(model3): essentially perfect fit: summary may be
    ## unreliable

    ## Warning in summary.lm(model): essentially perfect fit: summary may be unreliable

    ## Warning in summary.lm(model2): essentially perfect fit: summary may be
    ## unreliable

``` r
# token_tbl %>% group_by(id) %>% filter(id==179) %>% select(z_sent_values)
```

``` r
# round r squared
model_matrix = round(model_matrix, 5)

# compare latest models as an example
anova(model, model2)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: y ~ x
    ## Model 2: y ~ x + I(x^2)
    ##   Res.Df    RSS Df Sum of Sq      F Pr(>F)
    ## 1     77 6.8867                           
    ## 2     76 6.8821  1 0.0046004 0.0508 0.8223

``` r
anova(model2, model3)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: y ~ x + I(x^2)
    ## Model 2: y ~ x + I(x^2) + I(x^3)
    ##   Res.Df    RSS Df Sum of Sq      F                Pr(>F)    
    ## 1     76 6.8821                                              
    ## 2     75 2.5599  1    4.3222 126.63 < 0.00000000000000022 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
anova(model, model3)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: y ~ x
    ## Model 2: y ~ x + I(x^2) + I(x^3)
    ##   Res.Df    RSS Df Sum of Sq      F                Pr(>F)    
    ## 1     77 6.8867                                              
    ## 2     75 2.5599  2    4.3268 63.383 < 0.00000000000000022 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
#plot 
plot(x,y)
```

![](TED_sentiment_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

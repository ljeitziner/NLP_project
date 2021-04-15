# NLP_project

For the Masters Seminar "Natural Language Processing" our group aimes to analyse the sentiment arcs of TED talks.

According to an article of Reagan (2016), there are six different emotional arcs in telling a story. 

* rags to riches (rise -> positive linear function)
* tragedy (fall -> negative linear function)
* man in a hole (fall-rise -> positive quadratic function)
* icarus (rise-fall -> negative quadratic function)
* cinderella (rise-fall-rise -> sinus function)
* oedipus (fall-rise-fall -> cosinus function)

We belive, that the more applicable a sentiment arc of a TED talk to the six types is, the more succesfull it was (measured in views). 

To operationalize our idea, we calculated emotional arcs of multiple TED talks from Kaggle TED-talks data.
We analyze, how good the emotional arcs fit to linear, quadratic or sinus/cosinus models. 
By comparison of the calculated r.squared values, we will be able to (1) classify the TED talks according to Reagan (2016) and (2) predict the number of views through the goodness of fit. 

# Our questions

* Are our assumptions about the six different emotional arcs applicable to our proposed method of classifying the TED talks?
* Is r.squared a proper value to compare models in this regard?
* For the last two arcs, is it possible to define the formula in R like this for fitting our arc to the sinus/cosinus courve?: lm(y ~ cos(x)) / lm(y ~ sin(x))

# Feedback from Dirk (10.11.20)

* Before we do anything else, first define **what exactly it is that we want to find out**. (E.g., when we say "clearness" of arcs, what does that mean?) And based on our research question, do the analyses that can best answer our question.
* Whatever our question is, do step-by-step and easy-to-follow analyses building up to the main question (e.g., does emotion play any roll in how successful a talk is?)
* Sticking to the 6 arcs may be too top-down -> go for a more data-driven approach (unless there is good reason to expect that one of these arcs will best predict views)
* Suggestions: 
  * Develop measure for general "arc-iness", i.e. if a given talk adheres to some temporal-dependent emotional structure (auto-correlation)
  * Analyze extreme talks (best and worst-performing talks): What (emotional arc) characterizes them?
  * Splice each talk into x components and calculate an arc based on the development of (mean) emotion in each component (like Reagan (2016)?)

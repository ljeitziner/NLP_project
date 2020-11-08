# NLP_project

For the Masters Seminar "Natural Language Processing" our group aimes to analyse the sentiment arcs of TED talks.

According to an article of Reagan (2016), there are six different emotional arcs in telling a story. 

* rags to riches (rise -> positive linear function)
* tragedy (fall -> negative linear function)
* man in a hole (fall-rise -> positive quadratic function)
* icarus (rise-fall -> negative quadratic function)
* cinderella (rise-fall-rise -> sinus function)
* oedipus (fall-rise-fall -> cosinus function)

We belive, that the more applicable a sentiment arc of a TED talk to the six types is, the more succesfall it was (measured in views). 

To operationalize our idea, we calculated emotional arcs of multiple TED talks from Kaggle TED-talks data.
We analyze, how good the emotional arcs fit to linear, quadratic or sinus/cosinus models. 
By comparison of the calculated r.squared values, we will be able to (1) classify the TED talks according to Reagan (2016) and (2) predict the number of views through the goodness of fit. 

# Our questions

* Are our assumptions about the six different emotional arcs applicable?
* Is r.squared a proper value to compare models in this regard?
* other
*

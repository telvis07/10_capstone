# Using text analysis techniques to build a predictive text model
Telvis Calhoun  
June 10, 2016  

## Executive Summary

[Swiftkey](https://swiftkey.com/en) is a text messaging application that predicts the next word as you type a message. When the user types: `"I went to the "` : the application presents three options for what the next word might be. For example, the three words might be `gym, store, restaurant`.

In this project, we use `R` text mining tools to build a application that predicts the next word entered into an [shiny application](http://shiny.rstudio.com/). We use data scraped from blogs, twitter and news sites to build a language model to perform prediction. We predict the next word by calculating the [maximum likelihood estimate](https://en.wikipedia.org/wiki/Maximum_likelihood) (MLE) based on the language model.

### TL;DR

In a nutshell, here's a summary of the data analysis performed in this report.

1. Load the raw data.
2. Extract a 1% subsample of the data.
3. Preprocess the data to remove stopwords, convert to lowercase and other tasks
4. Generate 2-grams, 3-grams and 4-grams.
5. Build a n-gram language model using a suffix tree
6. Predict the next word by calculating the maximum likelihood estimate (MLE) for all suffixes for a search string.




## Complete Dataset
The [Data Science Capstone Course](https://www.coursera.org/learn/data-science-project/home/welcome) provided text data from 3 data sources: `blogs`, `twitter` and `news`. The table below shows the number of lines from data source.





|filename          | line_count|
|:-----------------|----------:|
|en_US.blogs.txt   |     899288|
|en_US.news.txt    |    1010242|
|en_US.twitter.txt |    2360148|
|Total             |    4269678|

## Processing Unstructured Text for Analysis
We use [a framework for text mining applications within R](https://cran.r-project.org/web/packages/tm/index.html) to transform the unstructured text to a structured document-by-term matrix format required for analysis. The first step is "clean" the text with a series of text processing functions. We use the following preprocessing steps.

- Remove all Punctuation
- Remove all Numbers
- Convert all words to Lowercase
- Remove English Stopwords
- Strip extra whitespace
- Remove Profanity

Second, we tokenize the text into [ngrams](https://en.wikipedia.org/wiki/N-gram). For our analysis, a `gram` equals a whitespace delimited sequence of characters corresponding to an English word. The `N` in ngram corresponds to the number of  words considered to be part of the same unit. For example: a 1-gram is `new`, 2-gram is `new york`, 3-gram is `new york city` and a 4-gram is `new york city police`.

Finally, we build a document-by-term matrix by transforming the ngrams to a [bag-of-words model](https://en.wikipedia.org/wiki/Bag-of-words_model). The columns in a docterm matrix represent unique ngram. The rows represent a document and the frequency that each ngram appears in the document. 

## Sampling the Dataset
The `4,269,678` lines from the complete dataset can be memory intensive for the text mining tools and slow the analysis. To speed things up, we subsample `1%` of the complete dataset and then work with the subsampled data for exploration and modeling. The subsampling implementation is in `Appendix 1`. 





|source  | num_lines| num_unique_words| median_word_freq| mean_word_freq|
|:-------|---------:|----------------:|----------------:|--------------:|
|twitter |     23602|             8040|                9|             20|
|blogs   |      8993|            12414|                6|             15|
|news    |     10103|            12850|                7|             15|



We see that the `mean` word frequency is nearly twice the `median` frequency for all data sources. The word frequency distribution has a [long tail](https://en.wikipedia.org/wiki/Long_tail) - as seen in the plot below. 
937 of 22899 (4.1%) words cover 50% of all word instances in the dataset. The `mean` word frequency is heavily weighted by a few words that occur very frequently.

![](capstone_milestone_report_1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


### NGram Frequencies
Now we will look at the top bigrams and trigrams in the dataset.

#### Top Bigrams
The top bigrams include places like: `new york` and common phrases like: `happy birthday` and `last night`.
![](capstone_milestone_report_1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

#### Top Trigrams
The top trigrams include places like: `new york city` and common phrases like: `happy mothers day` and `rock n roll`.

![](capstone_milestone_report_1_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

<!-- ### Top 4-grams -->
<!-- The top 4-grams do not appear to be commonly occurring phrases but reflect frequently occuring lines in the corpus. I will investigate using a weighting technique like [TD-IDF](https://en.wikipedia.org/wiki/Tf%E2%80%93idf) to account for frequently occuring lines. -->

<!-- ```{r, echo=FALSE, cache=TRUE} -->
<!-- ngram_4 <- get_docterm_matrix(docs, 4) -->
<!-- p4 <- generate_word_frequency_plot(ngram_4$wf, "Top 4-grams for Sampled Text") -->
<!-- print(p4) -->
<!-- ``` -->

## Language Model for Word Prediction

A suffix tree is a data structure where an interior node represents a "root" character sequence from the training data and child node represents a suffix of that root. For our implementation, each node represents a word and an arc (e.g. search path) represents a bigram, trigram or 4-gram. Each node has an associated count that represents the ngram frequency from the training data. We use these counts to predict the next word by calculating the maximimum likelihood estimate (MLE). 

### Word Prediction for: 'data'
For example: in the figure below we show a suffix tree for ngrams that begin with the word `data`. The first level (w1) of the tree corresponds to the 1-gram "data". The second level (w2) represents all bigrams whose root is "data" : such as "data entry" or "data streams". The same applies to the third (w3) and fourth (w4) levels for trigrams and 4-grams respectively.

<!--html_preserve--><div id="htmlwidget-296" style="width:672px;height:480px;" class="grViz html-widget"></div>
<script type="application/json" data-for="htmlwidget-296">{"x":{"diagram":"digraph {\n\ngraph [rankdir = \"TB\", label = \"Tree Lookup for: data\", fontsize = \"40\"]\n\nnode [style = \"filled,rounded\", shape = \"box\", fillcolor = \"GreenYellow\", fontname = \"helvetica\"]\n\nedge [arrowhead = \"vee\", color = \"grey35\", penwidth = \"2\"]\n\n  \"start\" [label = \"start\", tooltip = \"- name: start\"] \n  \"data\" [label = \"data\", tooltip = \"- freq: 198\"] \n  \"entry\" [label = \"entry\", tooltip = \"- freq: 12\n- word: data entry\"] \n  \"just\" [label = \"just\", tooltip = \"- freq: 6\n- word: data entry just\"] \n  \"overwhelming\" [label = \"overwhelming\", tooltip = \"- freq: 6\n- word: data entry just overwhelming\"] \n  \"respond\" [label = \"respond\", tooltip = \"- freq: 6\n- word: data entry respond\"] \n  \"emails\" [label = \"emails\", tooltip = \"- freq: 6\n- word: data entry respond emails\"] \n  \"streams\" [label = \"streams\", tooltip = \"- freq: 10\n- word: data streams\"] \n  \"live\" [label = \"live\", tooltip = \"- freq: 10\n- word: data streams live\"] \n  \"race\" [label = \"race\", tooltip = \"- freq: 10\n- word: data streams live race\"] \n  \"recovery\" [label = \"recovery\", tooltip = \"- freq: 8\n- word: data recovery\"] \n  \"software\" [label = \"software\", tooltip = \"- freq: 8\n- word: data recovery software\"] \n  \"showed\" [label = \"showed\", tooltip = \"- freq: 8\n- word: data recovery software showed\"] \n  \"dating\" [label = \"dating\", tooltip = \"- freq: 7\n- word: data dating\"] \n  \"average\" [label = \"average\", tooltip = \"- freq: 7\n- word: data dating average\"] \n  \"year\" [label = \"year\", tooltip = \"- freq: 7\n- word: data dating average year\"] \n  \"personalize\" [label = \"personalize\", tooltip = \"- freq: 7\n- word: data personalize\"] \n  \"user\" [label = \"user\", tooltip = \"- freq: 7\n- word: data personalize user\"] \n  \"interface\" [label = \"interface\", tooltip = \"- freq: 7\n- word: data personalize user interface\"] \n  \"records\" [label = \"records\", tooltip = \"- freq: 7\n- word: data records\"] \n  \"show\" [label = \"show\", tooltip = \"- freq: 7\n- word: data records show\"] \n  \"since\" [label = \"since\", tooltip = \"- freq: 7\n- word: data records show since\"] \n  \"allowance\" [label = \"allowance\", tooltip = \"- freq: 6\n- word: data allowance\"] \n  \"htc\" [label = \"htc\", tooltip = \"- freq: 6\n- word: data allowance htc\"] \n  \"one\" [label = \"one\", tooltip = \"- freq: 6\n- word: data allowance htc one\"] \n  \"bus\" [label = \"bus\", tooltip = \"- freq: 5\n- word: data bus\"] \n  \"inverted\" [label = \"inverted\", tooltip = \"- freq: 5\n- word: data bus inverted\"] \n  \"inversion\" [label = \"inversion\", tooltip = \"- freq: 5\n- word: data bus inverted inversion\"] \n  \"soon\" [label = \"soon\", tooltip = \"- freq: 4\n- word: data soon\"] \n  \"much\" [label = \"much\", tooltip = \"- freq: 4\n- word: data soon much\"] \n  \"going\" [label = \"going\", tooltip = \"- freq: 4\n- word: data soon much going\"] \n\"start\"->\"data\"\n\"data\"->\"entry\"\n\"data\"->\"streams\"\n\"data\"->\"recovery\"\n\"data\"->\"dating\"\n\"data\"->\"personalize\"\n\"data\"->\"records\"\n\"data\"->\"allowance\"\n\"data\"->\"bus\"\n\"data\"->\"soon\"\n\"entry\"->\"just\"\n\"entry\"->\"respond\"\n\"streams\"->\"live\"\n\"recovery\"->\"software\"\n\"dating\"->\"average\"\n\"personalize\"->\"user\"\n\"records\"->\"show\"\n\"allowance\"->\"htc\"\n\"bus\"->\"inverted\"\n\"soon\"->\"much\"\n\"just\"->\"overwhelming\"\n\"respond\"->\"emails\"\n\"live\"->\"race\"\n\"software\"->\"showed\"\n\"average\"->\"year\"\n\"user\"->\"interface\"\n\"show\"->\"since\"\n\"htc\"->\"one\"\n\"inverted\"->\"inversion\"\n\"much\"->\"going\"\n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

Each node contains the ngram frequency count from the training data.The frequency count for "data" is 198. The frequency count for "entry" is 12 and "streams" is 10. We calculate the MLE as: 

$$
P_{mle}(w2|w1) = \frac{C(w1...w2)}{C(w1)}
$$

The probability of "data entry" is:

$$
P_{mle}(entry|data) = \frac{12}{198} = 0.06 = 6\% 
$$

The probability for "data streams" is:
$$
P_{mle}(streams|data) = \frac{10}{198} = 0.05 = 5\%
$$

Therefore, the language model would predict that "entry" is the most likely next word.


```r
results <- perform_search(ngram_tree, c("data"))
print(results)
```

```
##                   12                   10                  
## recommended_words "entry"              "streams"           
## likelihood        "0.0606060606060606" "0.0505050505050505"
##                   8                    7                   
## recommended_words "recovery"           "dating"            
## likelihood        "0.0404040404040404" "0.0353535353535354"
##                   7                   
## recommended_words "personalize"       
## likelihood        "0.0353535353535354"
```

### Word Prediction for: 'data entry'

Now let's predict the next word after: "data entry". The frequency count for "data entry" is 12. The frequency count for "just" is 6 and "respond" is 6. 

<!--html_preserve--><div id="htmlwidget-9096" style="width:672px;height:480px;" class="grViz html-widget"></div>
<script type="application/json" data-for="htmlwidget-9096">{"x":{"diagram":"digraph {\n\ngraph [rankdir = \"TB\", label = \"Tree Lookup for: data entry\", fontsize = \"40\"]\n\nnode [style = \"filled,rounded\", shape = \"box\", fillcolor = \"GreenYellow\", fontname = \"helvetica\"]\n\nedge [arrowhead = \"vee\", color = \"grey35\", penwidth = \"2\"]\n\n  \"start\" [label = \"start\", tooltip = \"- name: start\"] \n  \"data\" [label = \"data\", tooltip = \"- freq: 198\"] \n  \"entry\" [label = \"entry\", fillcolor = \"LightBlue\", tooltip = \"- freq: 12\n- word: data entry\", penwidth = \"5px\"] \n  \"just\" [label = \"just\", fillcolor = \"LightBlue\", tooltip = \"- freq: 6\n- word: data entry just\", penwidth = \"5px\"] \n  \"overwhelming\" [label = \"overwhelming\", fillcolor = \"LightBlue\", tooltip = \"- freq: 6\n- word: data entry just overwhelming\", penwidth = \"5px\"] \n  \"respond\" [label = \"respond\", fillcolor = \"LightBlue\", tooltip = \"- freq: 6\n- word: data entry respond\", penwidth = \"5px\"] \n  \"emails\" [label = \"emails\", fillcolor = \"LightBlue\", tooltip = \"- freq: 6\n- word: data entry respond emails\", penwidth = \"5px\"] \n  \"streams\" [label = \"streams\", tooltip = \"- freq: 10\n- word: data streams\"] \n  \"live\" [label = \"live\", tooltip = \"- freq: 10\n- word: data streams live\"] \n  \"race\" [label = \"race\", tooltip = \"- freq: 10\n- word: data streams live race\"] \n  \"recovery\" [label = \"recovery\", tooltip = \"- freq: 8\n- word: data recovery\"] \n  \"software\" [label = \"software\", tooltip = \"- freq: 8\n- word: data recovery software\"] \n  \"showed\" [label = \"showed\", tooltip = \"- freq: 8\n- word: data recovery software showed\"] \n  \"dating\" [label = \"dating\", tooltip = \"- freq: 7\n- word: data dating\"] \n  \"average\" [label = \"average\", tooltip = \"- freq: 7\n- word: data dating average\"] \n  \"year\" [label = \"year\", tooltip = \"- freq: 7\n- word: data dating average year\"] \n  \"personalize\" [label = \"personalize\", tooltip = \"- freq: 7\n- word: data personalize\"] \n  \"user\" [label = \"user\", tooltip = \"- freq: 7\n- word: data personalize user\"] \n  \"interface\" [label = \"interface\", tooltip = \"- freq: 7\n- word: data personalize user interface\"] \n  \"records\" [label = \"records\", tooltip = \"- freq: 7\n- word: data records\"] \n  \"show\" [label = \"show\", tooltip = \"- freq: 7\n- word: data records show\"] \n  \"since\" [label = \"since\", tooltip = \"- freq: 7\n- word: data records show since\"] \n  \"allowance\" [label = \"allowance\", tooltip = \"- freq: 6\n- word: data allowance\"] \n  \"htc\" [label = \"htc\", tooltip = \"- freq: 6\n- word: data allowance htc\"] \n  \"one\" [label = \"one\", tooltip = \"- freq: 6\n- word: data allowance htc one\"] \n  \"bus\" [label = \"bus\", tooltip = \"- freq: 5\n- word: data bus\"] \n  \"inverted\" [label = \"inverted\", tooltip = \"- freq: 5\n- word: data bus inverted\"] \n  \"inversion\" [label = \"inversion\", tooltip = \"- freq: 5\n- word: data bus inverted inversion\"] \n  \"soon\" [label = \"soon\", tooltip = \"- freq: 4\n- word: data soon\"] \n  \"much\" [label = \"much\", tooltip = \"- freq: 4\n- word: data soon much\"] \n  \"going\" [label = \"going\", tooltip = \"- freq: 4\n- word: data soon much going\"] \n\"start\"->\"data\"\n\"data\"->\"entry\"\n\"data\"->\"streams\"\n\"data\"->\"recovery\"\n\"data\"->\"dating\"\n\"data\"->\"personalize\"\n\"data\"->\"records\"\n\"data\"->\"allowance\"\n\"data\"->\"bus\"\n\"data\"->\"soon\"\n\"entry\"->\"just\"\n\"entry\"->\"respond\"\n\"streams\"->\"live\"\n\"recovery\"->\"software\"\n\"dating\"->\"average\"\n\"personalize\"->\"user\"\n\"records\"->\"show\"\n\"allowance\"->\"htc\"\n\"bus\"->\"inverted\"\n\"soon\"->\"much\"\n\"just\"->\"overwhelming\"\n\"respond\"->\"emails\"\n\"live\"->\"race\"\n\"software\"->\"showed\"\n\"average\"->\"year\"\n\"user\"->\"interface\"\n\"show\"->\"since\"\n\"htc\"->\"one\"\n\"inverted\"->\"inversion\"\n\"much\"->\"going\"\n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

We calculate the MLE as: 

$$
`P_mle(w3|w1...w2) = \frac{C(w1...w3)}{C(w2)}
$$

The probability of "data entry just" is:

$$
P_{mle}(just|data entry) = \frac{6}{12} = 0.50 = 50\%
$$

The probablility of "data entry respond" is:

$$
P_mle(respond|data entry) = \frac{6}{12} = 0.50 = 50\%
$$

The language model would predict that "just" and "respond"" are equally likely to be the next word.


```r
results <- perform_search(ngram_tree, c("data", "entry"))
print(results)
```

```
##                   6      6        
## recommended_words "just" "respond"
## likelihood        "0.5"  "0.5"
```

### Conclusion

The preliminary results show that we can build a language model using ngrams from training data from `blogs`, `news` and `twitter`. We can build a word predictor using a suffix tree built from ngrams extracted from the training data.

### Next Steps
Here are the list of next steps for the coming weeks.

- Use a weighting technique like `TF/IDF` to adjust the ngram frequencies.
- Apply text preprocessing to search text.
- Implement backoff search to handle instances when a phrase is not found in the tree.
- Build a model using the more than a 1% sample.
- Deploy the ngram tree to the server-side of an Shiny Application.


# Appendix

## Appendix 1: Subsampling Code

This code collects a 1% sample using a "coin flip" to decide which lines to choose.


```r
# sample the datasci dir
sample_capstone_data <- function(fn, outfn, sample_len=0.01) {
  print(sprintf("Reading %s", fn))
  lines <- readLines(fn)
  set.seed(123)
  print(sprintf("Read %s Length %s", fn, length(lines)))
  lines_sample <- lines[rbinom(length(lines)*sample_len, length(lines), 0.5)]
  print(sprintf("Writing %s. Length %s", outfn, length(lines_sample)))
  write.csv(lines_sample, file=outfn, row.names=FALSE, col.names=FALSE)
}

sample_capstone_data("./data/final/en_US/en_US.twitter.txt",
                     "./data/final/en_US/sample/en_US.twitter.txt")
sample_capstone_data("./data/final/en_US/en_US.blogs.txt",
                     "./data/final/en_US/sample/en_US.blogs.txt")
sample_capstone_data("./data/final/en_US/en_US.news.txt",
                     "./data/final/en_US/sample/en_US.news.txt")
```




# Using text analysis techniques to build a predictive text model
Telvis Calhoun  
June 10, 2016  

## Executive Summary

[Swiftkey](url/to/switftkey) develop a word prediction application that is used while typing into a keyboards on a mobile keyboard. When the user types: "I went to the " : the application presents three options for what the next word might be. For example, the three words might be `gym, store, restaurant`.

In this project, we use `R` to build a predictive model using text data provided by the [Data Science Capstone course](path/to/capstone). The data consists of text from 'Blogs', 'News' and 'Twitter' totaling more than 4 million lines and `???` unique words.

### TL;DR

In a nutshell, here's a summary of the data analysis performed in this report.

1. Load the raw data.
2. Extract a 1% subsample of the data.
3. Preprocess the data to remove stopword, convert to lowercase and other items.
4. Generate 2-grams, 3-grams and 4-grams.
5. Present a technique to use the [Dirichlet-multinomial model](path/to/wikipedia) as a language model.





<!-- First, I fetched the data from the URL provided by the course. Here are line counts per file. -->

<!-- ``` -->
<!-- $ wc -l data/final/en_US/*.txt -->
<!--   899288 data/final/en_US/en_US.blogs.txt -->
<!--  1010242 data/final/en_US/en_US.news.txt -->
<!--  2360148 data/final/en_US/en_US.twitter.txt -->
<!--  4269678 total -->
<!-- ``` -->

## Explore the Data
First, we sample 1% of the lines in the files in order to speed up the data exploration. The implementation is in [sample_capstone_data in sample_data.R](./sample_data.R). We use [tm R package](path/to/tm.html) to load each sample file for analysis. 




|   |source  | num_lines| num_unique_words| mean_word_freq| median_word_freq|
|:--|:-------|---------:|----------------:|--------------:|----------------:|
|1  |twitter |     23602|             8040|             20|                9|
|2  |blogs   |      8993|            12414|             15|                6|
|3  |news    |     10103|            12850|             15|                7|

### Text Cleaning
We perform the following text processing steps prior to parsing ngrams.

- Remove all Punctuation
- Remove all Numbers
- Convert all words to Lowercase
- Remove English Stopwords
- Strip extra whitespace
- Remove Profanity

### Word Frequencies

For example, look at the word frequency distribution for the sample data


```r
p <- all_docs_word_plot(sample_vector_corpus)
print(p)
```

![](capstone_milestone_report_1_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


### NGram Frequencies

Let's load all the data sources into 1 corpus.


```r
docs <- load_sample_dircorpus()
docs <- preprocess_entries(docs)
```

Here are top bigrams.

```r
ngram_2 <- get_docterm_matrix(docs, 2)
p2 <- generate_word_frequency_plot(ngram_2$wf, "Top Bigrams for Sampled Text")
print(p2)
```

![](capstone_milestone_report_1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Here are top tri-grams

```r
ngram_3 <- get_docterm_matrix(docs, 3)
p3 <- generate_word_frequency_plot(ngram_3$wf, "Top Trigrams for Sampled Text")
print(p3)
```

![](capstone_milestone_report_1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Here are top 4-grams

```r
ngram_4 <- get_docterm_matrix(docs, 4)
p4 <- generate_word_frequency_plot(ngram_4$wf, "Top 4-grams for Sampled Text")
print(p4)
```

![](capstone_milestone_report_1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

## Word Prediction using an NGrams

We build a tree using the ngrams and compute MLE () using the  [Dirichlet-multinomial model](path/to/wikipedia). We use
[node.tree](path/to/node.tree) which can build a tree from a data.frame. Now lets perform a search for "data".


### Word Prediction for: 'data'
<!--html_preserve--><div id="htmlwidget-1964" style="width:672px;height:480px;" class="grViz html-widget"></div>
<script type="application/json" data-for="htmlwidget-1964">{"x":{"diagram":"digraph {\n\ngraph [rankdir = \"TB\", label = \"Tree Lookup for: data\", fontsize = \"40\"]\n\nnode [style = \"filled,rounded\", shape = \"box\", fillcolor = \"GreenYellow\", fontname = \"helvetica\"]\n\nedge [arrowhead = \"vee\", color = \"grey35\", penwidth = \"2\"]\n\n  \"start\" [label = \"start\", tooltip = \"- name: start\"] \n  \"data\" [label = \"data\", tooltip = \"- freq: 198\"] \n  \"entry\" [label = \"entry\", tooltip = \"- freq: 12\n- word: data entry\"] \n  \"just\" [label = \"just\", tooltip = \"- freq: 6\n- word: data entry just\"] \n  \"overwhelming\" [label = \"overwhelming\", tooltip = \"- freq: 6\n- word: data entry just overwhelming\"] \n  \"respond\" [label = \"respond\", tooltip = \"- freq: 6\n- word: data entry respond\"] \n  \"emails\" [label = \"emails\", tooltip = \"- freq: 6\n- word: data entry respond emails\"] \n  \"streams\" [label = \"streams\", tooltip = \"- freq: 10\n- word: data streams\"] \n  \"live\" [label = \"live\", tooltip = \"- freq: 10\n- word: data streams live\"] \n  \"race\" [label = \"race\", tooltip = \"- freq: 10\n- word: data streams live race\"] \n  \"recovery\" [label = \"recovery\", tooltip = \"- freq: 8\n- word: data recovery\"] \n  \"software\" [label = \"software\", tooltip = \"- freq: 8\n- word: data recovery software\"] \n  \"showed\" [label = \"showed\", tooltip = \"- freq: 8\n- word: data recovery software showed\"] \n  \"dating\" [label = \"dating\", tooltip = \"- freq: 7\n- word: data dating\"] \n  \"average\" [label = \"average\", tooltip = \"- freq: 7\n- word: data dating average\"] \n  \"year\" [label = \"year\", tooltip = \"- freq: 7\n- word: data dating average year\"] \n  \"personalize\" [label = \"personalize\", tooltip = \"- freq: 7\n- word: data personalize\"] \n  \"user\" [label = \"user\", tooltip = \"- freq: 7\n- word: data personalize user\"] \n  \"interface\" [label = \"interface\", tooltip = \"- freq: 7\n- word: data personalize user interface\"] \n  \"records\" [label = \"records\", tooltip = \"- freq: 7\n- word: data records\"] \n  \"show\" [label = \"show\", tooltip = \"- freq: 7\n- word: data records show\"] \n  \"since\" [label = \"since\", tooltip = \"- freq: 7\n- word: data records show since\"] \n  \"allowance\" [label = \"allowance\", tooltip = \"- freq: 6\n- word: data allowance\"] \n  \"htc\" [label = \"htc\", tooltip = \"- freq: 6\n- word: data allowance htc\"] \n  \"one\" [label = \"one\", tooltip = \"- freq: 6\n- word: data allowance htc one\"] \n  \"bus\" [label = \"bus\", tooltip = \"- freq: 5\n- word: data bus\"] \n  \"inverted\" [label = \"inverted\", tooltip = \"- freq: 5\n- word: data bus inverted\"] \n  \"inversion\" [label = \"inversion\", tooltip = \"- freq: 5\n- word: data bus inverted inversion\"] \n  \"soon\" [label = \"soon\", tooltip = \"- freq: 4\n- word: data soon\"] \n  \"much\" [label = \"much\", tooltip = \"- freq: 4\n- word: data soon much\"] \n  \"going\" [label = \"going\", tooltip = \"- freq: 4\n- word: data soon much going\"] \n\"start\"->\"data\"\n\"data\"->\"entry\"\n\"data\"->\"streams\"\n\"data\"->\"recovery\"\n\"data\"->\"dating\"\n\"data\"->\"personalize\"\n\"data\"->\"records\"\n\"data\"->\"allowance\"\n\"data\"->\"bus\"\n\"data\"->\"soon\"\n\"entry\"->\"just\"\n\"entry\"->\"respond\"\n\"streams\"->\"live\"\n\"recovery\"->\"software\"\n\"dating\"->\"average\"\n\"personalize\"->\"user\"\n\"records\"->\"show\"\n\"allowance\"->\"htc\"\n\"bus\"->\"inverted\"\n\"soon\"->\"much\"\n\"just\"->\"overwhelming\"\n\"respond\"->\"emails\"\n\"live\"->\"race\"\n\"software\"->\"showed\"\n\"average\"->\"year\"\n\"user\"->\"interface\"\n\"show\"->\"since\"\n\"htc\"->\"one\"\n\"inverted\"->\"inversion\"\n\"much\"->\"going\"\n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

Here are the maximum likelihood estimates. They show 6% likelihood that entry will be the next word: "data entry" has a frequency = 12 and "data" has a frequency of 198 - so the maximimum likelihood estimate is 6.1%.


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

Then if we query for "data entry", we search the tree the nodes "data" then "entry" and we will recommend the words "just" and "respond".

<!--html_preserve--><div id="htmlwidget-5716" style="width:672px;height:480px;" class="grViz html-widget"></div>
<script type="application/json" data-for="htmlwidget-5716">{"x":{"diagram":"digraph {\n\ngraph [rankdir = \"TB\", label = \"Tree Lookup for: data entry\", fontsize = \"40\"]\n\nnode [style = \"filled,rounded\", shape = \"box\", fillcolor = \"GreenYellow\", fontname = \"helvetica\"]\n\nedge [arrowhead = \"vee\", color = \"grey35\", penwidth = \"2\"]\n\n  \"start\" [label = \"start\", tooltip = \"- name: start\"] \n  \"data\" [label = \"data\", tooltip = \"- freq: 198\"] \n  \"entry\" [label = \"entry\", fillcolor = \"LightBlue\", tooltip = \"- freq: 12\n- word: data entry\", penwidth = \"5px\"] \n  \"just\" [label = \"just\", fillcolor = \"LightBlue\", tooltip = \"- freq: 6\n- word: data entry just\", penwidth = \"5px\"] \n  \"overwhelming\" [label = \"overwhelming\", fillcolor = \"LightBlue\", tooltip = \"- freq: 6\n- word: data entry just overwhelming\", penwidth = \"5px\"] \n  \"respond\" [label = \"respond\", fillcolor = \"LightBlue\", tooltip = \"- freq: 6\n- word: data entry respond\", penwidth = \"5px\"] \n  \"emails\" [label = \"emails\", fillcolor = \"LightBlue\", tooltip = \"- freq: 6\n- word: data entry respond emails\", penwidth = \"5px\"] \n  \"streams\" [label = \"streams\", tooltip = \"- freq: 10\n- word: data streams\"] \n  \"live\" [label = \"live\", tooltip = \"- freq: 10\n- word: data streams live\"] \n  \"race\" [label = \"race\", tooltip = \"- freq: 10\n- word: data streams live race\"] \n  \"recovery\" [label = \"recovery\", tooltip = \"- freq: 8\n- word: data recovery\"] \n  \"software\" [label = \"software\", tooltip = \"- freq: 8\n- word: data recovery software\"] \n  \"showed\" [label = \"showed\", tooltip = \"- freq: 8\n- word: data recovery software showed\"] \n  \"dating\" [label = \"dating\", tooltip = \"- freq: 7\n- word: data dating\"] \n  \"average\" [label = \"average\", tooltip = \"- freq: 7\n- word: data dating average\"] \n  \"year\" [label = \"year\", tooltip = \"- freq: 7\n- word: data dating average year\"] \n  \"personalize\" [label = \"personalize\", tooltip = \"- freq: 7\n- word: data personalize\"] \n  \"user\" [label = \"user\", tooltip = \"- freq: 7\n- word: data personalize user\"] \n  \"interface\" [label = \"interface\", tooltip = \"- freq: 7\n- word: data personalize user interface\"] \n  \"records\" [label = \"records\", tooltip = \"- freq: 7\n- word: data records\"] \n  \"show\" [label = \"show\", tooltip = \"- freq: 7\n- word: data records show\"] \n  \"since\" [label = \"since\", tooltip = \"- freq: 7\n- word: data records show since\"] \n  \"allowance\" [label = \"allowance\", tooltip = \"- freq: 6\n- word: data allowance\"] \n  \"htc\" [label = \"htc\", tooltip = \"- freq: 6\n- word: data allowance htc\"] \n  \"one\" [label = \"one\", tooltip = \"- freq: 6\n- word: data allowance htc one\"] \n  \"bus\" [label = \"bus\", tooltip = \"- freq: 5\n- word: data bus\"] \n  \"inverted\" [label = \"inverted\", tooltip = \"- freq: 5\n- word: data bus inverted\"] \n  \"inversion\" [label = \"inversion\", tooltip = \"- freq: 5\n- word: data bus inverted inversion\"] \n  \"soon\" [label = \"soon\", tooltip = \"- freq: 4\n- word: data soon\"] \n  \"much\" [label = \"much\", tooltip = \"- freq: 4\n- word: data soon much\"] \n  \"going\" [label = \"going\", tooltip = \"- freq: 4\n- word: data soon much going\"] \n\"start\"->\"data\"\n\"data\"->\"entry\"\n\"data\"->\"streams\"\n\"data\"->\"recovery\"\n\"data\"->\"dating\"\n\"data\"->\"personalize\"\n\"data\"->\"records\"\n\"data\"->\"allowance\"\n\"data\"->\"bus\"\n\"data\"->\"soon\"\n\"entry\"->\"just\"\n\"entry\"->\"respond\"\n\"streams\"->\"live\"\n\"recovery\"->\"software\"\n\"dating\"->\"average\"\n\"personalize\"->\"user\"\n\"records\"->\"show\"\n\"allowance\"->\"htc\"\n\"bus\"->\"inverted\"\n\"soon\"->\"much\"\n\"just\"->\"overwhelming\"\n\"respond\"->\"emails\"\n\"live\"->\"race\"\n\"software\"->\"showed\"\n\"average\"->\"year\"\n\"user\"->\"interface\"\n\"show\"->\"since\"\n\"htc\"->\"one\"\n\"inverted\"->\"inversion\"\n\"much\"->\"going\"\n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


```r
results <- perform_search(ngram_tree, c("data", "entry"))
print(results)
```

```
##                   6      6        
## recommended_words "just" "respond"
## likelihood        "0.5"  "0.5"
```

### Next Steps

- Build a model using the more than a 1% sample.
- Deploy the ngram tree to the server-side of an Shiny Application.






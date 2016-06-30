library(tm)
library(SnowballC)
library(ggplot2)  
library(wordcloud) 
library(data.table)
library(timeit)
library(dplyr)
library(quanteda)
source("sample_data.R")

# setwd("/Users/telvis/work/datasciencecoursera/10_capstone")

fetch_capstone_data <- function() {
  data_dir = "./data"
  zipfile <- file.path("data", "Coursera-SwiftKey.zip")
  file_url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  unzipped_data = "data/final"
  
  # check for data zip
  if (!file.exists(data_dir)){
    print(sprintf("Creatiing dir: %s", data_dir))
    dir.create(data_dir)
  }
  
  # check for zip and download the data
  if (!file.exists(zipfile)) {
    print(sprintf("Downloading data: %s", zipfile))
    download.file(file_url, destfile=zipfile, method = "curl")
  }
  
  # unzip the data
  if(!file.exists(unzipped_data)){
    print(sprintf("Unzipping data: %s", zipfile))
    unzip(zipfile, exdir = data_dir)
  }
  
  list.files(data_dir)
}

add_meta_data_to_docs <- function(tm_corpus, tag, value) {
  i <- 1
  tm_corpus <- tm_map(tm_corpus, function(x) {
    meta(x, tag=tag) <- value
    x
  })
  tm_corpus
}

load_all_data <- function(nlines=0, procsess=FALSE) {
  tmp <- system.time({
    tweets <- newline_text_file_to_corpus(filename="./data/final/en_US/en_US.twitter.txt",
                                          nlines=nlines)
    tweets <- preprocess_entries(tweets)
    tweets <- add_meta_data_to_docs(tweets, "doc_type", "twitter")
    blogs <- newline_text_file_to_corpus(filename="./data/final/en_US/en_US.blogs.txt",
                                         nlines=nlines)
    blogs <- preprocess_entries(blogs)
    blogs <- add_meta_data_to_docs(blogs, "doc_type", "blog")
    news <- newline_text_file_to_corpus(filename="./data/final/en_US/en_US.news.txt",
                                        nlines=nlines)
    news <- add_meta_data_to_docs(news, "doc_type", "news")
    news <- preprocess_entries(blogs)
    docs <- c(tweets, blogs, news)
  })
  print(tmp)
  docs
}

remove_profanity <- function(docs) {
  profanity <- read.csv("data/profanity.txt", header=FALSE, stringsAsFactors=FALSE)
  profanity <- profanity$V1
  docs <- tm_map(docs, removeWords, profanity)
  docs
}

preprocess_single_string <- function(s) {
  s <- removePunctuation(s)
  s <- removeNumbers(s)
  s <- tolower(s)
  # s <- removeWords(s, stopwords("english"))
  # s <- removeWords(s, c('the', 'to', 'and', 'a', 'of'))
  # s <- removeWords(s, c('the', 'a'))
  s <- stripWhitespace(s)
  # TODO: load the profanity DB
  s
}

preprocess_entries <- function(docs) {
  
  options(mc.cores=1)
  
  docs <- tm_map(docs, removePunctuation)  # *Removing punctuation:*  
  docs <- tm_map(docs, removeNumbers)     # *Removing numbers:* 
  docs <- tm_map(docs, content_transformer(tolower))   # *Converting to lowercase:* 
  # docs <- tm_map(docs, removeWords, stopwords("english"))   # *Removing "stopwords" 
  # docs <- tm_map(docs, removeWords, c('the', 'to', 'and', 'a', 'of'))
  # docs <- tm_map(docs, removeWords, c('the', 'a'))
  docs <- tm_map(docs, stripWhitespace)  # *Stripping whitespace
  docs <- remove_profanity(docs)
  docs <- tm_map(docs, content_transformer(iconv), to="latin1", from="ASCII", sub="_TODO_")
  docs <- tm_map(docs, content_transformer(gsub), 
                 pattern="_TODO_", 
                 replacement="",
                 perl=T)
  docs
}

do_system.time <- function(what, args){
  tmp <- system.time({
      ret = do.call(what, args)
    })
  print(tmp)
  ret
}

get_docterm_matrix <- function(docs, 
                               ngram_length=1, 
                               parent_words=NULL,
                               prune_cover_percentage=0.60) {
  
  print(sprintf("get_docterm_matrix: %s-gram", ngram_length))
  
  # https://cran.r-project.org/web/packages/quanteda/vignettes/quickstart.html#document-feature-matrix-analysis-tools
  dtm <- dfm(docs, what="fasterword", ngrams=ngram_length, concatenator = " ")
  
  print("Generating term frequencies")
  freq <- colSums(dtm)
  print("Deleting doc term matrix")
  rm(dtm)
  
  freq <- sort(freq, decreasing=TRUE)
  wf <- data.table(word=names(freq), freq=freq, keep.rownames=F)
  print("deleting frequency list")
  rm(freq)
  
  # verify the class of 'word' is character instead of 'factor'
  # also remove the 'row.names' because it increases memory usage.
  # wf <- mutate(wf, word=as.character(word))
  wf[,word:=as.character(word)]
  setkey(wf, word)
  setorder(wf, -freq)
  count_before <- nrow(wf)
  
  if (ngram_length > 1) {
    # generate the root word
    print("generating root")
    # wf$root <- sapply(wf$word, 
    #                   function(x) {
    #                     w <- unlist(strsplit(x, " "))[1:ngram_length-1]; 
    #                     paste(w, collapse = " ")
    #                   })
    wf[,root:= {
      sapply(wf$word, 
             function(x) {
               w <- unlist(strsplit(x, " "))[1:ngram_length-1]; 
               paste(w, collapse = " ")
             })
    }]
    
    # filter by words in parent
    if (! is.null(parent_words)){
      print("filtering for words not in parent db")
      # wf <- filter(wf, root %in% parent_words)
      wf <- wf[root %in% parent_words,]
      count_after <- nrow(wf)
      print(sprintf("parent db removed %s rows %s-grams. %s remain", count_before - count_after, 
                    ngram_length, count_after))
    }
  }
  
  print("prune by cover percentage")
  wf <- prune_ngram_df_by_cover_percentage(wf, prune_cover_percentage)
  
  count_after <- nrow(wf)
  print(sprintf("Removed %s rows %-grams. Remaining: %s", count_before - count_after, ngram_length, count_after))
  
  # return term/doc matrix and word frequency data.frame in a list
  docterm_datums = list()
  
  # sorted word frequency data.frame
  docterm_datums$wf <- wf
  
  docterm_datums
}

prune_ngram_df_by_cover_percentage <- function(df, percentage) {
  # prune_ngram_df_by_cover_percentage(datums$df_ngram_4, "data/pruned_50p_term_doc_matrix_4_ngram_df.rds", .50)
  sums <- cumsum(df$freq)
  cover <- which(sums > sum(df$freq) * percentage)[1]
  print(sprintf("%s of %s (%s%%) cover %s%% of word instances", 
                cover, 
                nrow(df), 
                cover/nrow(df)*100,
                percentage*100))
  
  df[1:cover,]
}

hacking_with_quantenda <- function() {
  # steps from : https://cran.r-project.org/web/packages/quanteda/vignettes/quickstart.html
  
  # doc_dir <- "./data/final/en_US/sample.1.percent/"
  # docs <- load_sample_dircorpus(sampledir=doc_dir)
  # docs <- preprocess_entries(docs)
  # docs <- corpus(docs)
  
  tweets <- newline_text_file_to_corpus(filename="./data/final/en_US/sample.1.percent/en_US.twitter.txt")
  blogs <- newline_text_file_to_corpus(filename="./data/final/en_US/sample.1.percent/en_US.blogs.txt")
  news <- newline_text_file_to_corpus(filename="./data/final/en_US/sample.1.percent/en_US.news.txt")
  docs <- c(tweets, blogs, news)
  docs <- preprocess_entries(docs)
  docs <- corpus(docs)

  # From: http://stackoverflow.com/questions/31570437/really-fast-word-ngram-vectorization-in-r
  # toks <- tokenize(docs, what="fasterword")
  # toks2 <- ngrams(toks, n = 2, concatenator = " ")
  # ngram_2 <- dfm(toks2, verbose = FALSE)

  #
  summary(docs)
  dtm <- dfm(docs, what="fasterword", ngrams=2, concatenator = " ")
  freq <- colSums(dtm)
  freq <- sort(freq, decreasing=TRUE)
  wf <- data.frame(word=names(freq), freq=freq)
  
  dtm[,1:5]
  topfeatures(dtm, 20)
  plot(dtm, max.words = 20, colors = brewer.pal(6, "Dark2"), scale = c(8, .5))
  
  # https://github.com/kbenoit/quanteda/blob/master/R/ngrams.R
  # https://github.com/kbenoit/quanteda/issues/149
  # https://github.com/lmullen/tokenizers
}


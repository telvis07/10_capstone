library(tm)
library(SnowballC)
library(ggplot2)  
library(wordcloud) 
library(cluster)   
# library(fpc)
library(RWeka)
library(timeit)
library(ngram)
library(tau)

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

newline_text_file_to_corpus <- function(filename,
                                        nlines=10) {
  lines <- scan(file=filename, what="", sep="\n", nlines = nlines)
  t_corpus <- Corpus(VectorSource(lines))
  t_corpus
}

load_all_data <- function(nlines=10) {
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
  docs
}

remove_profanity <- function(docs) {
  profanity <- read.csv("data/profanity.txt", header=FALSE, stringsAsFactors=FALSE)
  profanity <- profanity$V1
  docs <- tm_map(docs, removeWords, profanity)
  docs
}

preprocess_entries <- function(docs, save_file="data/preprocessed_corpus.rds") {
  
  options(mc.cores=4)
  
  print("remove punctuation")
  print(system.time({docs <- tm_map(docs, removePunctuation)}))   # *Removing punctuation:*  
  
  print("removeNumbers")
  print(system.time({docs <- tm_map(docs, removeNumbers)}))      # *Removing numbers:* 
  
  print("tolower")
  print(system.time({docs <- tm_map(docs, content_transformer(tolower))}))   # *Converting to lowercase:* 
  
  print("removeWords")
  print(system.time({docs <- tm_map(docs, removeWords, stopwords("english"))}))   # *Removing "stopwords" 
  
  print("stripWhitespace")
  print(system.time({docs <- tm_map(docs, stripWhitespace)}))   # *Stripping whitespace
  
  print("remove profanity")
  print(system.time({docs <- remove_profanity(docs)}))
  
  # print("PlainTextDocument")
  # print(system.time({docs <- tm_map(docs, PlainTextDocument)}))
  
  print(sprintf("Saving processed docs to %s", save_file))
  saveRDS(docs, save_file)
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
                               save_file="data/term_doc_matrix_%s_ngram.rds",
                               save_file_df="data/term_doc_matrix_%s_ngram_df.rds") {
  
  save_file <- sprintf(save_file, ngram_length)
  save_file_df <- sprintf(save_file_df, ngram_length)
  # print("convert to data frame")
  # tmp <- system.time ({
  #   docs_df <- data.frame(text=unlist(sapply(docs, '[',"content")),stringsAsFactors=F)
  # })
  # print(tmp)
  options(mc.cores=1)
  
  tokenizer <- function(x) {
    # print(x)
    # ngram::ngram_asweka(content(x), min=ngram_length, max=ngram_length)
    # ngram(content(x), n=ngram_length)
    # rownames(as.data.frame(unclass(textcnt(content(x),method="string",n=ngram_length))))
    NGramTokenizer(x, Weka_control(min = ngram_length, max = ngram_length)) # create n-grams
  }
  if (ngram_length > 1) {
    print("Generating doc/term matrix")
    tmp <- system.time({
      dtm <- DocumentTermMatrix(docs, control = list( tokenize=tokenizer))
    })
    print(tmp)
  } else {
    print("Generating doc/term matrix")
    tmp <- system.time({dtm <- DocumentTermMatrix(docs)})
    print(tmp)
  }

  print(sprintf("Saving docterm matrix to %s", save_file))
  saveRDS(dtm, save_file)

  print("Most frequent words")
  freq <- colSums(as.matrix(dtm))
  freq <- sort(freq, decreasing=TRUE)
  wf <- data.frame(word=names(freq), freq=freq)
  
  print(sprintf("Saving docterm data frame to %s", save_file_df))
  saveRDS(wf, save_file_df)
  
  print(head(wf))
  
  dtm
  
  # sparse_filter=0.05
  # dtms <- removeSparseTerms(dtm, 0.01)
  # list(full=dtm, sparse=dtms)
}

prune_ngram_df_by_cover_percentage <- function(df, save_file, percentage) {
  # prune_ngram_df_by_cover_percentage(datums$df_ngram_4, "data/pruned_50p_term_doc_matrix_4_ngram_df.rds", .50)
  sums <- cumsum(df$freq)
  cover <- which(sums > sum(df$freq) * percentage)[1]
  print(sprintf("%s of %s (%s%%) cover %s%% of word instances", 
                cover, 
                nrow(df), 
                cover/nrow(df)*100,
                percentage*100))
  
  print(sprintf("Saving pruned docterm data frame to %s", save_file))
  saveRDS(df[1:cover,], save_file)
}


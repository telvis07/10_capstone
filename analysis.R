library(tm)
library(SnowballC)
library(ggplot2)  
library(wordcloud) 
library(cluster)   
library(fpc)
library(RWeka)

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

get_docterm_matrix <- function(docs, 
                               ngram_length=1,
                               save_file="data/term_doc_matrix_%s_ngram.rds") {
  
  save_file <- sprintf(save_file, ngram_length)
  printf(sprintf("Save file is %s", save_file))
  tokenizer <- function(x) {
    NGramTokenizer(x, Weka_control(min = ngram_length, max = ngram_length)) # create n-grams
  }
  
  if (ngram_length > 1) {
    system.time({dtm <- DocumentTermMatrix(docs, control = list(
      tokenize=tokenizer
    ))})
  } else {
    print("Generating term/doc matrix")
    system.time({dtm <- DocumentTermMatrix(docs)})
  }

  print(sprintf("Saving docterm matrix to %s", save_file))
  saveRDS(dtm, save_file)
  
  print("Most frequent words")
  freq <- colSums(as.matrix(dtm))
  freq <- sort(freq, decreasing=TRUE)
  wf <- data.frame(word=names(freq), freq=freq)
  print(head(wf))
  
  dtm
  
  # sparse_filter=0.05
  # dtms <- removeSparseTerms(dtm, 0.01)
  # list(full=dtm, sparse=dtms)
}

plot_word_frequencies <- function(dtm) {
  print("using colsums/head")
  freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)   
  head(freq, 14) 
  wf <- data.frame(word=names(freq), freq=freq)   
  print(head(wf)) 
  
  p <- ggplot(subset(wf, freq>15), aes(word, freq))    
  p <- p + geom_bar(stat="identity")   
  p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
  p
}

plot_wordcloud <- function(dtm) {
  
  freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
  set.seed(142)   
  wordcloud(names(freq), freq, min.freq=10, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))  
}

plot_wordcloud_top_n <- function(dtm, max.words=15) {
  freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
  set.seed(142)   
  dark2 <- brewer.pal(6, "Dark2")   
  wordcloud(names(freq), freq, max.words=max.words, rot.per=0.2, colors=dark2)  
}

hierarchical_cluster <- function(dtm) {
  d <- dist(t(dtm), method="euclidian")   
  fit <- hclust(d=d, method="ward")   
  fit
  
  plot(fit, hang=-1)   
}

kmeans_plot <- function(dtm) {
  d <- dist(t(dtm), method="euclidian")   
  kfit <- kmeans(d, 2)   
  clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   
}
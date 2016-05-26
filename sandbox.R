library(tm)
library(SnowballC)
library(ggplot2)  
library(wordcloud) 
library(cluster)   
library(fpc) 


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

preprocess_entries <- function(docs) {
  docs <- tm_map(docs, removePunctuation)   # *Removing punctuation:*    
  docs <- tm_map(docs, removeNumbers)      # *Removing numbers:*    
  docs <- tm_map(docs, tolower)   # *Converting to lowercase:*    
  docs <- tm_map(docs, removeWords, stopwords("english"))   # *Removing "stopwords" 
  docs <- tm_map(docs, stemDocument)   # *Removing common word endings* (e.g., "ing", "es")   
  docs <- tm_map(docs, stripWhitespace)   # *Stripping whitespace   
  docs <- tm_map(docs, PlainTextDocument)  
  docs
}

explore_data <- function(docs) {
  dtm <- DocumentTermMatrix(docs)
  print("Using findFreqTerms")
  print(findFreqTerms(dtm, lowfreq=20))
  freq <- colSums(as.matrix(dtm))
  ord <- order(freq) 
  print("most frequent")
  print(freq[tail(ord, n=10)])
  
  # dtms <- removeSparseTerms(dtm, 0.01)
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

# test code 
test_read_tweets <- function() {
  lines <- scan(file="./data/final/en_US/en_US.twitter.txt", what="", sep="\n", nlines = 10)
  # only the first 10 lines
  t_corpus <- Corpus(VectorSource(lines))
  t_corpus[[1]]$content
  
  # stemming
  t_corpus <- tm_map(t_corpus, stemDocument)
  t_corpus[[1]]$content
  # "How are you? Btw thank for the RT. You gonna be in DC anytim soon? Love to see you. Been way, way too long."

  # lower case 
  t_corpus <- tm_map(t_corpus, tolower)
  t_corpus[[1]]
  
  # grep/search
  tm_filter(t_corpus, 
            FUN = function(x) any(grep("RT", content(x))))[[1]]$content
  
  # document term matrix
  dtm <- DocumentTermMatrix(t_corpus)
  inspect(dtm)
  
  # dictionary

}

test_reuters <- function() {
  # dictionary
  reut21578 <- system.file("texts", "crude", package = "tm")
  reuters <- VCorpus(DirSource(reut21578),
                     readerControl = list(reader = readReut21578XMLasPlain))
  inspect(DocumentTermMatrix(reuters,
                             list(dictionary = c("prices", "crude", "oil"))))
}



# quiz #1

find_longest_line <- function(){
  max(sapply(readLines("./data/final/en_US/en_US.twitter.txt"), nchar))
  max(sapply(readLines("./data/final/en_US/en_US.blogs.txt"), nchar))
  max(sapply(readLines("./data/final/en_US/en_US.news.txt"), nchar))
}

twitter_word_count_love_hate <- function() {
  # lines <- scan(file="./data/final/en_US/en_US.twitter.txt", what="", sep="\n")
  lines <- readLines("./data/final/en_US/en_US.twitter.txt")
  t_corpus <- Corpus(VectorSource(lines))
  t_corpus <- tm_map(t_corpus, tolower)
  t_corpus <- tm_map(t_corpus, PlainTextDocument)
  has_love <- tm_filter(t_corpus, 
                        FUN = function(x) any(grep("love", content(x))))
  has_hate <- tm_filter(t_corpus, 
                        FUN = function(x) any(grep("hate", content(x))))
  length(has_love) / length(has_hate)
  
  # has biostats
  has_love <- tm_filter(t_corpus, 
                        FUN = function(x) any(grep("biostats", content(x))))[[1]]$content
  
}

hacking_with_weka <- function() {
  NGramTokenizer("a b a c a b b", Weka_control(min = 2, max = 2))
  NGramTokenizer(PlainTextDocument("a b a c a b b"), Weka_control(min = 2, max = 2))
  PlainTextDocument("a b a c a b b")
  content(PlainTextDocument("a b a c a b b"))
}

# twitter_word_count_phrase <- function() {
#   
# }



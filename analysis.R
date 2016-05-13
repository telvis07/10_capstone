library(tm)
library(SnowballC)

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

load_all_data <- function() {
  tweets <- newline_text_file_to_corpus(filename="./data/final/en_US/en_US.twitter.txt")
  tweets <- add_meta_data_to_docs(tweets, "doc_type", "twitter")
  blogs <- newline_text_file_to_corpus(filename="./data/final/en_US/en_US.blogs.txt")
  blogs <- add_meta_data_to_docs(blogs, "doc_type", "blog")
  news <- newline_text_file_to_corpus(filename="./data/final/en_US/en_US.news.txt")
  news <- add_meta_data_to_docs(news, "doc_type", "news")
  
  corpus <- c(tweets, blogs, news)
  corpus
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
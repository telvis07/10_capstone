library(tm)

# sample the datasci dir
sample_capstone_data <- function(fn, outfn, sample_len=0.25) {
  print(sprintf("Reading %s", fn))
  lines <- readLines(fn)
  set.seed(123)
  # sample.int
  # 
  print(sprintf("Read %s Length %s", fn, length(lines)))
  n = length(lines)
  size = n*sample_len
  lines_sample <- lines[sample(n, size, replace=FALSE)]
  print(sprintf("Writing %s. Length %s", outfn, length(lines_sample)))
  write.csv(lines_sample, file=outfn, row.names=FALSE)
}


generate_sample_files <- function() {
  sample_dir <- "./data/final/en_US/sample"
  
  # check for data zip
  if (!file.exists(sample_dir)){
    print(sprintf("Creatiing dir: %s", sample_dir))
    dir.create(sample_dir)
  }
  
  sample_capstone_data("./data/final/en_US/en_US.twitter.txt",
                       "./data/final/en_US/sample/en_US.twitter.txt")
  sample_capstone_data("./data/final/en_US/en_US.blogs.txt",
                       "./data/final/en_US/sample/en_US.blogs.txt")
  sample_capstone_data("./data/final/en_US/en_US.news.txt",
                       "./data/final/en_US/sample/en_US.news.txt")
}

load_sample_dircorpus <- function(sampledir="./data/final/en_US/sample/", 
                                  save_file=NULL) {
  docs <- Corpus(DirSource(sampledir),
                 readerControl = list(
                   language="en_US"
                 ))
  # save datums
  if (! is.null(save_file)) {
    print(sprintf("Saving corpus to %s", save_file))
    saveRDS(docs, save_file)
  }
  docs
}

newline_text_file_to_corpus <- function(filename) {
  lines <- readLines(filename)
  t_corpus <- Corpus(VectorSource(lines))
  t_corpus
}

load_sample_vec_corpus <- function(save_file="data/sample_vec_corpus.rds") {
  tmp <- system.time({
    print("reading twitter")
    tweets <- newline_text_file_to_corpus(filename="./data/final/en_US/sample/en_US.twitter.txt")
    print("reading blogs")
    blogs <- newline_text_file_to_corpus(filename="./data/final/en_US/sample/en_US.blogs.txt")
    print("reading news")
    news <- newline_text_file_to_corpus(filename="./data/final/en_US/sample/en_US.news.txt")
    print("Joining data")
    docs <- c(tweets, blogs, news)
    print(sprintf("Saving vector corpus to %s", save_file))
    saveRDS(docs, save_file)
  })
  print(tmp)
  docs
}

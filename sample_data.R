library(tm)

# sample the datasci dir
sample_capstone_data <- function(fn, outfn, sample_len, seed) {
  print(sprintf("Reading %s", fn))
  lines <- readLines(fn)
  set.seed(seed)
  # sample.int
  # 
  print(sprintf("Read %s Length %s", fn, length(lines)))
  n = length(lines)
  size = n*sample_len
  lines_sample <- lines[sample(n, size, replace=FALSE)]
  print(sprintf("Writing %s. Length %s", outfn, length(lines_sample)))
  write.csv(lines_sample, file=outfn, row.names=FALSE)
}


generate_sample_files <- function(sample_dir = "./data/final/en_US/test_sample",
                                  sample_len=0.01,
                                  seed=123) {
  
  # to generate test data
  # generate_sample_files(sample_dir = "./data/final/en_US/test_sample, seed=4567)
  
  
  # check for data zip
  if (!file.exists(sample_dir)){
    print(sprintf("Creatiing dir: %s", sample_dir))
    dir.create(sample_dir)
  }
  
  sample_capstone_data("./data/final/en_US/all/en_US.twitter.txt",
                       file.path(sample_dir, "en_US.twitter.txt"),
                       sample_len = sample_len,
                       seed = seed)
  sample_capstone_data("./data/final/en_US/all/en_US.blogs.txt",
                       file.path(sample_dir, "en_US.blogs.txt"),
                       sample_len = sample_len,
                       seed=seed)
  sample_capstone_data("./data/final/en_US/all/en_US.news.txt",
                       file.path(sample_dir, "en_US.news.txt"),
                       sample_len = sample_len,
                       seed=seed)
}

load_sample_dircorpus <- function(sample_dir="./data/final/en_US/sample.1.percent/", 
                                  save_file=NULL) {
  docs <- Corpus(DirSource(sample_dir),
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

load_sample_vec_corpus <- function(sample_dir="./data/final/en_US/sample.1.percent/") {

  print("reading twitter")
  tweets <- newline_text_file_to_corpus(filename=file.path(sample_dir, "en_US.twitter.txt"))
  print("reading blogs")
  blogs <- newline_text_file_to_corpus(filename=file.path(sample_dir, "en_US.blogs.txt"))
  print("reading news")
  news <- newline_text_file_to_corpus(filename=file.path(sample_dir, "en_US.news.txt"))
  print("Joining data")
  docs <- c(tweets, blogs, news)

  docs
}

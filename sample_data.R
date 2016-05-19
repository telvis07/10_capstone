library(tm)

# sample the datasci dir
sample_capstone_data <- function(fn, outfn, sample_len=0.01) {
  print(sprintf("Reading %s", fn))
  lines <- readLines(fn)
  set.seed(123)
  print(sprintf("Read %s Length %s", fn, length(lines)))
  lines_sample <- lines[rbinom(length(lines)*sample_len, length(lines), 0.5)]
  print(sprintf("Writing %s. Length %s", outfn, length(lines_sample)))
  write.csv(lines, file=outfn, row.names=FALSE, col.names=FALSE)
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
                                  save_file="data/sample_corpus.rds") {
  docs <- Corpus(DirSource(sampledir),
                 readerControl = list(
                   language="en_US"
                 ))
  print(sprintf("Saving corpus to %s", save_file))
  saveRDS(docs, save_file)
  docs
}
library(data.tree)
library(dplyr)

tree_single_word <- function() {
  last_root_word_df <- filter(datums$df_ngram_all, grepl("^last ", word) & freq > 10)
  last_root_word_df$pathString <- sapply(last_root_word_df$word, gen_path_string)
  ngram_tree <- as.Node(last_root_word_df)
}

gen_path_string <- function (x) {
  paste("root", gsub(" ", "/", x), sep="/")
}

fun_with_trees <- function() {
  # level 1 is 'root'
  # level 2 is word #1 in the ngram
  # level 3 is word #2 in the ngram
  # level 4 is word #3 in the ngram
  # level 5 is word #4 in the ngram
  w = ngram_tree$Get(function(x) {list(word=x$word, freq=x$freq)}, filterFun = function(x){x$level==3})
  # w[2,1]$word
  # w[2,1]$freq

  length(ngram_tree$Climb(name="last")$children)
  w = ngram_tree$Climb(name="last")$Get(function(x) {c(x$word, x$freq)}, filterFun = function(x){x$level==3})
  
  # > w[1,1]
  # $word
  # [1] "last year"
  # 
  # > w[2,1]
  # $freq
  # [1] 18583
}







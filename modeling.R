library(data.tree)
library(dplyr)

tree_single_word <- function() {
  last_root_word_df <- filter(datums$df_ngram_all, grepl("^last ", word) & freq > 10)
  last_root_word_df$pathString <- sapply(last_root_word_df$word, gen_path_string)
  ngram_tree <- as.Node(last_root_word_df)
  
  # the ngram tokenization didn't get frequencies for 1-grams.
  # populate it from the tree
  for (node in ngram_tree$children){ 
    node$freq <- sum(node$Get("freq"), na.rm = TRUE) 
  }

  
  # > sum(node$Get("freq"), na.rm =TRUE)
  # [1] 138429
  # > sum(last_root_word_df$freq)
  # [1] 138429
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



search_tree <- function(ngram_tree, phrase, num_suggestions = 5) {
  words = strsplit(phrase, " ")
  words = unlist(words)
  # depth 1 is 'root'
  tree_depth = length(words) + 2
  print(sprintf("phrase: %s", phrase))
  print(sprintf("tree_depth: %s", tree_depth))
  subtree = ngram_tree$Climb(name=words)
  if (!is.null(subtree)){
    # print(subtree, "word", "freq", limit=10)
    results = subtree$Get(function(x) {c(x$name, as.numeric(x$freq))}, 
                                        filterFun = function(x){x$level==tree_depth})
    print(sprintf("phrase freq: %s", subtree$freq))
    max_range = min(num_suggestions, dim(results)[2])
    recommended_words = results[1, 1:max_range]
    # Calculate the likelihood that this word follows the search phrase.
    likelihood = sapply(results[2, 1:max_range], as.numeric)/subtree$freq 
    ret = rbind(recommended_words, likelihood)
  } else {
    print(sprintf("No suggestions for word after: '%s'", phrase))
    ret = NA
  }

  print(ret)
  ret
}






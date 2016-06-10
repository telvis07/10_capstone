library(data.tree)
library(dplyr)

tree_single_word <- function(min_frequency=10) {
  tmp <- system.time({
    last_root_word_df <- filter(datums$df_ngram_all, grepl("^last ", word) & freq > min_frequency)
    year_root_word_df <- filter(datums$df_ngram_all, grepl("^year ", word) & freq > min_frequency)
    all_data <- rbind(last_root_word_df, year_root_word_df)
    print(sprintf("rows after: %s", nrow(all_data)))
  })
  print(tmp)
  
  tmp <- system.time({
    all_data$pathString <- sapply(all_data$word, gen_path_string)
    ngram_tree <- as.Node(all_data)
  })
  print(tmp)
  
  # the ngram tokenization didn't get frequencies for 1-grams.
  # populate it from the tree
  for (node in ngram_tree$children){ 
    node$freq <- sum(node$Get("freq"), na.rm = TRUE) 
  }

  # > sum(node$Get("freq"), na.rm =TRUE)
  # [1] 138429
  # > sum(last_root_word_df$freq)
  # [1] 138429
  ngram_tree
}

build_tree <- function(all_ngram_df) {
  all_ngram_df$pathString <- sapply(all_ngram_df$word, gen_path_string)
  ngram_tree <- as.Node(all_ngram_df)

  # the ngram tokenization didn't get frequencies for 1-grams.
  # populate it from the tree
  for (node in ngram_tree$children){ 
    node$freq <- sum(node$Get("freq"), na.rm = TRUE) 
  }
  
  ngram_tree
}

build_tree_with_timing <- function(all_ngram_df) {
  # tmp <- system.time({
  #   print(sprintf("rows before: %s", nrow(datums$df_ngram_all)))
  #   all_data <- filter(datums$df_ngram_all, freq > min_frequency)
  #   print(sprintf("rows after: %s", nrow(all_data)))
  # })
  # print(tmp)
  # print("deleting datums - to make room for the tree")
  # rm(datums)
  
  tmp <- system.time({
    all_ngram_df$pathString <- sapply(all_ngram_df$word, gen_path_string)
    ngram_tree <- as.Node(all_ngram_df)
  })
  print(tmp)
  
  # the ngram tokenization didn't get frequencies for 1-grams.
  # populate it from the tree
  for (node in ngram_tree$children){ 
    node$freq <- sum(node$Get("freq"), na.rm = TRUE) 
  }
  
  # > sum(node$Get("freq"), na.rm =TRUE)
  # [1] 138429
  # > sum(last_root_word_df$freq)
  # [1] 138429
  ngram_tree
}

gen_path_string <- function (x) {
  paste("start", gsub(" ", "/", x), sep="/")
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

multi_search_tree <- function(ngram_tree, phrase, num_suggestions=5){
  words = strsplit(phrase, " ")
  words = unlist(words)
  recommended_words = c()
  likelihood = c()
  # print(words)
  
  for (i in seq_along(words)) {
    # https://en.wikipedia.org/wiki/Katz%27s_back-off_model
    # consider only doing backoff queries if the full phrase 
    # returns no results.
    search_words = tail(words, i)
    tmp <- system.time({
      ret = perform_search(ngram_tree = ngram_tree,
                        words = search_words,
                        num_suggestions = num_suggestions)
    })
    print(tmp)
    if(!is.null(ret)) {
      recommended_words = c(recommended_words, ret[1,])
      likelihood = c(likelihood, ret[2,])
    }
  }
  
  # MLE - maximum likelihood estimate
  max_range = min(num_suggestions, length(likelihood))
  ord = order(sapply(likelihood, as.numeric), decreasing = TRUE)
  recommendations = recommended_words[ord[1:max_range]]
  
  # print recommended words
  print("recommended words....")
  print(recommendations)
  
  ret = rbind(recommended_words, likelihood)
  ret
}

perform_search <- function(ngram_tree, words, num_suggestions = 5) {
  ret = NULL
  
  # tree_depth 1 is 'root'
  # tree_depth 2 is word #1 in the 1-gram
  # tree_depth 3 is word #2 in the 2-gram
  # tree_depth 4 is word #3 in the 3-gram
  # tree_depth 5 is word #4 in the 4-gram
  tree_depth = length(words) + 2
  subtree = ngram_tree$Climb(name=words)
  
  if (!is.null(subtree)){
    # there is a word path in the tree corresponding to the search phrase
    
    # print(subtree, "word", "freq", limit=10)
    results = subtree$Get(function(x) {c(x$name, as.numeric(x$freq))}, 
                                        filterFun = function(x){x$level==tree_depth})
    if (!is.null(results)){
      # the word path exists and there are words that follow
      
      # print(sprintf("phrase freq: %s", subtree$freq))
      max_range = min(num_suggestions, dim(results)[2])
      recommended_words = results[1, 1:max_range]
      
      # Calculate the likelihood that this word follows the search phrase.
      # print(order(sapply(results[2,], as.numeric), decreasing = TRUE))
      likelihood = sapply(results[2, 1:max_range], as.numeric)/subtree$freq 
      
      ret = rbind(recommended_words, likelihood)
    }
  } 
  
  if (is.null(ret)) {
    print(sprintf("No suggestions for word after: '%s'", paste(words, collapse=" ")))
  }

  ret
}






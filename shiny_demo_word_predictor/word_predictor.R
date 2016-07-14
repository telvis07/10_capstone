# Word Predictor for demo
library(tm)
library(dplyr)

preprocess_single_string <- function(s) {
  s <- removePunctuation(s)
  s <- removeNumbers(s)
  s <- tolower(s)
  s <- stripWhitespace(s)
  s
}

word_model_predict_query <- function(raw_phrase, 
                                     num_suggestions=3, 
                                     debug=F){
    null_predictions <- c("the", "to",  "and")
    
    if (nchar(raw_phrase)<2){
      recommended_words <- null_predictions
    } else {
      phrase <- preprocess_single_string(raw_phrase)
      
      if (debug) {
        print(phrase)
      }
      
      words = strsplit(phrase, " ")
      words = unlist(words)
      recommended_words = data.frame()
      
      # check the last N words, based on number of grams
      num_grams <- min(length(ngram_df_list), length(words))
      
      # for (i in seq_along(words)) {
      for (i in seq(num_grams, 1, -1)){
        # https://en.wikipedia.org/wiki/Katz%27s_back-off_model
        # consider only doing backoff queries if the full phrase 
        # returns no results.
        
        # This backoff method is called: 'stupid backoff'
        # 
        
        search_words = tail(words, i)
        if (debug) {
          print(search_words)
        }
        ret = perform_search_in_dataframe(ngram_df_list = ngram_df_list,
                                          words = search_words,
                                          num_suggestions = num_suggestions,
                                          debug=F)
        
        # this should be done in : compress_ngram_model_words()
        # ret <- filter(ret, ! word %in% stopwords("english"))
        
        if(nrow(ret)) {
          recommended_words = rbind(recommended_words, ret)
        }
        
        if (length(recommended_words)){
          break
        }
      }
      
      if(nrow(recommended_words)) {
        # MLE - maximum likelihood estimate
        max_range = min(num_suggestions, nrow(recommended_words))
        # prob = likelihood
        ord = order(recommended_words$prob, decreasing = TRUE)
        recommended_words <- recommended_words[ord,]$word
        
        # war crimes
        if (length(recommended_words) < num_suggestions) {
          recommended_words <- c(recommended_words, null_predictions)[1:3]
        }
      } else {
        recommended_words <- null_predictions
      }
    }
    
    recommended_words
}
perform_search_in_dataframe <- function(ngram_df_list, 
                                        words, 
                                        num_suggestions = 5, 
                                        min_frequency=0,
                                        debug=FALSE) {
  recommended_words = data.frame()
  joined_words <- paste(words, collapse = " ")
  print(sprintf("joined words: %s", joined_words))
  
  # finds all words with
  gram_length <- length(words)
  # root_ngram_index <- gram_length - 1
  next_ngram_index <- gram_length
  
  # ngram_df <- ngram_df_list[[root_ngram_index]]
  # root_ngram_df <- ngram_df[ngram_df$word == joined_words & ngram_df$freq > min_frequency,]
  # if (debug){
  #   print("root")
  #   print(root_ngram_df)
  # }
  
  # if (nrow(root_ngram_df) > 0)
  {
    # there is a word path in the tree corresponding to the search phrase
    next_ngram_df <- ngram_df_list[[next_ngram_index]]
    search_text <- paste0("^", joined_words, " ")
    if (debug) {
      print(search_text)
    }
    
    # results <- filter(next_ngram_df, grepl(search_text, word))
    results <- filter(next_ngram_df, root==joined_words)
    results$word <- sapply(results$word, 
                           function(x) {
                             w <- unlist(strsplit(x, " ")); 
                             tail(w,1)
                           })
    results <- filter(results, ! word %in% stopwords("english"))
    results <- results[order(results$prob, decreasing=T),]
    
    
    if (debug){
      print(head(results))
    }
    
    if (nrow(results) > 0){
      # the word path exists and there are words that follow
      
      # print(sprintf("phrase freq: %s", subtree$freq))
      max_range = min(num_suggestions, nrow(results))
      recommended_words = results[1:max_range,]
      
      # Calculate the likelihood that this word follows the search phrase.
      # print(order(sapply(results[2,], as.numeric), decreasing = TRUE))
      # recommended_words$likelihood <- recommended_words$freq/root_ngram_df[1,]$freq 
    }
  } 
  
  if (nrow(recommended_words) == 0) {
    print(sprintf("No suggestions for word after: '%s'", paste(words, collapse=" ")))
  }
  
  if(debug){
    print("recommended words")
    print(recommended_words)
  }
  recommended_words
}

load_ngram_models <- function() {
  ngram_2 <- read.table("models/ngram_df_list_ngram_2.csv", header=T, stringsAsFactors = F)
  ngram_3 <- read.table("models/ngram_df_list_ngram_3.csv", header=T, stringsAsFactors = F)
  ngram_4 <- read.table("models/ngram_df_list_ngram_4.csv", header=T, stringsAsFactors = F)
  
  # Combine all the word frequency data.frames
  ngram_df_list = list( "ngram_2"=ngram_2, 
                        "ngram_3"=ngram_3,
                        "ngram_4"=ngram_4)
  print("Loaded models")
  ngram_df_list
}

ngram_df_list <- load_ngram_models()

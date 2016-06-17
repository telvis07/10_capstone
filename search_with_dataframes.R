# searching using data.frames


perform_search_in_dataframe <- function(ngram_df_list, words, num_suggestions = 5, min_frequency=0) {
  recommended_words = data.frame()
  joined_words <- paste(words, collapse = " ")
  
  # finds all words with
  gram_length <- length(words)
  ngram_df <- ngram_df_list[[gram_length]]
  root_ngram_df <- ngram_df[ngram_df$word == joined_words,]
  print("root")
  print(root_ngram_df)
  
  if (nrow(root_ngram_df) > 0){
    # there is a word path in the tree corresponding to the search phrase
    next_ngram_df <- ngram_df_list[[gram_length+1]]
    search_text <- paste0("^", joined_words, " ")
    print(search_text)
    results <- filter(next_ngram_df, grepl(search_text, word) & freq > min_frequency)

    print(head(results))
    if (nrow(results) > 0){
      # the word path exists and there are words that follow
      
      # print(sprintf("phrase freq: %s", subtree$freq))
      max_range = min(num_suggestions, nrow(results))
      recommended_words = results[1:max_range,]
      
      # Calculate the likelihood that this word follows the search phrase.
      # print(order(sapply(results[2,], as.numeric), decreasing = TRUE))
      recommended_words$likelihood <- recommended_words$freq/root_ngram_df[1,"freq"] 
    }
  } 
  
  if (nrow(recommended_words) == 0) {
    print(sprintf("No suggestions for word after: '%s'", paste(words, collapse=" ")))
  }
  
  recommended_words
}
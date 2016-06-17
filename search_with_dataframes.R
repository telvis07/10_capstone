# searching using data.frames
source("analysis.R")
source("sample_data.R")

ngram_language_modeling_with_data_frames <- function(docs=NULL) {
  # How these probabilities are estimated is a matter of great interest in the area of
  # language modeling. The most straightforward way is take a word history and count
  # the different words which follow that word history. As language models are predictive
  # models, one wants to model future possible word sequences given what was seen
  # in training. This suggests a simple relative frequency as a probability estimate of a
  # sequence of words, which is better known as the maximum likelihood estimator (see
  #                                                                               [Manning and Sch¨utze, 1999]):
  
  # generate_sample_files()
  if (is.null(docs)) {
    docs <- load_sample_dircorpus()
    docs <- preprocess_entries(docs)
  }
  
  tmp <- system.time({
    ngram_1 <- get_docterm_matrix(docs, 1)
  })
  print(tmp)
  
  tmp <- system.time({
    ngram_2 <- get_docterm_matrix(docs, 2)
  })
  print(tmp)
  
  tmp <- system.time({
    # ngram_2$wf$root <- sapply(ngram_df_list$ngram_2$word, function(x) {unlist(strsplit(x, " "))[1]})
    ngram_2$wf$root <- sapply(ngram_2$wf$word, 
                              function(x) {
                                w <- unlist(strsplit(x, " "))[1]; 
                                paste(w, collapse = " ")
                              })
  })
  print(tmp)
  
  ngram_3 <- get_docterm_matrix(docs, 3)
  ngram_3$wf$root <- sapply(ngram_3$wf$word, 
                            function(x) {
                                w <- unlist(strsplit(x, " "))[1:2]; 
                                paste(w, collapse = " ")
                              })
  
  ngram_4 <- get_docterm_matrix(docs, 4)
  # ngram_4$wf$root <- sapply(ngram_df_list$ngram_4$word, function(x) {unlist(strsplit(x, " "))[1:3]})
  ngram_4$wf$root <- sapply(ngram_4$wf$word, 
                            function(x) {
                              w <- unlist(strsplit(x, " "))[1:3]; 
                              paste(w, collapse = " ")
                            })
  
  # Combine all the word frequency data.frames
  # ngram_all_df <- rbind(ngram_2$wf,
  #                       ngram_3$wf,
  #                       ngram_4$wf)
  ngram_df_list = list("ngram_1"=ngram_1$wf, 
                       "ngram_2"=ngram_2$wf, 
                       "ngram_3"=ngram_3$wf, 
                       "ngram_4"=ngram_4$wf)
  ngram_df_list
  
}

perform_search_in_dataframe <- function(ngram_df_list, words, num_suggestions = 5, min_frequency=0) {
  recommended_words = data.frame()
  joined_words <- paste(words, collapse = " ")
  
  # finds all words with
  gram_length <- length(words)
  ngram_df <- ngram_df_list[[gram_length]]
  root_ngram_df <- ngram_df[ngram_df$word == joined_words & ngram_df$freq > min_frequency,]
  print("root")
  print(root_ngram_df)
  
  if (nrow(root_ngram_df) > 0){
    # there is a word path in the tree corresponding to the search phrase
    next_ngram_df <- ngram_df_list[[gram_length+1]]
    search_text <- paste0("^", joined_words, " ")
    print(search_text)
    
    # results <- filter(next_ngram_df, grepl(search_text, word))
    results <- filter(next_ngram_df, root==root_ngram_df$word)


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
  
  print("recommended words")
  print(recommended_words)
  recommended_words
}

save_ngram_df_list <- function(ngram_df_list, save_file="data/ngram_df_list.5.percent.rds") {
  saveRDS(ngram_df_list, "data/ngram_df_list.5.percent.rds")  
}


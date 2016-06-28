# searching using data.frames
source("analysis.R")
source("sample_data.R")
library(quanteda)

multi_search_tree_with_data_frames <- function(ngram_df_list, 
                                               raw_phrase, 
                                               num_suggestions=3, 
                                               debug=FALSE){
  phrase <- preprocess_single_string(raw_phrase)
  print(phrase)
  words = strsplit(phrase, " ")
  words = unlist(words)
  recommended_words = data.frame()
  
  # check the last N words, based on number of grams
  num_grams <- min(length(ngram_df_list), length(words))

  # for (i in seq_along(words)) {
  for (i in seq(num_grams, 2, -1)){
    # https://en.wikipedia.org/wiki/Katz%27s_back-off_model
    # consider only doing backoff queries if the full phrase 
    # returns no results.
    search_words = tail(words, i)
    print(search_words)
    ret = perform_search_in_dataframe(ngram_df_list = ngram_df_list,
                                      words = search_words,
                                      num_suggestions = num_suggestions,
                                      debug=F)
    
    ret <- filter(ret, ! word %in% stopwords("english"))

    if(nrow(ret)) {
      recommended_words = rbind(recommended_words, ret)
    }
    
    # if (length(recommended_words) >= num_suggestions){
    #   break
    # }
  }
  
  if(nrow(recommended_words)) {
    # MLE - maximum likelihood estimate
    max_range = min(num_suggestions, nrow(recommended_words))
    ord = order(recommended_words$likelihood, decreasing = TRUE)
    recommended_words$word <- sapply(recommended_words$word, 
                                     function(x) {
                                       w <- unlist(strsplit(x, " ")); 
                                       tail(w,1)
                                     })
    
    
    # print recommended words
    if (debug) {
      print("recommended words....")
      print(recommended_words[ord,])
    }
    
    recommended_words <- recommended_words[ord,]
  }
  
  recommended_words
}


ngram_language_modeling_with_data_frames <- function(docs=NULL, doc_dir="./data/final/en_US/sample.1.percent/") {
  # How these probabilities are estimated is a matter of great interest in the area of
  # language modeling. The most straightforward way is take a word history and count
  # the different words which follow that word history. As language models are predictive
  # models, one wants to model future possible word sequences given what was seen
  # in training. This suggests a simple relative frequency as a probability estimate of a
  # sequence of words, which is better known as the maximum likelihood estimator (see
  #                                                                               [Manning and Sch¨utze, 1999]):
  
  # dirs
  # 1% : ./data/final/en_US/sample.1.percent
  # 25% : ./data/final/en_US/sample.25 
  # 100% : ./data/final/en_US/all
  # ngram_df_list <- ngram_language_modeling_with_data_frames(doc_dir = "./data/final/en_US/sample.25")
  
  # generate_sample_files()
  if (is.null(docs)) {
    print("loading corpus")
    docs <- load_sample_vec_corpus(sampledir=doc_dir)
    print("preprocessing entries")
    docs <- preprocess_entries(docs)
    docs <- corpus(docs)
  }
  
  # ngram_1 <- get_docterm_matrix(docs, 1)
  ngram_2 <- get_docterm_matrix(docs, 2)
  ngram_3 <- get_docterm_matrix(docs, 3, parent_words=ngram_2$wf$word)
  ngram_4 <- get_docterm_matrix(docs, 4, parent_words=ngram_3$wf$word)

  
  # Combine all the word frequency data.frames
  ngram_df_list = list( "ngram_2"=ngram_2$wf, 
                        "ngram_3"=ngram_3$wf,
                        "ngram_4"=ngram_4$wf)

  ngram_df_list
  
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
  root_ngram_index <- gram_length - 1
  next_ngram_index <- gram_length
  
  ngram_df <- ngram_df_list[[root_ngram_index]]
  root_ngram_df <- ngram_df[ngram_df$word == joined_words & ngram_df$freq > min_frequency,]
  if (debug){
    print("root")
    print(root_ngram_df)
  }
  
  if (nrow(root_ngram_df) > 0){
    # there is a word path in the tree corresponding to the search phrase
    next_ngram_df <- ngram_df_list[[next_ngram_index]]
    search_text <- paste0("^", joined_words, " ")
    if (debug) {
      print(search_text)
    }
    
    # results <- filter(next_ngram_df, grepl(search_text, word))
    results <- filter(next_ngram_df, root==root_ngram_df$word)
    results$word <- sapply(results$word, 
                           function(x) {
                             w <- unlist(strsplit(x, " ")); 
                             tail(w,1)
                           })
    results <- filter(results, ! word %in% stopwords("english"))
    results <- results[order(results$freq, decreasing=T),]
    

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
      recommended_words$likelihood <- recommended_words$freq/root_ngram_df[1,"freq"] 
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

save_ngram_df_list <- function(ngram_df_list, save_file="data/ngram_df_list.25.percent.rds") {
  saveRDS(ngram_df_list, save_file)  
}

run_quiz_sentences <- function(ngram_df_list) {
  queries <- c(
    "The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
    "You're the reason why I smile everyday. Can you follow me please? It would mean the",
    "Hey sunshine, can you follow me and make me the",
    "Very early observations on the Bills game: Offense still struggling but the",
    "Go on a romantic date at the",
    "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
    "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
    "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
    "Be grateful for the good times and keep the faith during the",
    "If this isn't the cutest thing you've ever seen, then you must be"
  )
  
  answers <- c(
    "beer",
    "world",
    "happiest",
    "crowd",
    "beach",
    "way",
    "time",
    "fingers",
    "bad",
    "insane"
  )
  i <- 1
  num_correct <- 0
  for (query in queries) {
    print("************")
    print(query)
    res <- multi_search_tree_with_data_frames(ngram_df_list, query, num_suggestions = 10)
    suggested_words <- res[1:3,]$word
    
    correct <- answers[i] %in% suggested_words
    if (correct) {
      num_correct <- num_correct + 1
    }
    print(sprintf("suggested_words: %s, Correct: :%s", paste(suggested_words, collapse=","), correct))
    i <- i+1
    print("-----------")
    print(res)
  }
  print("************")
  print(sprintf("Got %s of %s", num_correct, length(answers)))
}

build_final_model <- function() {
  doc_dir <- "./data/final/en_US/all"
  docs <- load_sample_vec_corpus(sampledir=doc_dir)
  saveRDS("data/docs_vec_corpus.rds")
  # reinit()
  
  docs <- readRDS("data/docs_vec_corpus.rds")
  docs <- preprocess_entries(docs)
  saveRDS("data/preprocessed_docs.rds")
  # reinit()
  
  docs <- readRDS("data/preprocessed_docs.rds")
  docs <- corpus(docs)
  saveRDS("data/quanteda_corpus_docs.rds")
  # reinit()
  
  docs <- readRDS("data/quanteda_corpus_docs.rds")
  ngram_df_list <- ngram_language_modeling_with_data_frames(docs=docs)
  saveRDS("data/ngram_df_list.rds")
  # reinit()
  
  ngram_df_list <- readRDS("data/ngram_df_list.rds")
  run_quiz_sentences(ngram_df_list = ngram_df_list)
}


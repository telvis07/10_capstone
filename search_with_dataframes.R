# searching using data.frames
source("analysis.R")
source("sample_data.R")
library(quanteda)

multi_search_tree_with_data_frames <- function(ngram_df_list, 
                                               raw_phrase, 
                                               num_suggestions=5, 
                                               debug=FALSE){
  phrase <- preprocess_single_string(raw_phrase)
  print(phrase)
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
    print(search_words)
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


ngram_language_modeling_with_data_frames <- function(docs=NULL, 
                                                     doc_dir="./data/final/en_US/sample.1.percent/",
                                                     prune_cover_percentage=0.66) {
  # How these probabilities are estimated is a matter of great interest in the area of
  # language modeling. The most straightforward way is take a word history and count
  # the different words which follow that word history. As language models are predictive
  # models, one wants to model future possible word sequences given what was seen
  # in training. This suggests a simple relative frequency as a probability estimate of a
  # sequence of words, which is better known as the maximum likelihood estimator (see
  #                                                                               [Manning and Sch¨utze, 1999]):
  
  # dirs
  # 1% : doc_dir <- "./data/final/en_US/sample.1.percent"
  # 25% : doc_dir <- "./data/final/en_US/sample.25" 
  # 100% : doc_dir <- "./data/final/en_US/all"
  # ngram_df_list <- ngram_language_modeling_with_data_frames(doc_dir = doc_dir)
  
  # generate_sample_files()
  if (is.null(docs)) {
    print("loading corpus")
    docs <- load_sample_vec_corpus(sample_dir=doc_dir)
    print("preprocessing entries")
    docs <- preprocess_entries(docs)
    docs <- corpus(docs)
  }
  
  ngram_1 <- get_docterm_matrix(docs, 1)
  ngram_2 <- get_docterm_matrix(docs, 2,
                                n_minus_1_gram_model = ngram_1$wf,
                                prune_cover_percentage=0.66)
  print("writing data/ngram_df_list_ngram_2.csv")
  write.table(ngram_2$wf, "data/ngram_df_list_ngram_2.csv")
  
  ngram_3 <- get_docterm_matrix(docs, 3, n_minus_1_gram_model = ngram_2$wf,
                                prune_cover_percentage=prune_cover_percentage)
  print("writing data/ngram_df_list_ngram_3.csv")
  write.table(ngram_3$wf, "data/ngram_df_list_ngram_3.csv")
  
  ngram_4 <- get_docterm_matrix(docs, 4, n_minus_1_gram_model=ngram_3$wf,
                                prune_cover_percentage=prune_cover_percentage)
  print("writing data/ngram_df_list_ngram_4.csv")
  write.table(ngram_4$wf, "data/ngram_df_list_ngram_4.csv")
  
  ngram_2 <- compress_ngram_model_words(ngram_2)
  write.table(ngram_2$wf, "data/ngram_df_list_ngram_2.compressed.csv")
  ngram_3 <- compress_ngram_model_words(ngram_3)
  write.table(ngram_3$wf, "data/ngram_df_list_ngram_3.compressed.csv")
  ngram_4 <- compress_ngram_model_words(ngram_4)
  write.table(ngram_4$wf, "data/ngram_df_list_ngram_4.compressed.csv")


  # Combine all the word frequency data.frames
  ngram_df_list = list( "ngram_2"=ngram_2$wf,
                        "ngram_3"=ngram_3$wf,
                        "ngram_4"=ngram_4$wf)

  ngram_df_list
}

compress_ngram_model_words <- function(ngram_model) {
  
  # Maybe do this in a post-processing step, prior to writing the
  # model as a CSV to disk.
  print("removing root from word")
  ngram_model$word <- sapply(ngram_model$word,
                       function(x) {
                         w <- unlist(strsplit(x, " "));
                         tail(w,1)
                       })
  
  print("removing where word=='stopword'")
  ngram_model <- filter(ngram_model, ! word %in% stopwords("english"))
  ngram_model <- ngram_model[order(ngram_model$freq, decreasing=T),]
  ngram_model
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

save_ngram_df_list <- function(ngram_df_list, save_file="data/ngram_df_list.25.percent.rds") {
  saveRDS(ngram_df_list, save_file)  
}

predict_test_data <- function(ngram_df_list, 
                              test_queries_df,
                              num_suggestions = 5) {
  
  # if (quiz) {
  #   test_queries_df <- generate_quiz_1_data()
  # } else {
  #   test_queries_df <- generate_test_data()
  # }
  
  queries <- test_queries_df$queries
  answers <- test_queries_df$answers
  
  missed_queries <- c()
  correct_answers <- c()
  model_answers <- c()
    
  i <- 1
  num_correct <- 0
  for (query in queries) {
    print("************")
    print(query)
    res <- multi_search_tree_with_data_frames(ngram_df_list, query, num_suggestions = num_suggestions)
    num_suggestions <- min(nrow(res), num_suggestions)
    suggested_words <- res[1:num_suggestions,]$word
    is_correct <- answers[i] %in% suggested_words
    correct_answers <- c(correct_answers, is_correct)
    model_answers <- c(model_answers, paste(suggested_words, collapse=","))
 
    print(sprintf("suggested_words: %s, Correct: :%s", paste(suggested_words, collapse=","), is_correct))
    i <- i+1
    print("-----------")
    print(res)
  }
  print("************")
  test_queries_df$correct <- correct_answers
  test_queries_df$y_hat <- model_answers 
  num_correct = sum(correct_answers)
  print(sprintf("Got %s of %s", num_correct, length(answers)))
  
  
  test_summary <- list (
                accuracy = num_correct/nrow(test_queries_df),
                predictions = test_queries_df
  )
  
  test_summary
}


generate_queries_and_answers <- function(files, num_lines=25) {
  queries <- c()
  answers <- c()
  
  for (fn in files){
    for (line in readLines(file.path(sample_dir, fn),  n=num_lines)) {
      s <- removePunctuation(line)
      s <- tolower(s)
      s <- removeNumbers(s)
      s <- stripWhitespace(s)
      s <- stringi::stri_split_charclass(s, "\\p{WHITE_SPACE}")
      s <- lapply(s, function(x) x <- x[which(x != "")])
      s <- unlist(s)
      
      if (length(s) < 3)
        next
  
      query <- paste0(head(s, length(s)-1), collapse = " ")
      answer <- tail(s, 1)
      queries <- c(queries, query)
      answers <- c(answers, answer)
    }
  }
  
  data.frame(queries=queries, answers=answers, stringsAsFactors = F)
}

generate_queries_and_answers_from_csv <- function(csv_fn="data/final_model_csv/testing.csv",
                                                  num_lines=25,
                                                  seed=123) {
  # queries <- generate_queries_and_answers_from_csv(csv_fn="data/final_model_csv/testing.csv", num_lines = -1)
  # queries <- generate_queries_and_answers_from_csv(csv_fn="data/final_model_csv/training.csv", num_lines = -1)
  queries <- c()
  answers <- c()
  texts <- read.csv(csv_fn, stringsAsFactors = F, nrows = num_lines)$text
  if (num_lines > 0){
    set.seed(seed)
    ss <- sample(1:length(texts), num_lines, replace=F)
    print(ss)
    texts <- texts[ss]
  }
  
  for (line in texts) {
    s <- removePunctuation(line)
    s <- tolower(s)
    s <- removeNumbers(s)
    s <- stripWhitespace(s)
    s <- stringi::stri_split_charclass(s, "\\p{WHITE_SPACE}")
    s <- lapply(s, function(x) x <- x[which(x != "")])
    s <- unlist(s)
    
    if (length(s) < 3)
      next
    
    query <- paste0(head(s, length(s)-1), collapse = " ")
    answer <- tail(s, 1)
    queries <- c(queries, query)
    answers <- c(answers, answer)
  }
  
  data.frame(queries=queries, answers=answers, stringsAsFactors = F)
}

generate_quiz_1_data <- function() {
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
  data.frame(queries=queries, answers=answers, stringsAsFactors = F)

}

generate_quiz_2_data <- function() {
  queries <- c(
    "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
    "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
    "I'd give anything to see arctic monkeys this",
    "Talking to your mom has the same effect as a hug and helps reduce your",
    "When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
    "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
    "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
    "Every inch of you is perfect from the bottom to the",
    "I’m thankful my childhood was filled with imagination and bruises from playing",
    "I like how the same people are in almost all of Adam Sandler's"
  )
  
  answers <- c(
    "die", # missed
    "marital", # missed (quiz incorrect. tried: financial)
    "weekend", # missed
    "stress", # missed
    "minute", # correct (quiz incorrect. tried: look, minute)
    "matter", # correct (quiz incorrect. tried: case)
    "hand", # missed
    "top", # correct
    "outside", # missed
    "movies" # missed
  )
  data.frame(queries=queries, answers=answers, stringsAsFactors = F)
  
}


store_datatables <- function(ngram_df_list) {
  write.table(ngram_df_list$ngram_2, "data/ngram_df_list_ngram_2.csv")
  write.table(ngram_df_list$ngram_3, "data/ngram_df_list_ngram_3.csv")
  write.table(ngram_df_list$ngram_4, "data/ngram_df_list_ngram_4.csv")
}

load_datatables <- function(){
  
  # http://www.inside-r.org/packages/cran/data.table/docs/fread
  # fread()
  # > object.size(ngram_df_list)
  # 1542808816 bytes
  # > class(ngram_df_list)
  # [1] "list"
  # > class(ngram_df_list$ngram_2)
  # [1] "data.table" "data.frame"
  # > class(ngram_df_list$ngram_3)
  # [1] "data.table" "data.frame"
  # > class(ngram_df_list$ngram_4)
  # [1] "data.table" "data.frame"
  ngram_2 <- read.table( "data/final_model_ngrams/ngram_df_list_ngram_2.csv", header=T, stringsAsFactors = F)
  ngram_3 <- read.table( "data/final_model_ngrams/ngram_df_list_ngram_3.csv", header=T, stringsAsFactors = F)
  ngram_4 <- read.table( "data/final_model_ngrams/ngram_df_list_ngram_4.csv", header=T, stringsAsFactors = F)
  
  # Combine all the word frequency data.frames
  ngram_df_list = list( "ngram_2"=ngram_2, 
                        "ngram_3"=ngram_3,
                        "ngram_4"=ngram_4)
  
  ngram_df_list
}

build_final_model <- function() {
  doc_dir <- "./data/final/en_US/all"
  docs <- load_sample_vec_corpus(sampledir=doc_dir)
  saveRDS(docs, "data/docs_vec_corpus.rds")
  # reinit()
  
  docs <- readRDS("data/docs_vec_corpus.rds")
  docs <- preprocess_entries(docs)
  saveRDS(docs, "data/preprocessed_docs.rds")
  # reinit()
  
  docs <- readRDS("data/preprocessed_docs.rds")
  docs <- corpus(docs)
  saveRDS(docs, "data/quanteda_corpus_docs.rds")
  # reinit()
  
  # docs <- readRDS("data/quanteda_corpus_docs.rds")
  # load the training data
  docs <- textfile("data/final_model_csv/training.csv", textField="texts")
  docs <- corpus(docs)
  print(ndoc(docs))
  
  # train the model
  ngram_language_modeling_with_data_frames(docs=docs,prune_cover_percentage=0.66)
  saveRDS(ngram_df_list, "data/ngram_df_list.rds")
  # reinit()
  
  test_data_queries <- generate_queries_and_answers_from_csv(csv_fn="data/final_model_csv/testing.csv", 
                                                             num_lines = 25)
  results <- predict_test_data(ngram_df_list, test_data_queries)

  
  ngram_df_list <- readRDS("data/ngram_df_list.rds")
  # run_quiz_sentences(ngram_df_list = ngram_df_list)
  test_queries_df <- generate_quiz_1_data()
  predict_test_data(ngram_df_list = ngram_df_list, test_queries_df)

}



build_ngram_4_partition <- function () {
  # train the 4-grams by partitioning into 10 batches
  # ngram_4_merged <- build_ngram_4_partition()
  
  print("Loading docs")
  docs <- textfile("data/final_model_csv/training.csv", textField="texts")
  docs <- corpus(docs)
  print(ndoc(docs))
  
  print("loading ngram3")
  ngram_3 <- read.table( "data/final_model_ngrams/ngram_df_list_ngram_3.csv",
                         stringsAsFactors = F)
  parent_words <- ngram_3$word
  
  # start processing corpus in 10 batches
  num_batches <- 10
  batch_size <- floor(ndoc(docs) / num_batches)
  ngram_4_merged <- NULL
  
  # iterate over batches
  for (offset in seq(1, ndoc(docs), batch_size)) {
    offset <- floor(offset)
    end <- offset+batch_size
    if (end > ndoc(docs)){
      end <- ndoc(docs)
    }
    print(sprintf("***** Batch %s:%s *****", offset, end))
    
    docs_ss <- docs[seq(offset, end),]
    docs_ss <- corpus(docs_ss)
    print(sprintf("partition size: %s", ndoc(docs_ss)))
    
    # generate frequency data.table for 4-grams
    # set cover percentage to 100% so we keep all the n-grams
    ngram_4 <- get_docterm_matrix(docs_ss, 4, parent_words=parent_words,
                                  prune_cover_percentage=1.00)
    print("deleting docs_ss")
    rm(docs_ss); gc()
    if (is.null(ngram_4_merged)){
      ngram_4_merged <- ngram_4$wf
    } else {
      print(sprintf("Merging: before %s", nrow(ngram_4_merged)))
      ngram_4_all_merged <- merge(ngram_4_merged, ngram_4$wf, by="word", all=T)
      print("merging freq")
      ngram_4_all_merged$freq <- apply(ngram_4_all_merged[,list(freq.x, freq.y)], 
                                       1,
                                      function(x){
                                        if (is.na(x[1])){ret <- x[2]}
                                        else if (is.na(x[2])){ret <-x[1] }
                                        else{ret <- sum(x)}
                                        ret
                                      })
      print("merging root")
      ngram_4_all_merged$root <- apply(ngram_4_all_merged[, list(root.x, root.y)],
                                  1,
                                  function(x){
                                    if (is.na(x[1])){ret <- x[2]}
                                    else {ret <- x[1]} 
                                    ret
                                  })
      # head(ngram_4_all_merged[freq.x>0 & freq.y>0])
      print("removing .x and .y")
      ngram_4_all_merged <- ngram_4_all_merged[,freq.x:=NULL]
      ngram_4_all_merged <- ngram_4_all_merged[,freq.y:=NULL]
      ngram_4_all_merged <- ngram_4_all_merged[,root.x:=NULL]
      ngram_4_all_merged <- ngram_4_all_merged[,root.y:=NULL]
      
      # sorting
      print("sorting")
      setkey(ngram_4_all_merged, word)
      setorder(ngram_4_all_merged, -freq)
      ngram_4_merged <- ngram_4_all_merged
      print(sprintf("Merging: after %s", nrow(ngram_4_merged)))  
      
      print("removing ngram_4_all_merged")
      rm(ngram_4_all_merged); gc()
    }
  }
  
  ngram_4_merged <- prune_ngram_df_by_cover_percentage(ngram_4_merged, 
                                                       percentage=0.66)
  write.table(ngram_4_merged, "data/final_model_ngrams/ngram_df_list_ngram_4.csv")
  ngram_4_merged
  
  

  # 
  # 
  # 
  # print(sprintf("num parent words %s", length(parent_words)))
  # ngram_4 <- get_docterm_matrix(docs, 4, parent_words=parent_words,
  #                               prune_cover_percentage=0.66)
  # print("writing data/ngram_df_list_ngram_4.csv")
  # write.table(ngram_4$wf, "data/final_model_ngrams/ngram_df_list_ngram_4.csv")
}


ngram_4_merging_shenanigans <- function () {
  
  
  # > head(ngram_4_all_merged[freq.x>0 & freq.y>0])
  # word freq.x            root.x freq.y            root.y freq
  # 1:      a acre site at      2       a acre site      1       a acre site    3
  # 2: a advantage after a      2 a advantage after      1 a advantage after    3
  # 3:        a all tie in      1         a all tie      1         a all tie    2
  # 4:     a am phone call      1        a am phone      1        a am phone    2
  # 5:          a and a to      1           a and a      1           a and a    2
  # 6:        a as well as      2         a as well      1         a as well    3
  # root
  # 1:       a acre site
  # 2: a advantage after
  # 3:         a all tie
  # 4:        a am phone
  # 5:           a and a
  # 6:         a as well
}

build_ngram_4 <- function () {
  # This fails because get_docterm_matrix() fails for 4grams 
  # on 100% of the corpus 
  
  print("Loading docs")
  docs <- textfile("data/final_model_csv/training.csv", textField="texts")
  docs <- corpus(docs)
  print(ndoc(docs))
  
  print("loading ngram3")
  ngram_3 <- read.table( "data/final_model_ngrams/ngram_df_list_ngram_3.csv", 
                         stringsAsFactors = F)
  parent_words <- ngram_3$word
  print(sprintf("num parent words %s", length(parent_words)))
  ngram_4 <- get_docterm_matrix(docs, 4, parent_words=parent_words,
                                prune_cover_percentage=0.66)
  print("writing data/ngram_df_list_ngram_4.csv")
  write.table(ngram_4$wf, "data/final_model_ngrams/ngram_df_list_ngram_4.csv")
}

run_test_data <- function () {
  ngram_df_list <- load_datatables()
  
  queries <- generate_queries_and_answers_from_csv(csv_fn="data/final_model_csv/testing.csv", num_lines = 100)
  test_summary <- predict_test_data(ngram_df_list = ngram_df_list, queries)
  write.csv(test_summary, "test_queries.csv")
  
  queries <- generate_queries_and_answers_from_csv(csv_fn="data/final_model_csv/training.csv", num_lines = 100)
  test_summary <- predict_test_data(ngram_df_list = ngram_df_list, queries)
  write.csv(test_summary, "training_queries.csv")
}

run_quiz_1_data <- function(ngram_df_list) {
  test_queries_df <- generate_quiz_1_data()
  predict_test_data(ngram_df_list = ngram_df_list, test_queries_df)
}






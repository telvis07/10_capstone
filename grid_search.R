source("sample_data.R")
source("search_with_dataframes.R")

grid_search <- function(seed=5678, sample_len=0.10) {
  # coverage_params <- seq(.10, 1.0, .1)
  # coverage_params <- seq(.60)
  coverage_params <- seq(.33, 1.0, .33)
  # coverage_params <- c(1.0)
  num_iterations <- 5
  all_files <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")

  c_list <- c()
  test_accuracies <- c()
  train_accuracies <- c()
  set.seed(seed)
  
  for (i in seq(num_iterations)) {
    train_dir <- sprintf("training/train.%s", i)
    test_dir <- sprintf("training/test.%s", i)
    print(train_dir)
    print(test_dir)
    
    for (sample_dir in c(train_dir, test_dir)) {
      # check for data zip
      if (file.exists(sample_dir)){
        next
      }
      
      # generate sample files
      generate_sample_files(sample_dir = sample_dir, 
                            seed=NULL,
                            sample_len=sample_len)
    }
    
    docs <- load_sample_vec_corpus(sample_dir=train_dir)
    docs <- preprocess_entries(docs)
    docs <- corpus(docs)
    
    for (prune_cover_percentage in coverage_params) { 
      
      # TODO: revisit bootstrap sampling for grid search
      # ss <- sample(1:ndoc(docs), replace=T)
      # sampled_docs <- docs[ss]
      # names(sampled_docs) <- NULL
      # sampled_docs <- corpus(sampled_docs)
      
      # Examples:
        
      # # sampling from a corpus
      # summary(sample(inaugCorpus, 5)) 
      # summary(sample(inaugCorpus, 10, replace=TRUE))
      # # sampling from a dfm
      # myDfm <- dfm(inaugTexts[1:10], verbose = FALSE)
      # sample(myDfm)[, 1:10]
      # sample(myDfm, replace = TRUE)[, 1:10]
      # sample(myDfm, what = "features")[1:10, ]

      # train the model
      ngram_model <- ngram_language_modeling_with_data_frames(docs=docs,
                                                              prune_cover_percentage=prune_cover_percentage)
      
      # generate data from with queries and answers
      text_files <- sapply(all_files, file.path, train_dir)
      train_data_queries <- generate_queries_and_answers(text_files, num_lines=100)
      text_files <- sapply(all_files, file.path, test_dir)
      test_data_queries <- generate_queries_and_answers(text_files, num_lines=100) 
      
      # predict on training data
      results <- predict_test_data(ngram_model, train_data_queries)
      train_accuracies <- c(train_accuracies, results$accuracy)
      print(sprintf(""))
      
      # predict on test data
      results <- predict_test_data(ngram_model, test_data_queries)
      test_accuracies <- c(test_accuracies, results$accuracy)
      
      # 
      c_list <- c(c_list, prune_cover_percentage)
      
      # print progress after each session
      res <- data.frame(prune_cover_percentage=c_list,
                        train_accuracy=train_accuracies,
                        test_accuracy=test_accuracies)
      print(res)
    }

  }
  res
}
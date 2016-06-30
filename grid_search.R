source("sample_data.R")
source("search_with_dataframes.R")

grid_search <- function(seed=4567, sample_len=0.01) {
  # cover_params <- seq(.10, 1.0, .1)
  coverage_params <- seq(.60)
  i <- 1
  
  test_accuracies <- c()
  train_accuracies <- c()
  
  for (prune_cover_percentage in coverage_params) {
    
    train_dir <- sprintf("training/train.%s", i)
    test_dir <- sprintf("training/test.%s", i)
    
    for (sample_dir in c(train_dir, test_dir)) {
      generate_sample_files(sample_dir = train_dir, 
                            seed=seed,
                            sample_len=sample_len)
    }
    
    # train the model
    ngram_model <- ngram_language_modeling_with_data_frames(doc_dir=train_dir,
                                                            prune_cover_percentage=prune_cover_percentage)
    # generate data from with queries and answers
    train_data_queries <- generate_queries_and_answers(train_dir)
    test_data_queries <- generate_queries_and_answers(test_dir) 
    
    # run predictions
    train_data_results <- predict_test_data(ngram_model, train_data_queries)
    test_data_results <- predict_test_data(ngram_model, test_data_queries)
    
    # collect the accuracy values
    test_accuracies <- c(test_accuracies, train_data_results$accuracy)
    train_accuracies <- c(train_accuracies, test_data_results$accuracy)
  }
  
  data.frame(prune_cover_percentage=coverage_params,
             train_accuracy=train_accuracies,
             test_accuracy=test_accuracies)
}
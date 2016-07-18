Final project for the Data Science Specialization Capstone Course

## Links

- [Shiny application that demos the predictor](https://technicalelvis.shinyapps.io/shiny_demo_word_predictor/)
- [Presentation Slides](https://telvis07.github.io/slides_demo_word_predictor/)
- [1st Milestone Report](http://rpubs.com/telvis/capstone_report_1)


## Files

analysis.R : code to generate ngrams
- `fetch_capstone_data` : fetch the Capstone data
- `preprocess_entries` : perform text preprocessing and data cleanup
- `get_docterm_matrix` : function used to generate ngram model and compute the Maximum Likelihood Estimate for each ngram.


search\_with\_dataframes.R : code to build ngram models and perform search using the models
- `ngram_language_modeling_with_data_frames` : train models on 2-grams, 3-grams and 4-grams on sampled data
- `multi_search_tree_with_data_frames` : predict function to estimate the next word for an input. Performs `stupid backoff` from ngram-4, to ngram-3 or ngram-2 
  if a model yields no results.
- `predict_test_data` : predict the model accuracy for test data
- `generate_queries_and_answers`, `generate_queries_and_answers_from_csv`, `generate_quiz_1_data`, `generate_quiz_2_data` : 
   methods to generate test data for `predict_test_data`
- `build_ngram_4_partition` : experimental code to build a model with 100% of the data


grid\_search.R 
- `grid_search` : Attempt to find to the optimal value for "ngram coverage" to prune the ngram models using a grid search.


sample\_data.R 
- `sample_capstone_data`, `generate_sample_files` : methods to generate samples of raw training data.

See [steps.md](steps.md) for steps to build the ngram models.



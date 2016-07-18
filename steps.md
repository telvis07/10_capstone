This doc describes in a nutshell how I built the ngram models.

## Steps to build the model
```r
source("sample_data.R")
source("search_with_dataframes.R")
source("analysis.R")
```

Generate 25% sample

```r
fetch_capstone_data()
generate_sample_files(seed=4567, sample_len=0.25, sample_dir="./data/final/en_US/sample.25")
```


train on 25% sampled data
```r
doc_dir <- "./data/final/en_US/sample.25"
ngram_df_list <- ngram_language_modeling_with_data_frames(doc_dir = doc_dir)
run_quiz_1_data(ngram_df_list)
```

store the models as data.table files. I also copied these to the shiny app.
```r
store_prod_models(ngram_df_list)
```

test on quiz 1 data
```r
queries <- generate_quiz_1_data()
predict_test_data(ngram_df_list, queries)
```

test on quiz 2 data
```r
queries <- generate_quiz_2_data()
predict_test_data(ngram_df_list, queries)
```

test/train split data
```r
queries <- generate_queries_and_answers_from_csv(csv_fn="data/final_model_csv/testing.csv", num_lines = 100)
predict_test_data(ngram_df_list, queries)
```
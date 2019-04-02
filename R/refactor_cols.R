refactor_cols = function(df, cols) {
    df %>% mutate_at(vars(cols), as.factor)
}


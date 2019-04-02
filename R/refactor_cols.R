refactor_cols = function(df, cols) {
    df[,cols] = lapply(df[, cols], as.factor)
}

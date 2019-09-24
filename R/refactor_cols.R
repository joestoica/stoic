#' Refactor Columns
#'
#' @param df A dataframe
#' @param cols A vector of columns that are to be turned into factors.
#'
#' @return a dataframe
#'
refactor_cols = function(df, cols) {
    df %>% mutate_at(vars(cols), as.factor)
}

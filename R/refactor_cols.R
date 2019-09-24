#' Refactor Columns
#'
#' @param df A dataframe
#' @param cols A vector of columns that are to be turned into factors.
#'
#' @return a dataframe
#'
#' @export

refactor_cols = function(df, cols) {
    library(dplyr)
    library(magrittr)
    df %>% mutate_at(vars(cols), as.factor)
}

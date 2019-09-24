#' Tidy Names
#'
#' @param df A dataframe object
#'
#' @return A vector of column names that are lowercase with spaces, periods,
#' and hyphens replaced with dashes
#'
#' @examples
tidy_names <- function(df) {
    return(gsub(pattern = "(\\s|\\.|-)",
                replacement =  "_",
                x = tolower(names(df))))

}

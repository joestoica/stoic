#' Clean Dataframe Names
#'
#' @param df A Dataframe object
#'
#' @return A vector that is the names of the dataframe with spaces, periods, and
#' hyphens replaced with an underscore.
#'
#' @examples
#' clean_df_names(iris)
#' @export
clean_df_names <- function(df) {
    return(gsub(pattern = "(\\s|\\.|-)",
                replacement =  "_",
                x = tolower(names(df))))

}

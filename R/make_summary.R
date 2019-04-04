#' Makes a summary table of specific data frame columns given certain functions.
#'
#' \code{make_summary} returns a data frame that summarizes each input column using
#'   the functions given in a supplied vector.
#'
#' @param df data frame that supplies the data
#' @param columns A numerical or character vector indicating the columns that are to be selected.
#' @param funs a character vector that supplies the functions that will be used to summarize the columns
#' @param range boolean to determine whether range should be calculated, TRUE by default
#'
#' @return A data frame with number of rows equal to length of the column
#'   vector and the number of columns equal to the length of funs.
#'
#' @examples
#' make_summary(iris, 1:4)
#' make_summary(iris, c("Sepal.Length", "Sepal.Width"))
#' #' make_summary(iris, 1:4, funs = c("length", "mean", "var", "sd", "median", "min"), range = FALSE)
#'
#' \dontrun{
#' make_summary(iris, c(Sepal.Length, Sepal.Width))
#' make_summary(iris, 1:4, funs = c("length", "mean", "var", "sd", "median", "min"), range = TRUE)
#' }
#' @export


make_summary <- function(df, columns,
                         funs = c("length", "mean", "var", "sd", "median", "max", "min"), range = TRUE,
                         drop = TRUE) {

    # Helper function that applies every input function to the certain column
    make_summary_helper = function(x, funs) {
        sapply(funs, function(f) f(x))
    }

    # select columns from df
    df = df[columns]

    # get the names from the df to be used later as variable names
    names = colnames(df)

    # apply make_summary_helper to every column in df and return a dataframe
    df_summary = data.frame(t(apply(df, 2, make_summary_helper, funs = sapply(funs, get))))

    # cbind the names from earlier to the summary table
    df_summary = cbind(names, df_summary)

    # rename columns
    names(df_summary) <- c("Variable", funs)

    # Calculate range if applicable
    if (range == TRUE) {
        if(!"max" %in% funs | !"min" %in% funs) stop("Cannot calculate range without max or min")
    }

    # If range calculation criteria are met, calculate
    if ("max" %in% funs & "min" %in% funs & range == TRUE) {

        df_summary$range = df_summary$max - df_summary$min

        # reorder columns
        if (drop == TRUE) {
            df_summary = df_summary[, c(2,3,4,5,6,9,7,8)]
        } else {
            df_summary = df_summary[, c(1,2,3,4,5,6,9,7,8)]
        }
    }

    return(df_summary)
}

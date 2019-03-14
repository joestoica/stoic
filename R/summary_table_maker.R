make_summary_helper <- function(x) {
    funs <- c(length, mean, var, sd, min, max, skewness, kurtosis, median)
    sapply(funs, function(f) f(x))
}

make_summary <- function(df, columns, title) {

    df = iris

    columns = c(1,2,3,4)

    df = df %>%
        select(columns)

    title = "x"

    names = colnames(df)

    df_summary <- apply(df, 2, make_summary_helper) %>%
        t() %>%
        data.frame()

    df_summary <- cbind(names, df_summary)

    names(df_summary) <- c("Variable", "length", "mean", "var", "sd", "min", "max", "skewness", "kurtosis", "median")

    df_summary <- df_summary %>%
        mutate(range = max - min) %>%
        select(Variable, length, mean, var, sd, median, max, min, range, skewness, kurtosis)

    return(df_summary)
}

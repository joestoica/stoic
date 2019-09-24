#' Linear Mixed Models Check Assumptions
#'
#' @param model A linear mixed model fitted using the lme4 package
#'
#' @return Plots for to check normality assumptions for residuals
#'  and random effects
#' @export
#'
#' @examples


lmm_assumption <- function(model){
    library(ggplot2)
    library(magrittr)
    model_df = data.frame(f = fitted(model), resid = resid(model), ranef(model)$id)

    names(model_df)[3] = "ranef"

    resid_plot = model_df %>%
        ggplot(aes(f, resid))+
        geom_point()+
        geom_hline(yintercept = 0, color = "red")+
        theme_stoic()+
        labs(x = "Fitted",
             y = "Residual",
             title = "Fitted vs. Residual Plot")

    resid_qq = model_df %>%
        ggplot(aes(sample = resid))+
        geom_qq()+
        geom_qq_line()+
        theme_stoic()+
        labs(x = "Theoretical",
             y = "Sample",
             title = "Q-Q Plot for residuals")


    resid_dens = model_df %>%
        ggplot(aes(resid))+
        geom_density(fill = stoic.styles["med"])+
        theme_stoic()+
        labs(x = "Residual",
             y = "Density",
             title = "Density plot for residuals")


    ranef_qq = model_df %>%
        ggplot(aes(sample = ranef))+
        geom_qq()+
        geom_qq_line()+
        theme_stoic()+
        labs(x = "Theoretical",
             y = "Sample",
             title = "Q-Q Plot for random effects")

    ranef_dens = model_df %>%
        ggplot(aes(ranef))+
        geom_density(fill = stoic.styles["med"])+
        theme_stoic()+
        labs(x = "Random Effect",
             y = "Density",
             title = "Density plot for random effects")

    return(list(resid_plot = resid_plot,
                resid_qq = resid_qq, resid_dens = resid_dens,
                ranef_qq = ranef_qq, ranef_dens = ranef_dens))
}

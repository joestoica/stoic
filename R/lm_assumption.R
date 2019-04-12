lm_assumption <- function(model){
    p1 <- data.frame(f = fitted(model), r = resid(model)) %>%
        ggplot(aes(f, r))+
        geom_point()+
        geom_smooth(color = "red")+
        theme_stoic()+
        labs(x = "Fitted",
             y = "Residuals",
             title = "Fitted and Residual Plot")

    p2 <- data.frame(f = fitted(model), r = resid(model)) %>%
        ggplot(aes(sample = r))+
        geom_qq(alpha = 0.5)+
        geom_qq_line()+
        theme_stoic()+
        labs(x = "Theoretical",
             y = "Sample",
             title = "Q-Q Plot for Residuals")

    vif = vif(model)

    return(list(resid = p1, qq = p2, vif = vif))
}

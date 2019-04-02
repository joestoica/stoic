 # ggplot2 theme
theme_stoic <- function(){

    theme_bw(base_size=12, base_family="sans") %+replace%
        theme(axis.line = element_blank(),
              axis.text = element_text(color = "#9B9B9B", size = 10),
              axis.ticks = element_blank(),
              axis.title = element_text(),
              legend.background = element_blank(),
              legend.box = "vertical",
              legend.direction = "vertical",
              legend.position = "right",
              legend.box.background = element_blank(),
              panel.border = element_blank(),
              panel.grid = element_line(colour = NULL),
              panel.grid.major = element_line(colour = "#D2D2D2"),
              panel.grid.minor = element_blank(),
              plot.margin = unit(c(1,1, 1, 1), "lines"),
              plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
              strip.background = element_blank())
}

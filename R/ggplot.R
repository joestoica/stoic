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
              strip.background = element_rect())
}

# Fivethirtyeight
# function (base_size = 12, base_family = "sans")
# {
#     (theme_foundation(base_size = base_size, base_family = base_family) +
#          theme(line = element_line(colour = "black"),
#                rect = element_rect(fill = ggthemes_data$fivethirtyeight["ltgray"], linetype = 0, colour = NA),
#                text = element_text(colour = ggthemes_data$fivethirtyeight["dkgray"]),
#                axis.title = element_blank(),
#                axis.text = element_text(),
#                axis.ticks = element_blank(),
#                axis.line = element_blank(),
#                legend.background = element_rect(),
#                legend.position = "bottom",
#                legend.direction = "horizontal",
#                legend.box = "vertical",
#                panel.grid = element_line(colour = NULL),
#                panel.grid.major = element_line(colour = ggthemes_data$fivethirtyeight["medgray"]),
#                panel.grid.minor = element_blank(),
#                plot.title = element_text(hjust = 0, size = rel(1.5), face = "bold"),
#                plot.margin = unit(c(1, 1, 1, 1), "lines"),
#                strip.background = element_rect()))
# }

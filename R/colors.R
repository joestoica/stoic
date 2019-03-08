## Color Pallette

# Palette colors
stoic.styles <- c(
    `red` = "#ca6093",
    `blue`= "#6093ca",
    `light` = "#F0F0F0",
    `med` = "#D2D2D2",
    `dark` = "#3C3C3C")

# Fn to extract them by hex codes
styles <- function(...) {
    cols <- c(...)

    if (is.null(cols))
        return (stoic.styles)

    stoic.styles[cols]
}

# Create separate palettes
stoic.palettes <- list(
    `diverging`  = styles('red','light', 'blue'),
    `cool_b`  = styles('blue', 'light'),
    `cool_r`  = styles('red', 'light'),
    `qual` = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')
)

# Function to access colors
stoic_pal <- function(palette = "diverging", reverse = FALSE, ...) {
    pal <- stoic.palettes[[palette]]

    if (reverse) pal <- rev(pal)

    colorRampPalette(pal, ...)
}

# Function for customer scale
scale_color_stoic <- function(palette = "diverging", discrete = TRUE, reverse = FALSE, ...) {
    pal <- stoic_pal(palette = palette, reverse = reverse)

    #' Scale color using AgC color palette.
    #' @param palette: diverging, greens or greys
    #' @param discrete: T or F
    #' @param reverse: reverse the direction of the color scheme

    if (discrete) {
        discrete_scale("colour", paste0("stoic_", palette), palette = pal, ...)
    } else {
        scale_color_gradientn(colours = pal(256), ...)
    }
}

scale_fill_stoic <- function(palette = "diverging", discrete = TRUE, reverse = FALSE, ...) {

    #' Scale fill using AgC color palette.
    #' @param palette: diverging, greens or greys
    #' @param discrete: T or F
    #' @param reverse: reverse the direction of the color scheme

    pal <- stoic_pal(palette = palette, reverse = reverse)

    if (discrete) {
        discrete_scale("fill", paste0("stoic_", palette), palette = pal, ...)
    } else {
        scale_fill_gradientn(colours = pal(256), ...)
    }
}

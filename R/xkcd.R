#' View XKCD Comics
#'
#' @param comic_number Any number from an XKCD comic
#' @param random A boolean specifying a random comic
#'
#' @return An XKCD comic with relevant comic data
#' @export
#'
#' @examples
#' xkcd(1423) # a comic
#' xkcd() # most recent comic
#' xkcd(random = TRUE) # a random comic
#' \dontrun{
#' xkcd(961) # gif won't load in ggplot (is this possible?)
#' xkcd(404) # 404 page won't work
#' xkcd(3000) # A comic outside the current range, which fails
#' }

xkcd <- function(comic_number = NULL, random = FALSE){
    library(RJSONIO)
    library(png)
    library(jpeg)
    library(grid)
    library(ggplot2)

    xkcd_meta_data <- fromJSON("https://xkcd.com/info.0.json")
    available_comics = 1:xkcd_meta_data$num

    if (random == TRUE){

        # 404 page, and gif comics that I know of
        available_comics <- available_comics[-c(404, 961, 1116, 1264)]

        comic_number = sample(available_comics, 1)
    }

    if (is.null(comic_number)){ # this will be the most recent comic
        comic_number = xkcd_meta_data$num
    } else {

        # comic number outside range
        if (comic_number > xkcd_meta_data$num){
            stop("Comic does not exist (yet)")
        }

        # 404 is the only comic number I've found that doesn't exist
        if (comic_number == 404 ){
            stop("Comic 404 does not exist.")
        }

        # this will get the comic with specified comic_number
        url <- paste0("https://xkcd.com/", comic_number, "/info.0.json")
        xkcd_meta_data <- fromJSON(url)
    }

    # Get comic data
    num <- xkcd_meta_data$num
    title <- xkcd_meta_data$title
    imgsrc <- xkcd_meta_data$img
    alt = xkcd_meta_data$alt
    date = paste0(xkcd_meta_data$month, "/",
                  xkcd_meta_data$day, "/",
                  xkcd_meta_data$year)

    # Create a temporary file to download the image
    z <- tempfile()
    download.file(imgsrc, z, mode = "wb",quiet = TRUE)

    # some of the files are JPG, some are PNG, this handles both (not gifs)
    filetype = substr(imgsrc, nchar(imgsrc) - 2, nchar(imgsrc))
    if (filetype == "png"){
        img <- readPNG(z)
    } else if (filetype == "jpg") {
        img <- readJPEG(z)
    } else {
        gif_error = paste0("Error: Filetype not supported- .", filetype)
        return(gif_error)
    }

    # Delete temp file
    file.remove(z)

    g <- rasterGrob(img, interpolate=TRUE)

    # The ggplot to display the comic
    plot <- ggplot(data = data.frame(1:10, 1:10)) +
        annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
        geom_blank()+
        theme_void()

    # Print number
    cat(paste0("Number: ", num))

    # Print the date
    cat(paste0("\nDate: ", date))

    # Print the title
    cat(paste0("\nTitle: ", title))

    # Print  the alt
    cat(paste0("\nAlt: ", alt))
    print(plot)

    # This function controls what the user wants to do next, TODO make pretty!
    what_now <- function() {
        cat("\n\n1. Prev Comic \n2. Next Comic \n3. Random Comic \n4. Quit\n")
        what <- readline()
        return(as.integer(what))
    }

    # run the prompt
    res = what_now()

    # Do what the user wants
    if (res == 1){
        xkcd(comic_number = comic_number - 1)
    } else if (res == 2){
        xkcd(comic_number = comic_number + 1)
    } else if (res == 3){
        xkcd(random = TRUE)
    } else if (res == 4){
        return(cat("Goodbye!"))
    } else {
        # if the user does anything outside 1:4
        cat("Try again!\n")
        res = what_now()
    }
}



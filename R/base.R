#' @export
"[.magick-image" <- function(x, i){
  assert_image(x)
  stopifnot(is.numeric(i))
  i <- seq_along(x)[i]  # normalize to positive subscripts
  magick_image_subset(x, i)
}

#' @export
"[<-.magick-image" <- function(x, i, value){
  assert_image(x)
  assert_image(value)
  stopifnot(is.numeric(i))
  i <- seq_along(x)[i]
  magick_image_replace(x, i, value)
}

#TODO: return 3 ch 'rgb' or 1 ch greyscale bitmap depending on colorspace
#' @export
"[[.magick-image" <- function(x, i){
  assert_image(x)
  image_data(x[i])
}

#' @export
"[[<-.magick-image" <- function(x, i, value){
  stop("[[ assignment not implemented. Try single bracket.")
}

#' @export
"print.bitmap" <- function(x, ...){
  dims <- dim(x)
  cat(sprintf("%d channel %dx%d bitmap array:", dims[1], dims[2], dims[3]))
  utils::str(x)
}

#' @export
"as.integer.bitmap" <- function(x, transpose = TRUE, ...){
  if(transpose){
    x <- aperm(x)
  }
  structure(as.vector(x, mode = 'integer'), dim = dim(x))
}

#' @export
"as.double.bitmap" <- function(x, transpose = TRUE, ...){
  if(transpose){
    x <- aperm(x)
  }
  structure(as.vector(x, mode = 'double') / 255, dim = dim(x))
}

#' @export
"as.integer.magick-image" <- function(x, ...){
  magick_image_write_integer(x)
}

#' @export
"c.magick-image" <- function(...){
  image_join(...)
}

#' @export
"length.magick-image" <- function(x){
  if(magick_image_dead(x))
    return(NULL)
  assert_image(x)
  magick_image_length(x)
}

#' @export
"rev.magick-image" <- function(x){
  assert_image(x)
  magick_image_rev(x)
}

#' @export
"as.list.magick-image" <- function(x, ...){
  assert_image(x)
  len <- length(x)
  lapply(seq_len(len), magick_image_subset, image = x)
}

#' @export
"rep.magick-image" <- function(x, ...){
  assert_image(x)
  rep_magick_image(x, ...)
}

rep_magick_image <- function(x, times){
  image_join(lapply(seq_len(times), function(...){
    x
  }))
}

#' @export
"print.magick-image" <- function(x, info = TRUE, ...){
  img <- x
  is_knit_image <- isTRUE(getOption('knitr.in.progress'))
  if(isTRUE(getOption('jupyter.in_kernel'))){
    jupyter_print_image(img)
  } else if(!is_knit_image && !magick_image_dead(x) && length(img)){
    previewer <- getOption('magick.viewer')
    if(is.function(previewer)){
      previewer(img)
    }
  }
  if(isTRUE(info))
    print(image_info(x))
  if(is_knit_image)
    `knit_print.magick-image`(x)
  else
    invisible()
}



# This is registered as an S3 method in .onLoad()
"knit_print.magick-image" <- function(x, ..., inline = FALSE){
  if(!length(x))
    return(invisible())

  in_base_dir <- utils::getFromNamespace('in_base_dir', 'knitr')
  ext <- ifelse(all(tolower(image_info(x)$format) == "gif"), "gif", "png")

  if (inline) {
    fig_path <- knitr::opts_chunk$get("fig.path")
    n <- knitr::opts_chunk$get("fig.inline.cur")
    if (is.null(n)) {
      n <- 1
    }
    knitr::opts_chunk$set("fig.inline.cur" = n+1)

    tmp <- file.path(
      fig_path,
      paste0("inline-fig-", n, ".", ext)
    )
  } else {
    plot_counter <- utils::getFromNamespace('plot_counter', 'knitr')
    tmp <- knitr::fig_path(ext, number = plot_counter())
  }

  # save relative to 'base' directory, see discussion in #110
  in_base_dir({
    dir.create(dirname(tmp), showWarnings = FALSE, recursive = TRUE)
    image_write(x, path = tmp, format = ext)
  })
  knitr::include_graphics(tmp)
}

#' @export
#' @importFrom grDevices as.raster
"as.raster.magick-image" <- function(image, native = FALSE, ...){
  assert_image(image)
  if(isTRUE(native))
    return(as.integer(image))
  bitmap <- image_write_frame(image, format = "rgba")
  magick_image_as_raster(bitmap)
}

#' @export
#' @importFrom graphics plot
"plot.magick-image" <- function(x, ...){
  plot(as.raster(x), ...)
}

image_preview <- function(img, max_width = 800, max_len = 10, viewer = getOption('viewer')){
  if(is.function(viewer)){
    viewer_supported <- c("bmp", "png", "jpeg", "jpg", "svg", "gif", "webp")
    format <- tolower(image_info(img[1])$format)
    len <- length(img)
    info <- image_info(img)
    if(len > 1 && format != "gif"){
      if(info$width[1] > max_width){
        img <- image_resize(img, paste0(max_width, 'x'))
      }
      if(len > max_len){
        i <- round(seq(1, len, length.out = max_len))
        img <- img[i]
      } else {
        i <- seq_len(len)
      }
      img <- image_annotate(img, paste0("[preview] frame ", i, "/", len), size = 18, font = 'mono',
                            location = '+10+10', color = 'white', boxcolor = 'black')
      img <- image_animate(img, fps = 1)
      format <- "gif"
    } else if(is.na(match(format, viewer_supported))){
      img <- image_convert(img, "PNG")
      format <- 'png'
    }
    tmp <- file.path(tempdir(), paste0("preview.", format))
    image_write(img, path = tmp, format = format)
    viewer(tmp)
  }
}

jupyter_print_image <- function(img){
  if(!length(img))
    return()
  format <- tolower(image_info(img[1])$format)
  if(!(format %in% c("png", "jpg", "jpeg", "svg", "gif")))
    format <- "png"
  tmp <- image_write(img, format = format)
  switch (format,
    png = IRdisplay::display_png(tmp),
    jpg = IRdisplay::display_jpeg(tmp),
    jpeg = IRdisplay::display_jpeg(tmp),
    gif = display_gif(tmp),
    svg = IRdisplay::display_svg(rawToChar(tmp))
  )
}

## Placeholder until IRdisplay::display_gif() is available
display_gif <- function(buf){
  contents <- jsonlite::base64_enc(buf)
  IRdisplay::display_html(sprintf('<img src="data:image/gif;base64,%s">', contents))
}

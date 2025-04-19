# R/filters.R


#' Convert pixel to greyscale
#'
#' This function takes an RGB pixel vector and returns a greyscale version.
#'
#' @param rgb A numeric vector of length 3 representing red, green, and blue values.
#' @return A raw vector of length 3 with equal gray values in R, G, and B.

#' @export
convert_to_grey <- function(rgb) {
  r <- as.numeric(rgb[1])
  g <- as.numeric(rgb[2])
  b <- as.numeric(rgb[3])
  grey <- 0.299 * r + 0.587 * g + 0.114 * b
  grey_raw <- as.raw(round(grey))
  return(rep(grey_raw, 3))
}

#' Red filter function
#' Applies a red filter by preserving the red channel and setting green and blue to zero.
#'
#' @param rgb A numeric vector of length 3 representing red, green, and blue values.
#' @return A raw vector of length 3 with the red channel preserved and the green/blue channels set to zero.
#'
#' @export
convert_to_red <- function(rgb) {
  r <- as.numeric(rgb[1])
  r_raw <- as.raw(round(r))
  return(c(r_raw, as.raw(0), as.raw(0)))  # Keep red, zero out green and blue
}

#' Green filter function
#' Applies a green filter by preserving the green channel and setting red and blue to zero.
#'
#' @param rgb A numeric vector of length 3 representing red, green, and blue values.
#' @return A raw vector of length 3 with the green channel preserved and the red/blue channels set to zero.
#'
#' @export
convert_to_green <- function(rgb) {
  g <- as.numeric(rgb[2])
  g_raw <- as.raw(round(g))
  return(c(as.raw(0), g_raw, as.raw(0)))  # Keep green, zero out red and blue
}

#' Blue filter function
#' @param rgb A numeric vector of length 3 representing red, green, and blue values.
#' @return A raw vector of length 3 with the blue channel preserved and the red/green channels set to zero.
#'
#' @export
convert_to_blue <- function(rgb) {
  b <- as.numeric(rgb[3])
  b_raw <- as.raw(round(b))
  return(c(as.raw(0), as.raw(0), b_raw))  # Keep blue, zero out red and green
}






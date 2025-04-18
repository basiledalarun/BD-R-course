# Load required package
library(magick)

# Load the image from your working directory
my_image <- image_read("~/Library/CloudStorage/OneDrive-KarolinskaInstitutet/Desktop/image.jpg")

print(my_image)


# Convert image to raw RGB array (channels x width x height)
img_data <- image_data(my_image, channels = "rgb")

# Grayscale conversion function
convert_to_grey <- function(rgb) {
  r <- as.numeric(rgb[1])
  g <- as.numeric(rgb[2])
  b <- as.numeric(rgb[3])
  grey <- 0.299 * r + 0.587 * g + 0.114 * b
  grey_raw <- as.raw(round(grey))
  return(rep(grey_raw, 3))
}

# Get dimensions
dims <- dim(img_data)  # 3 x width x height

# Apply grayscale to each pixel
for (x in 1:dims[2]) {
  for (y in 1:dims[3]) {
    rgb_pixel <- c(img_data[1, x, y], img_data[2, x, y], img_data[3, x, y])
    grey_pixel <- convert_to_grey(rgb_pixel)
    img_data[1, x, y] <- grey_pixel[1]
    img_data[2, x, y] <- grey_pixel[2]
    img_data[3, x, y] <- grey_pixel[3]
  }
}

# Reconstruct the image from modified pixel data
grey_image <- image_read(img_data)

# Display the grayscale image
print(grey_image)


#Red filter function

convert_to_red <- function(rgb) {
  r <- as.numeric(rgb[1])
  r_raw <- as.raw(round(r))
  return(c(r_raw, as.raw(0), as.raw(0)))  # Keep red, zero out green and blue
}

# Get dimensions
dims <- dim(img_data)  # 3 x width x height

# Apply red filter to each pixel
for (x in 1:dims[2]) {
  for (y in 1:dims[3]) {
    rgb_pixel <- c(img_data[1, x, y], img_data[2, x, y], img_data[3, x, y])
    red_pixel <- convert_to_red(rgb_pixel)
    img_data[1, x, y] <- red_pixel[1]  # Red
    img_data[2, x, y] <- red_pixel[2]  # Green = 0
    img_data[3, x, y] <- red_pixel[3]  # Blue = 0
  }
}

# Reconstruct the image from modified pixel data
red_image <- image_read(img_data)

# Display the red-filtered image
print(red_image)

####################
#Lets do green # Green filter function ###


convert_to_green <- function(rgb) {
  g <- as.numeric(rgb[2])
  g_raw <- as.raw(round(g))
  return(c(as.raw(0), g_raw, as.raw(0)))  # Keep green, zero out red and blue
}

# Get dimensions
dims <- dim(img_data)  # 3 x width x height

# Apply green filter to each pixel
for (x in 1:dims[2]) {
  for (y in 1:dims[3]) {
    rgb_pixel <- c(img_data[1, x, y], img_data[2, x, y], img_data[3, x, y])
    green_pixel <- convert_to_green(rgb_pixel)
    img_data[1, x, y] <- green_pixel[1]  # Red = 0
    img_data[2, x, y] <- green_pixel[2]  # Green
    img_data[3, x, y] <- green_pixel[3]  # Blue = 0
  }
}

# Reconstruct the image from modified pixel data
green_image <- image_read(img_data)

# Display the green-filtered image
print(green_image)


####################
#Lets do BLUE filter function

# Blue filter function
convert_to_blue <- function(rgb) {
  b <- as.numeric(rgb[3])
  b_raw <- as.raw(round(b))
  return(c(as.raw(0), as.raw(0), b_raw))  # Keep blue, zero out red and green
}

# Get dimensions
dims <- dim(img_data)  # 3 x width x height

# Apply blue filter to each pixel
for (x in 1:dims[2]) {
  for (y in 1:dims[3]) {
    rgb_pixel <- c(img_data[1, x, y], img_data[2, x, y], img_data[3, x, y])
    blue_pixel <- convert_to_blue(rgb_pixel)
    img_data[1, x, y] <- blue_pixel[1]  # Red = 0
    img_data[2, x, y] <- blue_pixel[2]  # Green = 0
    img_data[3, x, y] <- blue_pixel[3]  # Blue
  }
}

# Reconstruct the image from modified pixel data
blue_image <- image_read(img_data)

# Display the blue-filtered image
print(blue_image)





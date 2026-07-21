##Metadata----------------------------------------------------------------------

# Author   : Augustin DEBLY
# Contact  : augustin.debly@gmail.com
# Github   : github.com/augustindebly

##Functions---------------------------------------------------------------------

#Note that the functions will need the package "terra"

contrast_correction <- function(band, lower_pct = 0.002, upper_pct = 0.998) {
  lower     = quantile(values(band), lower_pct)[[1]]
  upper     = quantile(values(band), upper_pct)[[1]]
  
  stretched = (band - lower) / (upper - lower) * 255
  stretched = clamp(stretched, lower = 0, upper = 255)
  
  return(stretched)
}

correct_underwater_image <- function(input_path, output_path, lower_pct = 0.002, upper_pct = 0.998) {
  
  img = rast(input_path)
  
  r = img[[1]]
  g = img[[2]]
  b = img[[3]]
  
  r_corrected = contrast_correction(r, lower_pct, upper_pct)
  g_corrected = contrast_correction(g, lower_pct, upper_pct)
  b_corrected = contrast_correction(b, lower_pct, upper_pct)
  
  img_corrected        = c(r_corrected, g_corrected, b_corrected)
  names(img_corrected) = c("red", "green", "blue")
  
  img_corrected        = flip(img_corrected)
  
  writeRaster(img_corrected, output_path, overwrite = TRUE, datatype = "INT1U")
}

##Export RData------------------------------------------------------------------

save(contrast_correction, correct_underwater_image, file = "functions.RData")
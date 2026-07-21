##Metadata----------------------------------------------------------------------

# Author   : Augustin DEBLY
# Contact  : augustin.debly@gmail.com
# Github   : github.com/augustindebly

##Package-----------------------------------------------------------------------

library(terra)

##Functions---------------------------------------------------------------------

load("functions.RData")

##Script------------------------------------------------------------------------

input_dir   = "INPUT_images"
output_dir  = "OUTPUT_images"

name_images = list.files(input_dir, full.names = FALSE)
p_input     = paste(input_dir, name_images, sep = "/")
p_output    = paste(output_dir, paste(substr(name_images, 1, nchar(name_images) - 4), "_corrected.tif", sep = ""), sep = "/")

for(i in seq(length(name_images))){
  pin  = p_input[i]
  pout = p_output[i]
  
  correct_underwater_image(input_path = pin, output_path = pout)
}
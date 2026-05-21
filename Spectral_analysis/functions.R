##Metadata----------------------------------------------------------------------

# Author   : Augustin DEBLY
# Contact  : augustin.debly@gmail.com
# Github   : github.com/augustindebly

##Functions---------------------------------------------------------------------

resampling.hyperspectral.spectrum <- function(hyperspectral.wavelength.vector  = NULL,
                                              hyperspectral.reflectance.vector = NULL,
                                              spectral.response.functions.list = NULL) {
  
  resampled.wavelength.vector  = numeric(length(spectral.response.functions.list))
  resampled.reflectance.vector = numeric(length(spectral.response.functions.list))
  
  for (band in seq_along(spectral.response.functions.list)) {
    integral.numerator.reflectance     = sum((diff(hyperspectral.wavelength.vector) * (spectral.response.functions.list[[band]](hyperspectral.wavelength.vector) * hyperspectral.reflectance.vector)[-1]))
    integral.denominator.reflectance   = sum(diff(hyperspectral.wavelength.vector) * spectral.response.functions.list[[band]](hyperspectral.wavelength.vector)[-1])
    integral.numerator.wavelength      = sum((diff(hyperspectral.wavelength.vector) * (spectral.response.functions.list[[band]](hyperspectral.wavelength.vector) * hyperspectral.wavelength.vector)[-1]))
    integral.denominator.wavelength    = sum(diff(hyperspectral.wavelength.vector) * spectral.response.functions.list[[band]](hyperspectral.wavelength.vector)[-1])
    resampled.reflectance.vector[band] = integral.numerator.reflectance / integral.denominator.reflectance
    resampled.wavelength.vector[band]  = integral.numerator.wavelength / integral.denominator.wavelength
  }
  
  return(list(resampled.wavelength.vector, resampled.reflectance.vector))
}

##Export RData------------------------------------------------------------------

save(resampling.hyperspectral.spectrum, file = "functions.RData")
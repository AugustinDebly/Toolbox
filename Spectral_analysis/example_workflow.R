##Metadata----------------------------------------------------------------------

# Author   : Augustin DEBLY
# Contact  : augustin.debly@gmail.com
# Github   : github.com/augustindebly

##Package-----------------------------------------------------------------------

library(ggplot2)

##Functions---------------------------------------------------------------------

load("SRFs.RData")
load("functions.RData")

##Path--------------------------------------------------------------------------

path.data = "example_data.csv"

##Script------------------------------------------------------------------------

data         = read.csv(path.data, sep = ";", dec = ".")

M3M.spectrum = resampling.hyperspectral.spectrum(hyperspectral.wavelength.vector  = data$X,
                                                 hyperspectral.reflectance.vector = data$Y,
                                                 spectral.response.functions.list = SRFs_M3M)

df           = data.frame(wavelength  = data$X,
                          reflectance = data$Y,
                          G           = SRF_M3M_G(data$X),
                          R           = SRF_M3M_R(data$X),
                          RE          = SRF_M3M_RE(data$X),
                          NIR         = SRF_M3M_NIR(data$X))

##Plot--------------------------------------------------------------------------

ggplot(df, aes(x = wavelength)) +
  geom_line(aes(y = reflectance), linewidth = 1,        colour = "lightgray") +
  geom_line(aes(y = G),           linetype  = "dashed", colour = "darkgreen") +
  geom_line(aes(y = R),           linetype  = "dashed", colour = "red") +
  geom_line(aes(y = RE),          linetype  = "dashed", colour = "darkred") +
  geom_line(aes(y = NIR),         linetype  = "dashed", colour = "black") +
  geom_line(data = data.frame(wavelength = M3M.spectrum[[1]], reflectance = M3M.spectrum[[2]]), aes(x = wavelength, y = reflectance), inherit.aes = F, linewidth = 1) +
  theme_bw() +
  labs(
    x = "Wavelength (nm)",
    y = "Reflectance"
  )
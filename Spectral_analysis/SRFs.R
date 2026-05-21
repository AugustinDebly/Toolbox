##Metadata----------------------------------------------------------------------

# Author   : Augustin DEBLY
# Contact  : augustin.debly@gmail.com
# Github   : github.com/augustindebly

##Path--------------------------------------------------------------------------

SRFs_S2A = "SRFs/S2A/S2A.csv"
SRFs_S2B = "SRFs/S2B/S2B.csv"

##Functions---------------------------------------------------------------------

SRF_model <- function(x,a,b,c,d){
  # Generate a channel for sensors which don't have SRF data, with a 
  # 4-parameter Gaussian model :
  # a : amplitude (between 0 and 1)
  # b : width
  # c : shape (1 : Gaussian ; +inf : rectangular)
  # d : center wavelength
  return(a*exp(-((1/b/b)*(x-d)**2)**c))
}

SRF_from_raw_data <- function(x,x_data_nm,y_data){
  m   = matrix(rep(x_data_nm, each = length(x)), ncol = length(x_data_nm))
  ind = apply(abs(m-x), 1, which.min)
  return(y_data[ind])
}

#S2A
data_S2A = read.csv(SRFs_S2A,header = T,sep="\t")

SRF_S2A_B1 <- function(x){
  return(SRF_from_raw_data(x,data_S2A$SR_WL,data_S2A$S2A_SR_AV_B1))
}
SRF_S2A_B2 <- function(x){
  return(SRF_from_raw_data(x,data_S2A$SR_WL,data_S2A$S2A_SR_AV_B2))
}
SRF_S2A_B3 <- function(x){
  return(SRF_from_raw_data(x,data_S2A$SR_WL,data_S2A$S2A_SR_AV_B3))
}
SRF_S2A_B4 <- function(x){
  return(SRF_from_raw_data(x,data_S2A$SR_WL,data_S2A$S2A_SR_AV_B4))
}
SRF_S2A_B5 <- function(x){
  return(SRF_from_raw_data(x,data_S2A$SR_WL,data_S2A$S2A_SR_AV_B5))
}
SRF_S2A_B6 <- function(x){
  return(SRF_from_raw_data(x,data_S2A$SR_WL,data_S2A$S2A_SR_AV_B6))
}
SRF_S2A_B7 <- function(x){
  return(SRF_from_raw_data(x,data_S2A$SR_WL,data_S2A$S2A_SR_AV_B7))
}
SRF_S2A_B8 <- function(x){
  return(SRF_from_raw_data(x,data_S2A$SR_WL,data_S2A$S2A_SR_AV_B8))
}
SRF_S2A_B8A <- function(x){
  return(SRF_from_raw_data(x,data_S2A$SR_WL,data_S2A$S2A_SR_AV_B8A))
}
SRF_S2A_B9 <- function(x){
  return(SRF_from_raw_data(x,data_S2A$SR_WL,data_S2A$S2A_SR_AV_B9))
}
SRF_S2A_B10 <- function(x){
  return(SRF_from_raw_data(x,data_S2A$SR_WL,data_S2A$S2A_SR_AV_B10))
}
SRF_S2A_B11 <- function(x){
  return(SRF_from_raw_data(x,data_S2A$SR_WL,data_S2A$S2A_SR_AV_B11))
}
SRF_S2A_B12 <- function(x){
  return(SRF_from_raw_data(x,data_S2A$SR_WL,data_S2A$S2A_SR_AV_B12))
}
SRFs_S2A = list(SRF_S2A_B1,SRF_S2A_B2,SRF_S2A_B3,SRF_S2A_B4,SRF_S2A_B5,SRF_S2A_B6,SRF_S2A_B7,SRF_S2A_B8,SRF_S2A_B8A,SRF_S2A_B9,SRF_S2A_B10,SRF_S2A_B11,SRF_S2A_B12)

#S2B
data_S2B = read.csv(SRFs_S2B,header = T,sep="\t")

SRF_S2B_B1 <- function(x){
  return(SRF_from_raw_data(x,data_S2B$SR_WL,data_S2B$S2B_SR_AV_B1))
}
SRF_S2B_B2 <- function(x){
  return(SRF_from_raw_data(x,data_S2B$SR_WL,data_S2B$S2B_SR_AV_B2))
}
SRF_S2B_B3 <- function(x){
  return(SRF_from_raw_data(x,data_S2B$SR_WL,data_S2B$S2B_SR_AV_B3))
}
SRF_S2B_B4 <- function(x){
  return(SRF_from_raw_data(x,data_S2B$SR_WL,data_S2B$S2B_SR_AV_B4))
}
SRF_S2B_B5 <- function(x){
  return(SRF_from_raw_data(x,data_S2B$SR_WL,data_S2B$S2B_SR_AV_B5))
}
SRF_S2B_B6 <- function(x){
  return(SRF_from_raw_data(x,data_S2B$SR_WL,data_S2B$S2B_SR_AV_B6))
}
SRF_S2B_B7 <- function(x){
  return(SRF_from_raw_data(x,data_S2B$SR_WL,data_S2B$S2B_SR_AV_B7))
}
SRF_S2B_B8 <- function(x){
  return(SRF_from_raw_data(x,data_S2B$SR_WL,data_S2B$S2B_SR_AV_B8))
}
SRF_S2B_B8A <- function(x){
  return(SRF_from_raw_data(x,data_S2B$SR_WL,data_S2B$S2B_SR_AV_B8A))
}
SRF_S2B_B9 <- function(x){
  return(SRF_from_raw_data(x,data_S2B$SR_WL,data_S2B$S2B_SR_AV_B9))
}
SRF_S2B_B10 <- function(x){
  return(SRF_from_raw_data(x,data_S2B$SR_WL,data_S2B$S2B_SR_AV_B10))
}
SRF_S2B_B11 <- function(x){
  return(SRF_from_raw_data(x,data_S2B$SR_WL,data_S2B$S2B_SR_AV_B11))
}
SRF_S2B_B12 <- function(x){
  return(SRF_from_raw_data(x,data_S2B$SR_WL,data_S2B$S2B_SR_AV_B12))
}
SRFs_S2B = list(SRF_S2B_B1,SRF_S2B_B2,SRF_S2B_B3,SRF_S2B_B4,SRF_S2B_B5,SRF_S2B_B6,SRF_S2B_B7,SRF_S2B_B8,SRF_S2B_B8A,SRF_S2B_B9,SRF_S2B_B10,SRF_S2B_B11,SRF_S2B_B12)

##Export RData------------------------------------------------------------------

save(SRF_from_raw_data, SRF_model, SRFs_S2A, SRFs_S2B, file = "SRFs.RData")
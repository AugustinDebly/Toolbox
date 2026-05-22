##Metadata----------------------------------------------------------------------

# Author   : Augustin DEBLY
# Contact  : augustin.debly@gmail.com
# Github   : github.com/augustindebly

##Path--------------------------------------------------------------------------

SRFs_S2A    = "SRFs/S2A/S2A.csv"
SRFs_S2B    = "SRFs/S2B/S2B.csv"
SRF_G_M3M   = "SRFs/M3M/M3M_G.csv"
SRF_NIR_M3M = "SRFs/M3M/M3M_NIR.csv"
SRF_R_M3M   = "SRFs/M3M/M3M_R.csv"
SRF_RE_M3M  = "SRFs/M3M/M3M_RE.csv"

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
  m = matrix(rep(x_data_nm, each = length(x)), ncol = length(x_data_nm))
  ind = apply(abs(m-x), 1, which.min)
  return(ifelse(!((x<x_data_nm[1]) | (x>x_data_nm[length(x_data_nm)])),y_data[ind]/max(y_data),0))
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

#M3M
data_M3M_G   = read.csv(SRF_G_M3M,header = T,sep=",")
data_M3M_R   = read.csv(SRF_R_M3M,header = T,sep=",")
data_M3M_RE  = read.csv(SRF_RE_M3M,header = T,sep=",")
data_M3M_NIR = read.csv(SRF_NIR_M3M,header = T,sep=",")

SRF_M3M_G <- function(x){
  return(SRF_from_raw_data(x,data_M3M_G[[1]],data_M3M_G[[2]]))
}
SRF_M3M_R <- function(x){
  return(SRF_from_raw_data(x,data_M3M_R[[1]],data_M3M_R[[2]]))
}
SRF_M3M_RE <- function(x){
  return(SRF_from_raw_data(x,data_M3M_RE[[1]],data_M3M_RE[[2]]))
}
SRF_M3M_NIR <- function(x){
  return(SRF_from_raw_data(x,data_M3M_NIR[[1]],data_M3M_NIR[[2]]))
}
SRFs_M3M = list(SRF_M3M_G,SRF_M3M_R,SRF_M3M_RE,SRF_M3M_NIR)

##Export RData------------------------------------------------------------------

save.image(file = "SRFs.RData")
##Package-----------------------------------------------------------------------

library(xlsx)

##Path--------------------------------------------------------------------------

p_BathyElli = "INPUTS/BathyElliv2.1"
p_data      = "INPUTS/Data"
p_output    = "OUTPUTS/output.xlsx"

##Data--------------------------------------------------------------------------

BathyElli_ZH   = read.table(list.files(p_BathyElli, pattern = "ZH", full.names = TRUE), header = FALSE)
BathyElli_PBMA = read.table(list.files(p_BathyElli, pattern = "PBMA", full.names = TRUE), header = FALSE)
BathyElli_PHMA = read.table(list.files(p_BathyElli, pattern = "PHMA", full.names = TRUE), header = FALSE)
BathyElli_NM   = read.table(list.files(p_BathyElli, pattern = "NM", full.names = TRUE), header = FALSE)

data = data.frame()
for(p in list.files(p_data, pattern = ".csv", full.names = TRUE)){
  datai = read.csv(p)
  datai = datai[, c("Name", "Longitude", "Latitude", "Ellipsoidal.height", "Averaging.start")]
  data  = rbind(data,datai)
}

##Script------------------------------------------------------------------------

name = data$Name
lon  = data$Longitude
lat  = data$Latitude
h    = data$Ellipsoidal.height
date = as.POSIXct(substr(data$Averaging.start,1,19),format = "%Y-%m-%d %H:%M:%S")

list_ZH   = c()
list_PBMA = c()
list_PHMA = c()
list_NM   = c()

for(i in seq(length(name))){
  BathyElli_ZH_i   = which.min(sqrt((BathyElli_ZH[,1]-lon[i])^2+(BathyElli_ZH[,2]-lat[i])^2))
  href_ZH          = BathyElli_ZH[BathyElli_ZH_i,3]
  ZH               = h[i] - href_ZH
  list_ZH          = append(list_ZH, ZH)
  
  BathyElli_PBMA_i = which.min(sqrt((BathyElli_PBMA[,1]-lon[i])^2+(BathyElli_PBMA[,2]-lat[i])^2))
  href_PBMA        = BathyElli_PBMA[BathyElli_PBMA_i,3]
  PBMA             = h[i] - href_PBMA
  list_PBMA        = append(list_PBMA, PBMA)
  
  BathyElli_PHMA_i = which.min(sqrt((BathyElli_PHMA[,1]-lon[i])^2+(BathyElli_PHMA[,2]-lat[i])^2))
  href_PHMA        = BathyElli_PHMA[BathyElli_PHMA_i,3]
  PHMA             = h[i] - href_PHMA
  list_PHMA        = append(list_PHMA, PHMA)
  
  BathyElli_NM_i   = which.min(sqrt((BathyElli_NM[,1]-lon[i])^2+(BathyElli_NM[,2]-lat[i])^2))
  href_NM          = BathyElli_NM[BathyElli_NM_i,3]
  NM               = h[i] - href_NM
  list_NM          = append(list_NM, NM)
}

df = data.frame(name     = name,
                date_UTC = date,
                lon      = lon,
                lat      = lat,
                h        = h,
                ZH       = list_ZH,
                PBMA     = list_PBMA,
                PHMA     = list_PHMA,
                NM       = list_NM)

write.xlsx(df, p_output, row.names = FALSE)
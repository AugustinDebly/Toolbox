##Metadata----------------------------------------------------------------------

# Author   : Augustin DEBLY
# Contact  : augustin.debly@gmail.com
# Github   : github.com/augustindebly

##Functions---------------------------------------------------------------------

load("functions.RData")

##Script------------------------------------------------------------------------

name.dir        = "OUTPUTS"
path.output.txt = paste(name.dir,"profile.txt",sep = "/")

time.step       = 10*60      #[sec]
duration.acclim = 1*24*60*60 #[sec]
duration.exp    = 5*24*60*60 #[sec]

day.start       = 18
month.start     = 05
year.start      = 2026

hour.low.tide   = 12
min.low.tide    = 46

hour.sunrise    = 06
min.sunrise     = 24

hour.sunset     = 21
min.sunset      = 40

hour.start      = hour.low.tide - 3
min.start       = min.low.tide
sec.start       = 0

air.mini        = 12
air.maxi        = 22
wat.mini        = 16
wat.maxi        = 16

time            = time.vector(day.start   = day.start, 
                              month.start = month.start, 
                              year.start  = year.start,
                              hour.start  = hour.start,
                              min.start   = min.start,
                              sec.start   = sec.start,
                              duration    = duration.acclim + duration.exp,
                              time.step   = time.step)

time.low.tide   = time.vector(day.start   = day.start, 
                              month.start = month.start, 
                              year.start  = year.start,
                              hour.start  = hour.low.tide,
                              min.start   = min.low.tide,
                              sec.start   = 0,
                              duration    = 0,
                              time.step   = 0)

time.maxi.temp  = time.vector(day.start   = day.start, 
                              month.start = month.start, 
                              year.start  = year.start,
                              hour.start  = 12,
                              min.start   = 0,
                              sec.start   = 0,
                              duration    = 0,
                              time.step   = 0)

tempa            = cyclic.profile(shape     = "sinusoidal",
                                 time      = time,
                                 mini      = air.mini,
                                 maxi      = air.maxi,
                                 time.maxi = time.maxi.temp)

tempw            = cyclic.profile(shape     = "sinusoidal",
                                 time      = time,
                                 mini      = wat.mini,
                                 maxi      = wat.maxi,
                                 time.maxi = time.maxi.temp)

tide             = cyclic.profile(shape     = "step",
                                 period    = 12*60*60,
                                 time      = time,
                                 mini      = 0,
                                 maxi      = 1,
                                 time.mini = time.low.tide)

sunrise.vec      = as.POSIXct(paste(format(time,"%d/%m/%Y"), 
                                   hour.sunrise, 
                                   min.sunrise, sep = "/"), 
                             format = "%d/%m/%Y/%H/%M", tz="UTC")

sunset.vec       = as.POSIXct(paste(format(time,"%d/%m/%Y"), 
                                   hour.sunset, 
                                   min.sunset, sep = "/"), 
                             format = "%d/%m/%Y/%H/%M", tz="UTC")

light            = ifelse(time < sunrise.vec, 0, ifelse(time >= sunset.vec, 0, 100))
light[tide == 1] = 0

df               = data.frame(time, tempa, tempw, tide, light)

txt.generator(data  = df,
              time  = time,
              tempa = tempa,
              tempw = tempw,
              tide  = tide,
              light = light,
              path.output = path.output.txt)
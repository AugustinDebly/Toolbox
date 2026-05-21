##Metadata----------------------------------------------------------------------

# Author   : Augustin DEBLY
# Contact  : augustin.debly@gmail.com
# Github   : github.com/augustindebly

##Functions---------------------------------------------------------------------

time.vector <- function(day.start   = NULL,
                        month.start = NULL,
                        year.start  = NULL,
                        hour.start  = NULL,
                        min.start   = NULL,
                        sec.start   = NULL,
                        day.end     = NULL,
                        month.end   = NULL,
                        year.end    = NULL,
                        hour.end    = NULL,
                        min.end     = NULL,
                        sec.end     = NULL,
                        time.step   = NULL,
                        duration    = NULL) {
  
  if (is.null(day.start))   stop("'day.start' is missing.")
  if (is.null(month.start)) stop("'month.start' is missing.")
  if (is.null(year.start))  stop("'year.start' is missing.")
  if (is.null(hour.start))  stop("'hour.start' is missing.")
  if (is.null(min.start))   stop("'min.start' is missing.")
  if (is.null(sec.start))   stop("'sec.start' is missing.")
  if (is.null(time.step))   stop("'time.step' is missing.")
  
  if (is.null(duration) && (is.null(day.end) || is.null(month.end) || is.null(year.end) || is.null(hour.end) || is.null(sec.end))) {
    stop("Provide either 'duration', or 'day.end', 'month.end', 'year.end', 'hour.end', 'min.end' AND 'sec.end'.")
  }
  
  time.start = as.POSIXct(paste(day.start, month.start, year.start, hour.start, min.start, sec.start, sep = "/"), format = "%d/%m/%Y/%H/%M/%S", tz="UTC")
  
  if (is.null(day.end) || is.null(month.end) || is.null(year.end) || is.null(hour.end) || is.null(sec.end)) time.end = time.start + duration
  if (is.null(duration)) time.end = as.POSIXct(paste(day.end, month.end, year.end, hour.end, min.end, sec.end, sep = "/"), format = "%d/%m/%Y/%H/%M/%S", tz="UTC")
  
  output = seq(time.start, time.end, time.step)
  
  return(output)
}

cyclic.profile <- function(shape    = NULL,
                          period    = 24*60*60, #[sec]
                          time      = NULL,
                          mini      = NULL,
                          maxi      = NULL,
                          time.mini = NULL,
                          time.maxi = NULL) {
  
  if (is.null(shape)) stop("'shape' is missing.")
  if (is.null(time))  stop("'time' is missing.")
  if (is.null(mini))  stop("'mini' is missing.")
  if (is.null(maxi))  stop("'maxi' is missing.")
  
  if (is.null(time.mini) && is.null(time.maxi)) {
    stop("Provide either 'time.mini' or 'time.maxi'.")
  }
  
  time.step = as.numeric(diff(time), units = "secs")[[1]]
  period    = period / time.step
  
  if (shape == "sinusoidal" && !is.null(time.maxi)) {
    amplitude = (maxi - mini) / 2
    offset    = (maxi + mini) / 2
    output    = amplitude * sin(2 * pi * (seq(length(time)) - which.min(abs(time - time.maxi)) + period/4) / period) + offset
  }
  
  if (shape == "step") {
    phase  = ((seq(length(time)) - which.min(abs(time - time.mini)) + 0.75 * period) %% period) / period
    output = ifelse(phase < 0.5, maxi, mini)
  }
  
  return(output)
}

txt.generator <- function(data           = NULL,
                         time            = NULL,
                         temp            = NULL,
                         tempa           = NULL,
                         tempw           = NULL,
                         tide            = NULL,
                         light           = NULL,
                         path.output.txt = NULL,
                         path.output.img = NULL,
                         path.output.csv = NULL) {
  
  if (!is.null(data)) {
    if (is.null(time)  && "time"  %in% names(data)) time  = data$time
    if (is.null(tide)  && "tide"  %in% names(data)) tide  = data$tide
    if (is.null(light) && "light" %in% names(data)) light = data$light
    
    if (is.null(temp) && is.null(tempa) && is.null(tempw)) {
      if ("temp" %in% names(data)) {
        temp  = data$temp
      } else if ("tempa" %in% names(data) && "tempw" %in% names(data)) {
        tempa = data$tempa
        tempw = data$tempw
      }
    }
  }
  
  if (is.null(time))  stop("'time' is missing.")
  if (is.null(tide))  stop("'tide' is missing.")
  if (is.null(light)) stop("'light' is missing.")
  
  if (is.null(temp) && (is.null(tempa) || is.null(tempw))) {
    stop("Provide either 'temp', or both 'tempa' AND 'tempw'.")
  }
  
  if (is.null(temp)) temp = ifelse(tide == 1, tempw, tempa)
  
  temp_txt = as.integer(temp * 10)
  time_txt = as.numeric(difftime(time, as.POSIXct("1970-01-01", tz = "UTC", format = "%Y-%m-%d"), units = "secs"))
  
  output   = paste(time_txt, "-", temp_txt, tide, sprintf("%03d", light), rep(0, length(time)), sep = "")
  
  if (!is.null(path.output.txt)) {
    writeLines(output, con = path.output.txt)
  } else {
    return(output)
  }
  
  if (!is.null(path.output.img)) {
    packages   = c("ggplot2", "patchwork")
    for (pkg in packages) {
      if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
        install.packages(pkg)
        library(pkg, character.only = TRUE)
      }
    }
    plot.light = ggplot(data.frame(x = time, y = light), aes(x = x, y = y)) +
      geom_line() +
      labs(
        x = "Time",
        y = "Light"
      ) +
      theme_bw() +
      theme(
        axis.title.x = element_blank(),
        axis.text.x  = element_blank()
      )
    plot.tide = ggplot(data.frame(x = time, y = tide), aes(x = x, y = y)) +
      geom_line() +
      labs(
        x = "Time",
        y = "Tide"
      ) +
      theme_bw() +
      theme(
        axis.title.x = element_blank(),
        axis.text.x  = element_blank()
      )
    plot.temp = ggplot(data.frame(x = time, y = temp), aes(x = x, y = y)) +
      geom_line() +
      labs(
        x = "Time",
        y = "Temperature"
      ) +
      theme_bw()
    plot.patchwork = plot.light/plot.tide/plot.temp
    ggsave(plot.patchwork, file = path.output.img, height = 9, width = 9 * 4/3, units = "cm")
  }
  
  if (!is.null(path.output.csv)) {
    write.table(data, file = path.output.csv, row.names = F, sep = ";", dec = ".")
  }
}

##Export RData------------------------------------------------------------------

save(time.vector, cyclic.profile, txt.generator, file = "functions.RData")
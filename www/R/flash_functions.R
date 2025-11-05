# Flash functions

library(purrr)


# Return a sorted vector that is a combination of x and y, where if there is a
# similar value in both x and y the value of y is kept
add_time <- function(x, y, dif) {
  remove <- sapply(x, function(i) any(abs(i - y) < dif))
  sort(c(x[!remove], y))
}


# Calcualte the flash length and pause length of a glowtimes data.frame
get_glowtime <- function(x) {
  x$length <- x$end - x$start
  x <- x %>% mutate(last_end = lag(end))
  x$pause <- x$start - x$last_end
  x
}


# Group flashes in peak data frame if timedifference is less than breaktime
group_flashes <- function(data, breaktime) {
  data %>%
    mutate(samegroup = ifelse(timediff < breaktime, 0, 1)) %>%
    mutate(samegroup = replace(samegroup, is.na(samegroup), 1)) %>%
    mutate(grouping = cumsum(samegroup))
}


# Create a glowtimes dataframe from just a vector of times, where it is assumed
# every other time is a start/end
glowtimes_from_vec <- function(times) {
  gstarts <- times[c(TRUE, FALSE)]
  gends <- times[c(FALSE, TRUE)]
  if (length(gends) < length(gstarts)) {
    gends <-  c(gends, NA)
  }
  glowtimes <- data.frame(grouping = 1:ceiling(length(times) / 2),
                          start = gstarts,
                          end = gends)
  get_glowtime(glowtimes)
}

# Add flashes to an already calculated flash time
add_flash <- function(data, add, dif = NULL) {

  add <- add[add >= data$start & add <= data$end]

  dif <- ifelse(is.null(dif), (data$end - data$start) / 250, dif)

  if (data$flashtype %in% c("complex flash", "single flash")) {
    peak_time <- add_time(data$peak$peak_time, add, dif)

    group <- seq_along(peak_time)
    data$peak <- data.frame(grouping = group, peak_time = peak_time)
    data$peak$timediff <- c(0, diff(data$peak$peak_time))
  }

  if (data$flashtype == "complex flash") {
    data$peak <- group_flashes(data$peak, data$breaktime)
  }

  if (data$flashtype == "glow") {
    s_and_e_times <- c(data$glowtimes$start, data$glowtimes$end)

    s_and_e_times <- add_time(s_and_e_times, add, dif)

    data$glowtimes <- glowtimes_from_vec(s_and_e_times)
  }
  data
}

# Remove flashes to an already calculated flash time
rm_flash <- function(data, rm, dif = NULL) {
  dif <- ifelse(is.null(dif), (data$end - data$start) / 250, dif)
  peak_time <- data$peak$peak_time

  if (data$flashtype %in% c("single flash", "complex flash")) {
    for (i in rm) {
      peak_time <- peak_time[abs(peak_time - i) > dif]
    }

    group <- seq_along(peak_time)
    data$peak <- data.frame(grouping = group, peak_time = peak_time)
    data$peak$timediff <- c(0, diff(data$peak$peak_time))
  }

  if (data$flashtype == "complex flash") {
    data$peak <- group_flashes(data$peak, data$breaktime)
  }

  if (data$flashtype == "glow") {
    s_and_e_times <- sort(c(data$glowtimes$start, data$glowtimes$end))

    remove <- sapply(s_and_e_times, function(x) any(abs(x - rm) < dif))
    s_and_e_times <- s_and_e_times[!remove]

    data$glowtimes <- glowtimes_from_vec(s_and_e_times)
  }
  data
}


# Create a plot from time_array and amp datasets
flash_plot <- function(time_array,
                       amp,
                       ymin = NULL,
                       ymax = NULL, 
                       xlims = NULL) {

  if (is.null(xlims)) xlims <- c(time_array[1], time_array[length(time_array)])
  
  if (!is.null(ymin)) {
    yrange <- ymax - ymin
    ylim_adj <- c(ymin - 0.02 * yrange, ymax + 0.02 * yrange)
  } else {
    ylim_adj <- c(min(amp, na.rm = TRUE), max(amp, na.rm = TRUE))
  }
  
  par(mar = c(3, 3, 1, 0), mgp = c(1.5, 0.5, 0))
  plot(x = time_array,
       y = amp,
       type = "l",
       col = "black",
       xlab = "Seconds",
       ylab = "Amplitude",
       xlim = xlims,
       ylim = ylim_adj)
}

mysegments <- function(peak, ymax, ymin, mycol) {
  yrange <- ymax - ymin
  expand <- 0.05 * yrange
  ymax <- ymax + expand
  ymin <- ymin - expand
  segments(x0 = peak, x1 = peak, y0 = ymax, y1 = ymin, col = mycol)
}


# Select the amplitudes from the left channel between the start and end
get_amps <- function(wav, start, end) {
  starting <- (start + 0.01) * wav@samp.rate
  ending <- end * wav@samp.rate
  wav@left[starting:ending]
}

create_timeamp <- function(amp, time_array, quant, flashtype) {
  # Create dataframe of time and amplitude
  timeamp <- data.frame(Time = time_array, Amp = amp)

  # keep only amplitudes above the specified quantile
  if (quant == 1) {
    timeamp <- timeamp[1, ]
  } else {
    timeamp <- timeamp[abs(timeamp$Amp) > quantile(timeamp$Amp, quant), ]
  }

  # get time difference between remaining high level amplitudes
  timeamp$timediff <- c(0, diff(timeamp$Time))

  timer <- ifelse(flashtype == "glow", 0.3, 0.05)

  # create groups based on the difference in time.
  timeamp <- group_flashes(timeamp, timer)
}


# Get the median time of each group and find the difference between groups
create_peak <- function(timeamp) {
  peak <- timeamp %>% group_by(grouping) %>% summarise(peak_time = median(Time))
  peak$timediff <- c(0, diff(peak$peak_time))
  peak
}


# create functions for analyzing flash patterns
flashcalc <- function(wav,
                      start = 0,
                      end = length(wav@left) / wav@samp.rate,
                      quant = 0.998,
                      pause = 1,
                      flashtype) {
  max_time <- length(wav@left) / wav@samp.rate
  end <- ifelse(end > max_time, max_time, end)
  
  # Get only the amplitude values between the starting and ending times
  amp <- get_amps(wav, start, end)

  # Create time array
  time_array <- ((0:(length(amp) - 1)) / wav@samp.rate) + start

  # Create dataframe of time and amplitude
  timeamp <- create_timeamp(amp, time_array, quant, flashtype)
  
  if (flashtype %in% c("single flash", "complex flash") && quant != 1) {
    # Find median time of each sound grouping (peak) and the difference them
    peak <- create_peak(timeamp)
  } else {
    peak <- NULL
  }

  if (flashtype == "complex flash" && quant != 1) {
    # create complex flash groupings
    peak <- group_flashes(peak, pause)
  }

  if (flashtype == "glow" && quant != 1) {
    # get glow start times
    glowstart <- timeamp %>% group_by(grouping) %>% summarise(start = min(Time))

    # get glow end times
    glowend <- timeamp %>% group_by(grouping) %>% summarise(end = max(Time))

    glowtimes <- data.frame(grouping = seq_along(glowstart$start),
                            start = glowstart$start,
                            end = glowend$end)

    glowtimes <- get_glowtime(glowtimes)
  } else {
    glowtimes <- NULL
  }

  list(time_array = time_array,
       amp = amp,
       peak = peak,
       ampmax = max(amp),
       ampmin = min(amp),
       breaktime = pause,
       glowtimes = glowtimes,
       flashtype = flashtype,
       start = start,
       end = end)
}

flashcalc_df <- function(peak) {
  if (peak$flashtype %in% c("single flash", "complex flash") &&
      is.null(peak$peak)) {
    peak$peak <- data.frame(timediff = NA, grouping = NA)
  }
  
  if (peak$flashtype == "glow" && is.null(peak$glowtime)) {
    peak$glowtimes <- data.frame(pause = NA, length = NA)
  }
  
  if (peak$flashtype == "single flash") {
    sd <- sd(peak$peak$timediff[2:length(peak$peak$timediff)])
    data1 <- data.frame("Mean" = mean(peak$peak$timediff[-1]),
                        "Max" = max(peak$peak$timediff[-1]),
                        "Min" = min(peak$peak$timediff[-1]),
                        "Std. Dev" = sd,
                        "Intervals Measured" = (nrow(peak$peak) - 1),
                        check.names = FALSE)
  }

  if (peak$flashtype == "complex flash") {
    # create df of just the interflash timings
    interflash <- peak$peak[peak$peak$timediff < peak$breaktime, ]

    # create df of just the between timings
    betweengroup <- peak$peak[peak$peak$timediff > peak$breaktime, ]

    # create df of number of flashes per group
    num_flashes <- peak$peak %>% group_by(grouping) %>% summarise(n = n())

    data1 <- data.frame("Mean" = mean(interflash$timediff[-1]),
                        "Max" = max(interflash$timediff[-1]),
                        "Min" = min(interflash$timediff[-1]),
                        "Std. Dev" = sd(interflash$timediff[-1]),
                        "Intervals Measured" = nrow(interflash) - 1,
                        "Flashes per Group" = mean(num_flashes$n),
                        "Mean" = mean(betweengroup$timediff),
                        "Max" = max(betweengroup$timediff),
                        "Min" = min(betweengroup$timediff),
                        "Std. Dev" = sd(betweengroup$timediff),
                        "Pauses Measured" = nrow(betweengroup),
                        check.names = FALSE)
  }

  if (peak$flashtype == "glow") {
    data1 <- data.frame("Mean" = mean(peak$glowtimes$length),
                        "Max" = max(peak$glowtimes$length),
                        "Min" = min(peak$glowtimes$length),
                        "Std. Dev" = sd(peak$glowtimes$length),
                        "Flashes Measured" = nrow(peak$glowtimes),
                        "Mean" = mean(peak$glowtimes$pause[-1]),
                        "Max" = max(peak$glowtimes$pause[-1]),
                        "Min" = min(peak$glowtimes$pause[-1]),
                        "Std. Dev" = sd(peak$glowtimes$pause[-1]),
                        "Pauses Measured" = nrow(peak$glowtimes) - 1,
                        check.names = FALSE)
  }
  data1
}

flashcalc_plot <- function(data, xlims = NULL) {
  flash_plot(data$time_array, data$amp, data$ampmin, data$ampmax, xlims)
  if (data$flashtype == "complex flash") {
    flashtime <- data$peak$peak_time
    mycols <- rep(c("blue", "red"),
                  length(unique(data$peak$grouping + 1)
                         / 2))[data$peak$grouping + 1]
  } else if (data$flashtype == "single flash") {
    flashtime <- data$peak$peak_time
    mycols <- "red"
  } else {
    flashtime <- c(data$glowtimes$start, data$glowtimes$end)
    mycols <- rep(c("green", "red"), each = length(flashtime) / 2)
  }
  if (!is.null(flashtime)) {
    mysegments(flashtime, data$ampmax, data$ampmin, mycols)
  }
}


### Functions for formatting the datatable output ------------------------------

# Column headers for the different types of flash patterns
mycols <- c("Mean", "Max", "Min", "Std. Dev")
single_cols <- c(mycols, "Intervals Measured")
cmplx_cols <- c(single_cols, "Flashes per Group", mycols, "Pauses Measured")
glow_cols <- c(mycols, "Flashes Measured", mycols, "Pauses Measured")

# Group headers
cmplx_h <- c("Within Group Flash Interval", "", "Dark Period Between Groups")
glow_h <- c("Glow Duration", "", "Dark Period")

# Group header styles (needed to separate groups more easily in cmplx and glow)
mystyle <- "text-align: center;"
cmplx_style <- paste0(mystyle, c("", " border-bottom: none;", ""))

# Helper to generate a <thead> block
make_header <- function(colspans, labels, columns, flash_style) {
  htmltools::withTags(table(
    class = "cell-border compact",
    thead(
      tr(
        mapply(function(span, label, flash_style) {
          th(colspan = span, style = flash_style, label)
        },
        colspans,
        labels,
        flash_style,
        SIMPLIFY = FALSE)
      ),
      tr(lapply(columns, th))
    )
  ))
}

# Predefined header sketches
single_flash_dt  <- make_header(c(4), c("Flash Interval"), single_cols, mystyle)
complex_flash_dt <- make_header(c(4, 2, 4), cmplx_h, cmplx_cols, cmplx_style)
glow_flash_dt    <- make_header(c(4, 1, 4), glow_h, glow_cols, cmplx_style)


# Function to create a datatable from a dataframe based on flash type
mydt <- function(x, flashtype) {
  sketch <- switch(flashtype,
                   "single flash" = single_flash_dt,
                   "complex flash" = complex_flash_dt,
                   glow_flash_dt)
  datatable(round(x, 3),
            container = sketch,
            rownames = FALSE,
            options = list(searching = FALSE,
                           paging = FALSE,
                           ordering = FALSE,
                           info = FALSE))
}

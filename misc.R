# Misc Tools
# By xjtang
#----------------------------------------------------------------

# print progress
showProgress <- function(i, n, step) {
  percent <- floor(i / n * 100)
  if ((((i - 1) / n * 100) %/% step) < ((i / n * 100) %/% step)) {
    cat(percent, '%\n')
  }
  return(percent)
}

#--------------------------------------

# calculate day of year from date
dateToDOY <- function(year, month, day, dayOnly=F) {

  # check year
  if (year < 0) {
    cat('check the year\n')
    return(0)
  }
  
  # 365 days or 366 days in that year
  if ((year %% 4 == 0 & year %% 100 != 0) | year %% 400 == 0) {
    feb <- 29
  } else {
    feb <- 28
  }
  dom <- c(31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  # check month
  if (month > 12 | month < 1) {
    cat('check the month\n')
    return(0)
  }
  
  # check day
  if (day < 1 | day > dom[month]) {
    cat('check the day\n')
    return(0)
  }
  
  # calculate doy
  if (dayOnly) {year = 0}
  if (month == 1) {
    doy <- year * 1000 + day
  } else {
    doy <- year * 1000 + sum(dom[1:(month - 1)]) + day
  }
  
  # done
  return(doy)
}

#--------------------------------------

# calculate the date from day of year
doyToDate <- function(doy) {
  
  # get year
  year <- floor(doy / 1000)
  
  # check the doy
  if (year < 0) {
    cat('invalid doy\n')
    return(0)
  }
  
  # 365 days or 366 days in that year
  if ((year %% 4 == 0 & year %% 100 != 0) | year %% 400 == 0) {
    feb <- 29
  } else {
    feb <- 28
  }
  dom <- c(31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  # get total days
  tday <- doy - year * 1000
  
  # check total days
  if (tday <= 0 | tday > sum(dom)) {
    cat('invalid doy\n')
    return(0)
  }
  
  # calculate month and day
  for (i in 1:12) {
    tday <- tday-dom[i]
    if(tday <= 0) {
      month <- i
      day <- tday+dom[i]
      break
    }
  }
  
  # done
  date <- c(year, month, day)
  return(date)
}

#--------------------------------------

# fraction year to date
fracToDate <- function(fyear) {
  year <- floor(fyear)
  frac <- fyear - year
  doy <- round(frac * 365.25)
  return(as.Date(paste0(year, "-01-01")) + doy)
}

#--------------------------------------

# date difference
dateDiff <- function(t1, t2) {
  return(as.numeric(t2 - t1))
}
  
#--------------------------------------
# End
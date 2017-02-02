#' @title data2d class
#'
#' @description
#' Class that provides standard methods for working with 2d data item from CCHIC
#' Receives a reference to the full data set and then extracts and manipulates the field of interest.
#' @import data.table


#' @exportClass data2d
#' @export data2d
data2d <- setRefClass("data2d",
  # Fields
  fields = list(
    # input fields
    rdt="data.table", # original, do not modify
    name="character",
    # derived fields
    dt="data.table",
    dt.cc="data.table" # complete cases
    # summ="list",
    # period="list",
    # summ_t24="list"
    ),

  # Methods
  methods =list(
    initialize = function(rdt, name) { # execute when initialised.
      # a .self method is needed as the first command
      .self$name <- name  # name of the field
      .self$rdt <- rdt

      # Extract the 2d data for manipulation and working
      .self$dt <- rdt[,.(
        unit=site_id,
        admission=episode_id,
        time=time,
        val=get(name))]

      .self$dt.cc <- na.omit(dt)
      # .self$summ <- gen_summ(dt$val)
      # .self$summ_t24 <- summ_t(dt)
      # .self$period <- gen_summ(gen_periods(dt$val))
      # print(paste("Inspecting", name))
      # print(t(unlist(summ)))
  },

  # Private function to summarise data
  tab = function(d=dt) {
    # table(val, useNA="always")
    d <- data.table::copy(d)
    d[, val := as.character(val)]
    d[,.(n=.N, pct=round(100*.N/nrow(d))),by=val][order(val)]
  },

  tab_sites = function(d=dt) {
      with(dt, table(unit,val, useNA="always"))
  }

  # ,
  # # Produce table 1 like row for any categorical data
  # cats = function(d=dt) {
  #   d <- data.table::copy(ppv$dt)
  #   d[, val := as.character(val)]
  #   d[,.(n=.N, pct=round(100*.N/nrow(d))),by=val][order(val)]
  # }

  ,
  # Wide table of categories by site
  cat_data_sites = function(d=dt) {
      d <- d[,.(.N),by=.(unit,val)]
      d <- d[, unit.total := sum(N), by=unit][, pct := round(100*N/unit.total,0)][, unit.total := NULL]
      d <- melt(d, id.vars=c("unit", "val"))
      d <- dcast.data.table(d, unit ~ variable + val)
      return(d)
  }

  ,
  # Mosaic plot of categories by site
  mosaic_data_sites = function(d=dt, mainlab=name) {
    shadesN <- uniqueN(d$val)
    shadesN <- 1:shadesN/shadesN
    tt <- table(d$unit, d$val)
    mosaicplot(tt, main=mainlab, xlab="Unit", ylab="Category", col=gray(shadesN), las=1)
  }

  ,
  # Summarise by site
  summ_sites = function(d=dt, gen_summ=.self$gen_summ, t1=NULL, t0=NULL) {
    if (is.null(t0)) t0 <- min(d$time, na.rm=TRUE)
    if (is.null(t1)) t1 <- max(d$time, na.rm=TRUE)
    d <- d[time>=t0 & time<t1]
    x <- with(d, tapply(val, unit, gen_summ))
    # Now convert to data.frame without losing row and col names
    # see http://stackoverflow.com/a/4227504/992999
    # data.frame(t(sapply(x,c)))
    plyr::ldply(x, data.frame)
  },

  # Summarise periods by site
  period_sites = function(d=dt, gen_summ=.self$gen_summ, gen_periods=.self$gen_periods) {
    foo <- function(y) {
      gen_summ(gen_periods(y))
    }
    x <- with(d, tapply(val, unit, foo))
    # Now convert to data.frame without losing row and col names
    # see http://stackoverflow.com/a/4227504/992999
    # data.frame(t(sapply(x,c)))
    plyr::ldply(x, data.frame)
  }

  ,
  # Density plots of measurement period by site
  density_period_sites = function(d=dt, xmax=24, gen_periods=.self$gen_periods, mainlab=name) {
    library(ggplot2)
    d <- d[,.(val=gen_periods(val)),by=unit]
    ggplot(data=d, aes(x=val,fill=unit)) +
      labs(title=mainlab) +
      geom_density(alpha=0.5) +
      coord_cartesian(x=c(0,xmax)) +
      theme_minimal()
  }

  ,
  summ = function(v=dt$val) {
    gen_summ(v)
  }

  ,
  # Summarise 1st 24 hour variables (or by time)
  summ_t= function(d=dt, t1=24, t0=0) {
    # - [ ] NOTE(2017-01-05): problem with comparisons in i in data.table
    # use subset function instead
    val <- subset(d, time >= t0 & time<t1 )$val
    # val <- x$val
    gen_summ(val)

  },

  # Private function to summarise data
  gen_summ = function(val=dt$val) {
    val.cc <- na.omit(val)
    suppressWarnings(
      ll <- list(
        min=min(val.cc),
        q1=as.numeric(quantile(val.cc, 0.25)),
        mean=mean(val.cc),
        median=median(val.cc),
        q3=as.numeric(quantile(val.cc, 0.75)),
        max=max(val.cc),
        length=length(val),
        missing=sum(is.na(val))
        )
      )
    ll$missing.p <- round(ll$missing / ll$length * 100)
    return(ll)
  }

  ,
  # Mosaic plot of missingness by site
  missing_data_sites = function(d=dt, mainlab=name, t0=NULL, t1=NULL) {
    if (is.null(t0)) t0 <- min(d$time, na.rm=TRUE)
    if (is.null(t1)) t1 <- max(d$time, na.rm=TRUE) + 1
    d <- d[time>=t0 & time<t1]
    d[,val:=is.na(val)]
    shadesN <- c(0,1)
    tt <- table(d$unit, d$val)
    mosaicplot(tt, main=mainlab, xlab="Unit", ylab="Missing", col=gray(shadesN))
  }

  ,
  # Density plots by site
  density_data_sites = function(d=dt, mainlab=name, t0=NULL, t1=NULL) {
    library(ggplot2)
    if (is.null(t0)) t0 <- min(d$time, na.rm=TRUE)
    if (is.null(t1)) t1 <- max(d$time, na.rm=TRUE)
    d <- d[time>=t0 & time<t1]
    ggplot(data=d, aes(x=val,fill=unit)) +
      labs(title=mainlab) +
      geom_density(alpha=0.5) +
      theme_minimal()
  }

  ,
  # Private function to return periods between measures in vector
  gen_periods = function(x) {
    # Get indexes where data is not missing
    x.i <- seq(length(x))[!is.na(x)]
    # Count either leads i.e. interval before each measure
    # Shift by 1, add a leading 1, drop last
    # Then subtract for differences
    periods <- x.i - c(1,x.i[-length(x.i)])
    return(periods)
  }


))

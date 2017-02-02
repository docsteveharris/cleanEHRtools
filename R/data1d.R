#' @title data1d class
#'
#' @description
#' Class that provides standard methods for working with 1d data item from CCHIC.
#' Receives a reference to the full data set and then extracts and manipulates the field of interest.
#' @import data.table

#' @exportClass data1d
#' @export data1d
data1d <- setRefClass("data1d",
  # Fields
  fields = list(
    rdt="data.table", # original data; do not modify
    dt="data.table",
    name="character",
    dt.cc="data.table" # complete cases
    # tab="table",
    # summ="list"
    ),

  # Methods
  methods =list(
    initialize = function(rdt, name) {
      # execute when initialised.
      library(data.table)
      # a .self method is needed as the first command
      .self$name <- name  # name of the field
      .self$rdt <- rdt

      # Extract the 1d data for manipulation and working
      .self$dt <- unique(rdt[,.(
        unit=site_id,
        admission=episode_id,
        val=get(name))])

      .self$dt.cc <- na.omit(dt)

      # print(paste("Most frequent values for", name))
      # tt <- table(dt$val)
      # print(tt[order(tt, decreasing=T)])
  },


  # Private function to summarise data
  tab = function(val=dt$val) {
    table(val, useNA="always")
  }

  ,
  tab_sites = function(d=dt) {
      with(dt, table(unit,val, useNA="always"))
  }

  ,
  # Produce table 1 like row for any categorical data
  cats = function(d=dt) {
    lvls <- c(NA, unique(d$val))
    d <- dcast.data.table(d, unit + admission ~ val)
    d <- d[, lapply(.SD, function(x) !is.na(x)), by=.(unit, admission)]
    l <- lapply(lvls, function(x) list(level=x, n=sum(d[[x]]), pct=round(100*mean(d[[x]]))))
    return(rbindlist(l))
  }

  ,
  # Summarise
  summ = function(val=dt$val) {
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
    # print(t(unlist(ll)))
    return(ll)
    },

  # Summarise by site
  # Need to internally pass the function else not visible
  summ_sites = function(d=dt, summ=.self$summ) {
    x <- with(d, tapply(val, unit, summ))
    # Now convert to data.frame without losing row and col   names
    # see http://stackoverflow.com/a/4227504/992999
    # data.frame(t(sapply(x,c)))
    plyr::ldply(x, data.frame)
  }

  ,
  # Wide table of categories by site
  cat_data_sites = function(d=dt) {
      d <- d[,.(.N),by=.(unit,val)]
      d <- d[, unit.total := sum(N), by=unit][, pct := round(100*N/unit.total,0)][, unit.total := NULL]
      suppressWarnings(d <- melt(d, id.vars=c("unit", "val")))
      d <- dcast.data.table(d, unit ~ variable + val)
      return(d)
  }

  ,
  # Mosaic plot of missingness by site
  missing_data_sites = function(d=dt, mainlab=name) {
    dd <- data.table::copy(d) # since passed as reference
    dd[,val:=is.na(val)]
    if (uniqueN(dd$val > 1)) {
      shadesN <- c(0,1)
      tt <- table(dd$unit, dd$val)
      mosaicplot(tt, main=mainlab, xlab="Unit", ylab="Missing", col=gray(shadesN))
    } else {
      print(paste("*** No missingness found for", mainlab))
    }
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
  # Density plots by site
  density_data_sites = function(d=dt, mainlab=name) {
    library(ggplot2)
    ggplot(data=d, aes(x=val,fill=unit)) +
      labs(title=mainlab) +
      geom_density(alpha=0.5) +
      theme_minimal()
  }
  )
)

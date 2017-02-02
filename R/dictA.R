#' @title dictA
#'
#' @description
#' Prepare analysis dictionary
#' @import data.table


#' @export

dictA <- function(dict.analysis = "config/ANALYSIS_REF.yaml") {

  require(purrr)
  require(yaml)
  # Load then merge in analysis.dict
  analysis.dict <- yaml::yaml.load_file(dict.analysis)
  ccdata.dict <- yaml::yaml.load_file(
    system.file("conf/ITEM_REF.yaml", package = "ccdata")
    )

  # Extract those sub-items needed from ccdata.dict
  dict <- ccdata.dict %>% map(function(x) list(
    shortName = x$shortName,
    dataItem = x$dataItem,
    NHICcode = x$NHICcode,
    NHICdtCode = x$NHICdtCode,
    NHICmetaCode = x$NHICmetaCode,
    Datatype = x$Datatype,
    Classification1 = x$Classification1,
    Classification2 = x$Classification2,
    Classification3 = x$Classification3
    ))

    # dict[["NIHR_HIC_ICU_0444"]]

    # update dict with with fields from ANALYSIS_REF.yaml
    dict <- dict %>%
    map(function(x) update_list(x,
      shortName      =analysis.dict[[x$NHICcode]]$shortName,
      distribution   =analysis.dict[[x$NHICcode]]$distribution,
      decimal_places =analysis.dict[[x$NHICcode]]$decimal_places,
      levels         =analysis.dict[[x$NHICcode]]$category$levels,
      sepsis3_abx    =analysis.dict[[x$NHICcode]]$sepsis3_abx)
      )

      # dict[["NIHR_HIC_ICU_0444"]]
      # dict[["NIHR_HIC_ICU_0002"]]

      # Load the derived fields
      derived.dict <- yaml::yaml.load_file("config/DERIVED_REF.yaml")
      derived.dict
      # - [ ] NOTE(2016-07-18): make NHICcode the shortName if missing NHICcode
      #       relevant only for derived
      derived.dict <- map_if(
        derived.dict,
        function(x) {is.null(x[["NHICcode"]])},
        ~ update_list(., NHICcode = ~ shortName))
        derived.dict
        dict <- c(dict, derived.dict)


        # - [ ] NOTE(2016-07-18): prepare dimension data
        dict <- map_if(
          dict,
          function(x) {is.null(x[["dimension"]]) & !is.null(x[["NHICdtCode"]])},
          ~ update_list(., dimension = ~ "2d"))
          dict <- map_if(
            dict,
            function(x) {is.null(x[["dimension"]]) & is.null(x[["NHICdtCode"]])},
            ~ update_list(., dimension = ~ "1d"))

}


#' Create a data.frame with Historical and Projection years
#'
#' @param x An object of class `Hist`, `MSE`, `multiHist`, or `MMSE`
#'
#' @return A data.frame with years and period (Historical or Projection)
#' @export
#'
#' @examples
get_years <- function(x) {
  UseMethod("get_years")
}


#' @export
#' @rdname get_years
get_years.MSE <- function(x) {
  MSE <- x
  hist.yrs <- (MSE@OM$CurrentYr[1] - MSE@nyears + 1):MSE@OM$CurrentYr[1]
  proj.yrs <- (MSE@OM$CurrentYr[1]+1):(MSE@OM$CurrentYr[1]+MSE@proyears)
  data.frame(Year=c(hist.yrs, proj.yrs), Period=c(rep('Historical', MSE@nyears),
                                                  rep('Projection', MSE@proyears)))
}

#' @export
#' @rdname get_years
get_years.Hist <- function(x) {
  Hist <- x
  hist.yrs <- (Hist@OM@CurrentYr -  Hist@OM@nyears + 1):Hist@OM@CurrentYr
  proj.yrs <- (Hist@OM@CurrentYr[1]+1):(Hist@OM@CurrentYr[1]+Hist@OM@proyears)
  data.frame(Year=c(hist.yrs, proj.yrs), Period=c(rep('Historical', Hist@OM@nyears),
                                                  rep('Projection', Hist@OM@proyears)))
}


get_ts_hist <- function(x, name='Name', slot='SBiomass') {
  years <- get_years(x) %>% filter(Period=='Historical')
  dd <- dim(x@TSdata[[slot]])
  nsim <- dd[1]
  data.frame(Year=rep(years$Year, each=nsim),
             Sim=1:nsim,
             Value=as.vector(apply(x@TSdata[[slot]], 1:2, sum)),
             Period=years$Period,
             Name=name,
             Variable=slot)
}

get_ts_proj <- function(x, name='Name', slot='SBiomass') {
  years <- get_years(x) %>% filter(Period=='Projection')
  nyears <- length(years)
  nsim <- x@nsim
  nMPs <- x@nMPs
  MPs <- x@MPs
  data.frame(Year=rep(years$Year, each=nsim*nMPs),
             Sim=1:nsim,
             MP=rep(MPs,each=nsim),
             Value=as.vector(slot(x, slot)),
             Period=years$Period,
             Name=name,
             Variable=slot)
}


#' Title
#'
#' @param x
#' @param name
#'
#' @return
#' @export
#'
#' @examples
get_SSB <- function(x, name='Name') {
  UseMethod("get_SSB")
}

#' @export
#' @rdname get_SSB
get_SSB.Hist <- function(x, name='Name') {
  get_ts_hist(x, name=name, slot='SBiomass')
}

#' @export
#' @rdname get_SSB
get_SSB.MSE <- function(x, name='Name') {
  get_ts_proj(x, name=name, slot='SSB')
}

#' @export
#' @rdname get_SSB
get_SSB.list <- function(x, name='Name') {
  x <- check_names(x)
  purrr::map2_df(x, names(x), get_SSB)

}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_Biomass <- function(x, ...) {
  UseMethod("get_Biomass")
}

#' @export
#' @rdname get_Biomass
get_Biomass.Hist <- function(x, name='Name') {
  get_ts_hist(x, name=name, slot='Biomass')
}

#' @export
#' @rdname get_Biomass
get_Biomass.MSE <- function(x, name='Name') {
  get_ts_proj(x, name=name, slot='B')
}

#' @export
#' @rdname get_Biomass
get_Biomass.list <- function(x, name='Name') {
  x <- check_names(x)
  purrr::map2_df(x, names(x), get_Biomass)
}


#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_Landings <- function(x, ...) {
  UseMethod("get_Landings")
}

#' @export
#' @rdname get_Landings
get_Landings.Hist <- function(x, name='Name') {
  get_ts_hist(x, name=name, slot='Landings')
}

#' @export
#' @rdname get_Landings
get_Landings.MSE <- function(x, name='Name') {
  get_ts_proj(x, name=name, slot='Catch')

}

#' @export
#' @rdname get_Landings
get_Landings.list <- function(x, name='Name') {
  x <- check_names(x)
  purrr::map2_df(x, names(x), get_Landings)
}

#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
get_Removals <- function(x, ...) {
  UseMethod("get_Removals")
}

#' @export
#' @rdname get_Removals
get_Removals.Hist <- function(x, name='Name') {
  get_ts_hist(x, name=name, slot='Removals')
}

#' @export
#' @rdname get_Removals
get_Removals.MSE <- function(x, name='Name') {
  get_ts_proj(x, name=name, slot='Removals')

}

#' @export
#' @rdname get_Removals
get_Removals.list <- function(x, name='Name') {
  x <- check_names(x)
  purrr::map2_df(x, names(x), get_Removals)
}

check_names <- function(x) {
  if (is.null(names(x))) {
    message("List elements are not named. Using numeric values")
    names(x) <- 1:length(x)
  }
  x
}


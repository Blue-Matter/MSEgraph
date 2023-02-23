
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


#' Title
#'
#' @param x
#'
#' @param variable
#' @param model
#'
#' @export
get_ts <- function(x, variable='Spawning Biomass', model='Model 1') {
  UseMethod("get_ts")
}

#' @export
#' @rdname get_ts
get_ts.Hist <- function(x, variable='Spawning Biomass', model='Model 1') {
  slot <- match_ts_variable(variable, 'Hist')
  if (grepl('\\()', slot)) {
    fn <- gsub('\\()','', slot)
    value <- get(fn)(x)
  } else {
    value <- as.vector(apply(x@TSdata[[slot]], 1:2, sum))
  }

  years <- get_years(x) %>% filter(Period=='Historical')
  dd <- dim(x@TSdata$Number)
  nsim <- dd[1]
  data.frame(Year=rep(years$Year, each=nsim),
             Sim=1:nsim,
             Value=value,
             Variable=variable,
             Period=years$Period,
             Model=model)
}

hist.recruits <- function(x) {
  as.vector(apply(x@AtAge$Number[,1,,], 1:2, sum))
}


#' @rdname get_ts
#' @export
valid_ts_variables <- function() {
  unique(TS_Variables$Variable)
}



#' @param class
#' @export
#' @rdname get_ts
match_ts_variable <- function(variable='Spawning Biomass', class='Hist') {
  if (!variable %in%  TS_Variables$Variable)
    stop('Not a valid time-series variable. See `valid_ts_variables()`')
  out <- TS_Variables %>% filter(Variable %in% variable, Class==class)
  out$Slot
}


add_MPs <- function(hist_df, MPs) {
  nMPs <- length(MPs)
  hist_list <- replicate(nMPs, hist_df, FALSE)
  for (i in 1:nMPs) {
    hist_list[[i]]$MP=MPs[i]
  }
  do.call('rbind', hist_list)
}

#' @export
#' @rdname get_ts
get_ts.MSE <- function(x, variable='Spawning Biomass', model='Model 1') {
  slot <- match_ts_variable(variable, 'MSE')
  nsim <- x@nsim
  nMPs <- x@nMPs
  MPs <- x@MPs

  hist_df <- get_ts(x@Hist, variable=variable, model=model) %>%
    add_MPs(., MPs)

  proj.years <- get_years(x) %>% filter(Period=='Projection')
  pyears <- length(proj.years)

  proj_df <- data.frame(Year=rep(proj.years$Year, each=nsim*nMPs),
             Sim=1:nsim,
             MP=rep(MPs,each=nsim),
             Value=as.vector(slot(x, slot)),
             Period=proj.years$Period,
             Model=model,
             Variable=variable)

  bind_rows(hist_df, proj_df)

}

#' @export
#' @rdname get_ts
get_ts.list <- function(x, variable='Spawning Biomass', model='Model 1') {
  x <- check_names(x)
  purrr::map2(x, names(x), get_ts, variable=variable) %>%
    purrr::list_rbind()
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
get_SSB <- function(x, model='Model 1') {
  get_ts(x, variable='Spawning Biomass', model=model)
}

#' @export
#' @rdname get_SSB
get_Biomass <- function(x, model='Model 1') {
  get_ts(x, variable='Biomass', model=model)
}

#' @export
#' @rdname get_SSB
get_Landings <- function(x, model='Model 1') {
  get_ts(x, variable='Landings', model=model)
}

#' @export
#' @rdname get_SSB
get_Removals <- function(x, model='Model 1') {
  get_ts(x, variable='Removals', model=model)
}

#' @export
#' @rdname get_SSB
get_Recruits <- function(x, model='Model 1') {
  get_ts(x, variable='Recruits', model=model)
}

check_names <- function(x) {
  if (is.null(names(x))) {
    message("List elements are not named. Using numeric values")
    names(x) <- 1:length(x)
  }
  x
}


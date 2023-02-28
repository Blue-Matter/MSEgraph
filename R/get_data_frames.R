
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
  if (length(Hist@OM@CurrentYr)>0) {
    hist.yrs <- (Hist@OM@CurrentYr -  Hist@OM@nyears + 1):Hist@OM@CurrentYr
    proj.yrs <- (Hist@OM@CurrentYr[1]+1):(Hist@OM@CurrentYr[1]+Hist@OM@proyears)
    out <- data.frame(Year=c(hist.yrs, proj.yrs), Period=c(rep('Historical', Hist@OM@nyears),
                                                    rep('Projection', Hist@OM@proyears)))
  } else {
    hist.yrs <- x@Data@Year
    CurrentYr <- max(hist.yrs)
    nyears <- length(hist.yrs)
    proyears <- x@Misc$MOM@proyears
    proj.yrs <- (CurrentYr+1):(CurrentYr[1]+proyears)
    out <- data.frame(Year=c(hist.yrs, proj.yrs), Period=c(rep('Historical', nyears),
                                                    rep('Projection', proyears)))
  }
  out
}

#' @export
#' @rdname get_years
get_years.multiHist <- function(x) {
  multiHist <- x

  hist.yrs <- multiHist[[1]][[1]]@Data@Year
  CurrentYr <- max(hist.yrs)
  nyears <- length(hist.yrs)
  proyears <- multiHist[[1]][[1]]@Misc$MOM@proyears
  proj.yrs <- (CurrentYr+1):(CurrentYr[1]+proyears)
  data.frame(Year=c(hist.yrs, proj.yrs), Period=c(rep('Historical', nyears),
                                                  rep('Projection', proyears)))
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


#' Title
#'
#' @param x
#'
#' @param variable
#' @param model
#'
#' @export
get_ts <- function(x, variable='Spawning Biomass', model='Model 1', scale=NULL) {
  UseMethod("get_ts")
}

#' @export
#' @rdname get_ts
get_ts.Hist <- function(x, variable='Spawning Biomass', model='Model 1', scale=NULL) {
  slot <- match_ts_variable(variable, 'Hist')
  if (grepl('\\()', slot)) {
    fn <- gsub('\\()','', slot)
    value <- get(fn)(x)
  } else {
    value <- as.vector(apply(x@TSdata[[slot]], 1:2, sum))
  }
  if (!is.null(scale) & inherits(scale, 'function')) {
     value <- scale(value)
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

#' @export
#' @rdname get_ts
get_ts.multiHist <- function(x, variable='Spawning Biomass', model='Model 1', scale=NULL) {
  n_stocks <- length(x)
  stock_names <- names(x)
  n_fleets <- length(x[[1]])
  fleet_names <- names(x[[1]])

  if (variable %in% c('Spawning Biomass', 'Biomass', 'Recruits')) {
    by_stock <- TRUE
    by_fleet <- FALSE
  }
  if (variable %in% c('Landings', 'Removals')) {
    by_stock <- TRUE
    by_fleet <- TRUE
  }

  years <- get_years(x) %>% filter(Period=='Historical')
  nsim <- x[[1]][[1]]@Misc$MOM@nsim

  slot <- match_ts_variable(variable, 'Hist')

  stock_list <- list()
  if (!by_fleet) {
    for (st in 1:n_stocks) {
      value <- as.vector(apply(x[[st]][[1]]@TSdata[[slot]], 1:2, sum))
      if (!is.null(scale) & inherits(scale, 'function')) {
        value <- scale(value)
      }

      stock_list[[st]] <- data.frame(Year=rep(years$Year, each=nsim),
                                     Sim=1:nsim,
                                     Value=value,
                                     Variable=variable,
                                     Period=years$Period,
                                     Model=stock_names[st])
    }
    out <- do.call('rbind', stock_list)
  } else {
    for (st in 1:n_stocks) {
      stock_list[[st]] <- list()
      for (fl in 1:n_fleets) {
        value <- as.vector(apply(x[[st]][[fl]]@TSdata[[slot]], 1:2, sum))
        if (!is.null(scale) & inherits(scale, 'function')) {
          value <- scale(value)
        }

        stock_list[[st]][[fl]] <- data.frame(Year=rep(years$Year, each=nsim),
                                       Sim=1:nsim,
                                       Value=value,
                                       Variable=variable,
                                       Period=years$Period,
                                       Model=stock_names[st],
                                       Fleet=fleet_names[fl])
      }
      stock_list[[st]] <- do.call('rbind', stock_list[[st]])
    }
    out <- do.call('rbind', stock_list)

  }
  out
}






#' @export
#' @rdname get_ts
get_ts.MSE <- function(x, variable='Spawning Biomass', model='Model 1', scale=NULL) {
  slot <- match_ts_variable(variable, 'MSE')
  nsim <- x@nsim
  nMPs <- x@nMPs
  MPs <- x@MPs

  hist_df <- get_ts(x@Hist, variable=variable, model=model, scale=scale) %>%
    add_MPs(., MPs)

  proj.years <- get_years(x) %>% filter(Period=='Projection')
  pyears <- length(proj.years)

  value <- as.vector(slot(x, slot))
  if (!is.null(scale) & inherits(scale, 'function')) {
    value <- scale(value)
  }

  proj_df <- data.frame(Year=rep(proj.years$Year, each=nsim*nMPs),
             Sim=1:nsim,
             MP=rep(MPs,each=nsim),
             Value=value,
             Period=proj.years$Period,
             Model=model,
             Variable=variable)

  bind_rows(hist_df, proj_df)

}

#' @export
#' @rdname get_ts
get_ts.list <- function(x, variable='Spawning Biomass', model='Model 1', scale=NULL) {
  if (inherits(x,'multiHist')) {
    return(get_ts.multiHist(x, variable=variable, model=model, scale=scale))
  }

  x <- check_names(x)
  purrr::map2(x, names(x), get_ts, variable=variable, scale=scale) %>%
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
get_SSB <- function(x, model='Model 1', ...) {
  get_ts(x, variable='Spawning Biomass', model=model, ...)
}

#' @export
#' @rdname get_SSB
get_Biomass <- function(x, model='Model 1', ...) {
  get_ts(x, variable='Biomass', model=model, ...)
}

#' @export
#' @rdname get_SSB
get_Landings <- function(x, model='Model 1', ...) {
  get_ts(x, variable='Landings', model=model, ...)
}

#' @export
#' @rdname get_SSB
get_Removals <- function(x, model='Model 1', ...) {
  get_ts(x, variable='Removals', model=model, ...)
}

#' @export
#' @rdname get_SSB
get_Recruits <- function(x, model='Model 1', ...) {
  get_ts(x, variable='Recruits', model=model, ...)
}

check_names <- function(x) {
  if (is.null(names(x))) {
    message("List elements are not named. Using numeric values")
    names(x) <- 1:length(x)
  }
  x
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
get_assess_estimates <- function(x, ...) {
  UseMethod('get_assess_estimates')
}

#' @export
#' @rdname get_assess_estimates
get_assess_estimates.MSE <- function(x, model='Model 1') {
  lapply(1:x@nMPs, get_assess_estimates.MSE.MP, MSE=x, model=model) %>%
    bind_rows()
}

get_assess_estimates.MSE.MP <- function(mp, MSE, model='Model 1') {
  lapply(1:MSE@nsim, function(x) {
    if (!is.null(MSE@PPD[[mp]]@Misc[[x]]$Assessment_report)) {
      MSE@PPD[[mp]]@Misc[[x]]$Assessment_report %>%
        mutate(MP = MSE@MPs[mp], Simulation = x, Model=model)
    }

  })
}

#' @export
#' @rdname get_assess_estimates
get_assess_estimates.list <- function(x) {
  x <- check_names(x)
  purrr::map2(x, names(x), get_assess_estimates.MSE) %>%
    purrr::list_rbind()
}


#' Title
#'
#' @param x
#' @param model
#'
#' @return
#' @export
#'
#' @examples
get_at_Age <- function(x, model='Model 1') {
  UseMethod("get_at_Age")
}

#' @export
#' @rdname get_at_Age
get_at_Age.Hist <- function(x, model='Model 1') {

  Vars <- c('Length', 'Weight', 'Select', 'Retention', 'Maturity', 'N.Mortality')
  years <- get_years(x)
  dd <- dim(x@AtAge$Select)
  nsim <- dd[1]
  nage <- dd[2]
  nyear <- dd[3]
  Ages <- 0:(nage-1)

  df_out <- data.frame(Year=rep(years$Year, each=nsim*nage),
                       Sim=1:nsim,
                       Age=rep(Ages, each=nsim),
                       Period=years$Period,
                       Model=model)

  for (i in seq_along(Vars)) {
    var <- Vars[i]
    df_out[[var]] <- as.vector(x@AtAge[[var]])
  }
  df_out
}

#' @export
#' @rdname get_at_Age
get_at_Age.list <- function(x, model='Model 1') {
  x <- check_names(x)
  purrr::map2(x, names(x), get_at_Age) %>%
    purrr::list_rbind()
}

#' @export
#' @rdname get_at_Age
get_at_Age.multiHist <- function(x, model='Model 1') {

  n_stocks <- length(x)
  stock_names <- names(x)
  n_fleets <- length(x[[1]])
  fleet_names <- names(x[[1]])

  stock_list <- list()
  for (st in 1:n_stocks) {
    stock_list[[st]] <- list()
    for (fl in 1:n_fleets) {
      df_out <- get_at_Age.Hist(x[[st]][[fl]])
      df_out$Stock <- stock_names[st]
      df_out$Fleet <- fleet_names[fl]
      stock_list[[st]][[fl]] <- df_out
    }
    stock_list[[st]] <- purrr::list_rbind(stock_list[[st]])
  }
  do.call('rbind',stock_list)
}

#' @export
#' @rdname get_at_Age
get_at_Age.MSE <- function(x, model='Model 1') {
  get_at_Age(x@Hist, model=model)

}


#' Title
#'
#' @param x
#' @param model
#'
#' @return
#' @export
#'
#' @examples
get_LifeHistory <- function(x, model='Model 1') {
  UseMethod("get_LifeHistory")
}

#' @export
#' @rdname get_LifeHistory
get_LifeHistory.Hist <- function(x, model='Model 1') {

  Vars <- c('Linf', 'K', 'M', 'L50', 'ageM')

  years <- get_years(x)
  dd <- dim(x@AtAge$Select)
  nsim <- dd[1]
  nage <- dd[2]
  nyear <- dd[3]
  Ages <- 0:(nage-1)

  df_out <- data.frame(Year=rep(years$Year, each=nsim),
                       Sim=1:nsim,
                       Period=years$Period,
                       Model=model)

  for (i in seq_along(Vars)) {
    var <- Vars[i]
    var2 <- paste0(Vars[i], 'array')
    df_out[[var]] <- as.vector(x@SampPars$Stock[[var2]])
  }
  df_out %>% tidyr::pivot_longer(., cols=all_of(Vars),
                                 names_to = 'Variable',
                                 values_to='Value')
}

#' @export
#' @rdname get_LifeHistory
get_LifeHistory.list <- function(x, model='Model 1') {
  x <- check_names(x)
  purrr::map2(x, names(x), get_LifeHistory) %>%
    purrr::list_rbind()
}

#' @export
#' @rdname get_LifeHistory
get_LifeHistory.MSE <- function(x, model='Model 1') {
  Hist <- x@Hist
  get_LifeHistory(Hist, model=model)
}



get_years <- function(x) {
  UseMethod("get_years")
}

get_years.MSE <- function(x) {
  MSE <- x
  hist.yrs <- (MSE@OM$CurrentYr[1] - MSE@nyears + 1):MSE@OM$CurrentYr[1]
  proj.yrs <- (MSE@OM$CurrentYr[1]+1):(MSE@OM$CurrentYr[1]+MSE@proyears)
  data.frame(Year=c(hist.yrs, proj.yrs), Period=c(rep('Historical', MSE@nyears),
                                                  rep('Projection', MSE@proyears)))
}

get_years.Hist <- function(x) {
  Hist <- x
  hist.yrs <- (Hist@OM@CurrentYr -  Hist@OM@nyears + 1):Hist@OM@CurrentYr
  proj.yrs <- (Hist@OM@CurrentYr[1]+1):(Hist@OM@CurrentYr[1]+Hist@OM@proyears)
  data.frame(Year=c(hist.yrs, proj.yrs), Period=c(rep('Historical', Hist@OM@nyears),
                                                  rep('Projection', Hist@OM@proyears)))
}



get_SSB <- function(x, ...) {
  UseMethod("get_SSB")
}

get_SSB.Hist <- function(x, name='Name') {
  years <- get_years(x) %>% filter(Period=='Historical')
  dd <- dim(x@TSdata$SBiomass)
  nsim <- dd[1]
  data.frame(Year=rep(years$Year, each=nsim),
             Sim=1:nsim,
             Value=as.vector(apply(x@TSdata$SBiomass, 1:2, sum)),
             Period=years$Period,
             Name=name)
}


get_SSB.MSE <- function(x, name='Name') {
  years <- get_years(x) %>% filter(Period=='Projection')
  nyears <- length(years)
  nsim <- x@nsim
  nMPs <- x@nMPs
  MPs <- x@MPs
  data.frame(Year=rep(years$Year, each=nsim*nMPs),
             Sim=1:nsim,
             MP=rep(MPs,each=nsim),
             Value=as.vector(x@SSB),
             Period=years$Period,
             Name=name)

}

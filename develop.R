# packages
# authors: AH, RF

# # https://github.com/robynforrest/FSERP-herring - RF plotting functions
# hist_hMSEs <- readRDS("C:/Users/User/Documents/GitHub/FSERP-herring/MSEs/hist_hMSEs.rda")
# MSEs <- readRDS("C:/Users/User/Documents/GitHub/FSERP-herring/MSEs/HG/hMSEs_NF.rda")
#


library(dplyr)
library(ggplot2)
library(purrr)
library(RColorBrewer)


library(openMSE)
OM1 <- testOM
OM2 <- testOM
OM1@CurrentYr <- 2023
OM2@CurrentYr <- 2023
OM2@seed <- OM2@seed * 2

Hist1 <- Simulate(OM1)
Hist2 <- Simulate(OM2)

MSE1 <- Project(Hist1)
MSE2 <- Project(Hist2)

HistList <- list(Model_1=Hist1, Model_2=Hist2, Model_3=Hist1, Model_4=Hist2)
MSEList <- list(Model_1=MSE1, Model_2=MSE2, Model_3=MSE1, Model_4=MSE2)


plot_SSB(Hist1)
plot_SSB(HistList)

plot_SSB(MSE1)
plot_SSB(MSEList)

plot_SSB(list(Hist1, Hist2))
plot_SSB(list(Hist1, MSE2)) # add check for this

# include historical in MSE projections??



get_Biomass <- function(x, ...) {
  UseMethod("get_Biomass")
}

get_Landings <- function(x, ...) {
  UseMethod("get_Landings")
}

get_Removals <- function(x, ...) {
  UseMethod("get_Removals")
}




get_Biomass.Hist <- function(x, name='Name') {
  years <- get_years(x) %>% filter(Period=='Historical')
  dd <- dim(x@TSdata$Biomass)
  nsim <- dd[1]
  data.frame(Year=rep(years$Year, each=nsim),
             Sim=1:nsim,
             Value=as.vector(apply(x@TSdata$Biomass, 1:2, sum)),
             Period=years$Period,
             Name=name)
}

get_Landings.Hist <- function(x, name='Name') {
  years <- get_years(x) %>% filter(Period=='Historical')
  dd <- dim(x@TSdata$Landings)
  nsim <- dd[1]
  data.frame(Year=rep(years$Year, each=nsim),
             Sim=1:nsim,
             Value=as.vector(apply(x@TSdata$Landings, 1:2, sum)),
             Period=years$Period,
             Name=name)
}

get_Removals.Hist <- function(x, name='Name') {
  years <- get_years(x) %>% filter(Period=='Historical')
  dd <- dim(x@TSdata$Landings)
  nsim <- dd[1]
  data.frame(Year=rep(years$Year, each=nsim),
             Sim=1:nsim,
             Value=as.vector(apply(x@TSdata$Removals, 1:2, sum)),
             Period=years$Period,
             Name=name)
}




get_years(MSE)
get_years(Hist)

get_SSB(Hist)
get_SSB(MSE)

get_Landings(Hist)

x <- Hist
x <- hist_hMSEs




plot_Biomass <- function(x, ...) {
  UseMethod("plot_Biomass")
}

plot_Landings <- function(x, ...) {
  UseMethod("plot_Landings")
}

plot_Removals <- function(x, ...) {
  UseMethod("plot_Removals")
}






plot_Biomass.Hist <- function(x,
                              title='',
                              xlab='Year',
                              ylab='Biomass',
                              quantiles=c(0.025, 0.975),
                              ...) {
  df <- get_Biomass(x) %>%
    group_by(Year, Name) %>%
    summarize(Lower=quantile(Value, quantiles[1]),
              Median=median(Value),
              Upper=quantile(Value, quantiles[2]),
              .groups='drop')

  plot_ts_quants(df, xlab=xlab, ylab=ylab, title=title, ...)
}

plot_Biomass.list <- function(x,
                          title='',
                          xlab='Year',
                          ylab='Spawning Biomass',
                          quantiles=c(0.025, 0.975),
                          ...) {
  if (is.null(names(x))) {
    message("List elements are not named. Using numeric values")
    names(x) <- 1:length(x)
  }

  df <- purrr::map2_df(x, names(x), get_Biomass) %>%
    group_by(Year, Name) %>%
    summarize(Lower=quantile(Value, quantiles[1]),
              Median=median(Value),
              Upper=quantile(Value, quantiles[2]),
              .groups='drop')

  plot_ts_quants(df, xlab=xlab, ylab=ylab, title=title, ...)
}



plot_Biomass(Hist)
plot_Biomass(hist_hMSEs)
plot_Biomass(list(Hist, Hist))









plot_SSB(Hist)
plot_SSB(hist_hMSEs)
plot_SSB(x=list(Hist, Hist))

plot_Biomass
plot_Catch
plot_Removals







# Objects

# Stock

# Fleet

# Obs

# Imp

# OM

# Hist

# MSE

# multiHist

# Assess





remotes::install_github("Blue-Matter/SAMtool") # version 1.5.1 or later
library(tidyverse)

##### Create two SCA MPs with reporting
SCA_40_10 <- make_MP(SCA, HCR40_10, diagnostic = "full")
SCA_60_20 <- make_MP(SCA, HCR60_20, diagnostic = "full")

##### Run simulations
OM <- MSEtool::testOM; OM@proyears <- 20
myMSE <- MSEtool::runMSE(OM = OM, MPs = c("SCA_40_10", "SCA_60_20"))

##### Plot assessment estimates (F and SSB) inside MP
retrospective_AM(myMSE, MP = "SCA_40_10", sim = 2)

# How to get all the estimates
assess_estimates <- lapply(1:myMSE@nMPs, function(m) {
  lapply(1:myMSE@nsim, function(x) {
    myMSE@PPD[[m]]@Misc[[x]]$Assessment_report %>%  # Now a list of data frames instead of Assessment objects, saves a lot of space
      mutate(MP = myMSE@MPs[m], Simulation = x)
  }) %>% bind_rows()
}) %>% bind_rows()

##### Convergence diagnostics
diagnostic(myMSE, MP = "SCA_40_10")

# How to get all the reporting
conv_statistics <- lapply(1:myMSE@nMPs, function(m) {
  lapply(1:myMSE@nsim, function(x) {
    myMSE@PPD[[m]]@Misc[[x]]$diagnostic %>%  # Now a list of data frames instead of a list of lists, saves a lot of space
      mutate(MP = myMSE@MPs[m], Simulation = x)
  }) %>% bind_rows()
}) %>% bind_rows()



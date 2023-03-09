# packages
# authors: AH, RF

# # https://github.com/robynforrest/FSERP-herring - RF plotting functions
# hist_hMSEs <- readRDS("C:/Users/User/Documents/GitHub/FSERP-herring/MSEs/hist_hMSEs.rda")
# MSEs <- readRDS("C:/Users/User/Documents/GitHub/FSERP-herring/MSEs/HG/hMSEs_NF.rda")

# TODO:
# - MSEtool: Hist object should always be included in MSE object


# Functions:
# get_ - return data.frames with time-series information
# plot_ - plot the data.frames

# Object classes:
# - Hist
# - MSE
# - list(Hist, Hist, ...) # list of Hist objects
# - list(MSE, MSE, ...) # list of MSE objects
# - PPD@Misc - Assessment_report

# Not done yet:
# - multiHist - incomplete
# - MMSE
# - RCM
# - Assessment



library(MSEgraph)

# Get information from objects ----

## Meta-data ----
get_Years(Hist1) %>% head()
get_Years(MSE1) %>% head()

get_Metadata(Hist1) %>% names()
get_Metadata(HistList) %>% names()

get_Metadata(MSE1) %>% names()
get_Metadata(MSEList) %>% names()

## Assessment Estimates ----
# for MSE objects that have SAMtool assessment MPs
# requires SAMtool v1.5.1 or later
get_Assess_Estimates(MSE1) %>% head()
get_Assess_Estimates(MSE1) %>% tail()

get_Assess_Estimates(MSEList) %>% head()
get_Assess_Estimates(MSEList) %>% tail()

assess_estimates <- get_Assess_Estimates(MSE1)
# estimated variables returned by SAMtool MPs in `PPD@Misc`
assess_estimates$Variable %>% unique()

## At-Age Schedules by Year ----
get_at_Age(Hist1)
get_at_Age(HistList)
get_at_Age(HistList) %>% tail()

get_at_Age(MSE1)
get_at_Age(MSEList)
get_at_Age(MSEList) %>% tail()

## Time-Series ----

### Total Biomass ----
get_Biomass(Hist1) %>% head()
get_Biomass(MSE1) %>% head()
get_Biomass(MSE1) %>% tail()

# use a custom function to scale Value
get_Biomass(MSE1, scale=divide_1000) %>% head()

get_Biomass(HistList) %>% head()
get_Biomass(MSEList) %>% head()

### Spawning Biomass ----
get_SSB(Hist1) %>% head()
get_SSB(MSE1) %>% head()
get_SSB(HistList) %>% head()
get_SSB(MSEList) %>% head()

### Landings ----
get_Landings(Hist1) %>% head()
get_Landings(MSE1) %>% head()
get_Landings(HistList) %>% head()
get_Landings(MSEList) %>% head()

### Removals ----
get_Removals(Hist1) %>% head()
get_Removals(MSE1) %>% head()
get_Removals(HistList) %>% head()
get_Removals(MSEList) %>% head()


### Recruits ----
get_Recruits(Hist1) %>% head()
get_Recruits(HistList) %>% head()

# not working yet
get_Recruits(MSE1) %>% head()
get_Recruits(MSEList) %>% head()

## Life-History Parameters by Year ----

get_LifeHistory(Hist1)
get_LifeHistory(MSE1)
get_LifeHistory(HistList)
get_LifeHistory(MSEList)




# Plots ------

## Time-Series Plots ----
p <- plot_SSB(Hist1, print=FALSE)
p$plot
p$df

# Hist - no facet
# HistList - facet by model

# MSE - facet by MP
# MSE - no facet

# MSElist - facet by MP and model
# MSElist - no facet by MP

# up to here - figure out MSE with facet by MP - need to include Historical
plot_SSB(Hist1)
plot_SSB(Hist1, facet=FALSE)
plot_SSB(Hist1, facet=FALSE, inc.Legend=FALSE)
plot_SSB(HistList)
plot_SSB(HistList, facet=FALSE)

plot_SSB(multiHist)
plot_SSB(multiHist, facet=FALSE)

plot_SSB(MSE1)
plot_SSB(MSE1, facet=FALSE)
plot_SSB(MSE1, inc.Hist=TRUE)
plot_SSB(MSE1, inc.Hist=TRUE, facet=FALSE)

plot_SSB(MSEList)
plot_SSB(MSEList, inc.Hist=TRUE)
plot_SSB(MSEList, facet=FALSE)
plot_SSB(MSEList, facet=FALSE, quantiles=0.5)

# median only
plot_SSB(Hist1, quantiles=0.5)
plot_SSB(MSEList, quantiles=0.5)
plot_SSB(MSEList, quantiles=0.5, facet=FALSE)

plot_Biomass(Hist1)
plot_Biomass(HistList)
plot_Biomass(MSE1)
plot_Biomass(MSEList)

plot_Landings(Hist1)
plot_Landings(HistList)
plot_Landings(MSE1)
plot_Landings(MSEList)

plot_Removals(Hist1)
plot_Removals(HistList)
plot_Removals(MSE1)
plot_Removals(MSEList)

plot_Removals(multiHist)

plot_Recruits(Hist1)
plot_Recruits(MSE1) # todo
plot_Recruits(HistList)
plot_Recruits(MSEList) # todo

# revise TS plots for different facet types
plot_LifeHistory <- function(x,
                             title='',
                             xlab='Year',
                             ylab='Recruits',
                             quantiles=c(0.025, 0.975),
                             ...) {

  df <- summary_df(x, quantiles=quantiles, get_LifeHistory)
  plot_ts_quants(df, xlab=xlab, ylab=ylab, title=title, ...)

}

# ---- Plot At-Age Schedules ----



# Linf

# K

# M

# L50

# LifeHistory





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



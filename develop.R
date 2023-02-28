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
# - multiHist - incomplete
# - list(Hist, Hist, ...) # list of Hist objects
# - list(MSE, MSE, ...) # list of MSE objects
# - PPD@Misc - Assessment_report

# Not done yet:
# - RCM
# - Assessment
# - MMSE

# Load some Hist and MSE objects for development
test.objects <- readRDS('test-objects.rda')
Hist1 <- test.objects$Hist1
Hist2 <- test.objects$Hist2
MSE1 <- test.objects$MSE1
MSE2 <- test.objects$MSE2
HistList <- test.objects$HistList
MSEList <- test.objects$MSEList
multiHist <- test.objects$multiHist

# Everything below uses only MSEgraph package (and dependencies)
library(MSEgraph)

# ---- Get time-series data.frames ----


# ---- Spawning Biomass ----
get_SSB(Hist1) %>% head()
get_SSB(Hist1) %>% tail()

get_SSB(MSE1) %>% head()
get_SSB(MSE1) %>% tail()

get_SSB(HistList) %>% head()
get_SSB(HistList) %>% tail()

get_SSB(MSEList) %>% head()
get_SSB(MSEList) %>% tail()

# use a custom function to scale Value
divide_1000 <- function(x) x/1000
get_SSB(MSEList, scale=divide_1000)


# ---- Biomass ----
get_Biomass(Hist1)
get_Biomass(MSE1)
get_Biomass(HistList)
get_Biomass(MSEList)


# ---- Landings ----
get_Landings(Hist1)
get_Landings(MSE1)
get_Landings(HistList)
get_Landings(MSEList)


# ---- Removals ----
get_Removals(Hist1)
get_Removals(MSE1)
get_Removals(HistList)
get_Removals(MSEList)


# ---- Recruits ----
get_Recruits(Hist1)
get_Recruits(HistList)

# TODO - requires update to MSEtool
get_Recruits(MSE1)
get_Recruits(MSEList)

# ---- Assessment Estimates ----
get_assess_estimates(MSE1) %>% head()
get_assess_estimates(MSE1) %>% tail()

get_assess_estimates(MSEList) %>% head()
get_assess_estimates(MSEList) %>% tail()

assess_estimates <- get_assess_estimates(MSE1)
assess_estimates$variable %>% unique()

# ---- At-Age Schedules by Year ----
# Works on Hist, MSE, and multiHist objects

get_at_Age(Hist1)
get_at_Age(HistList) %>% head()
get_at_Age(HistList) %>% tail()

get_at_Age(MSE1)

get_at_Age(MSEList) %>% head()
get_at_Age(MSEList) %>% tail()



# ---- Life-History Parameters by Year ----

get_LifeHistory(Hist1)
get_LifeHistory(MSE1)
get_LifeHistory(HistList)
get_LifeHistory(MSEList)




# ---- Plots ------

# ----- Time-Series Plots ----
p <- plot_SSB(Hist1, print=FALSE)
p$plot
p$df

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



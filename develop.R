# packages
# authors: AH, RF

# # https://github.com/robynforrest/FSERP-herring - RF plotting functions
# hist_hMSEs <- readRDS("C:/Users/User/Documents/GitHub/FSERP-herring/MSEs/hist_hMSEs.rda")
# MSEs <- readRDS("C:/Users/User/Documents/GitHub/FSERP-herring/MSEs/HG/hMSEs_NF.rda")

# TODO:
# - MSEtool: Hist object should always be included in MSE object - added to dev version of MSEtool





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
library(dplyr)

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

# plot functions return list with `plot` and `df`:
p <- plot_SSB(Hist1)

# ggplot object:
p$plot + ggplot2::theme_classic() + ggplot2::labs(title='Test')

# summary data.frame
p$df

plot_SSB(Hist1)
plot_SSB(Hist1, years=1990:2000) # subset the years

plot_SSB(HistList)
plot_SSB(HistList, facet=FALSE)

plot_SSB(MSE1)
plot_SSB(MSE1, facet=FALSE)
plot_SSB(MSE1, inc.Hist=TRUE)
plot_SSB(MSE1, inc.Hist=TRUE, facet=FALSE)

plot_SSB(MSEList)
plot_SSB(MSEList, facet=FALSE)
plot_SSB(MSEList, inc.Hist=TRUE)
plot_SSB(MSEList, inc.Hist=TRUE, facet=FALSE)


# median only
plot_SSB(Hist1, quantiles=0.5)
plot_SSB(MSE1, quantiles=0.5)
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

plot_Recruits(Hist1)
plot_Recruits(MSE1)
plot_Recruits(HistList)
plot_Recruits(MSEList)

# Life-History parameters
plot_LifeHistory(Hist1)
plot_LifeHistory(HistList)
plot_LifeHistory(MSE1)
plot_LifeHistory(MSEList)


# ---- Plot At-Age Schedules ----

plot_Length(Hist1)
plot_Length(Hist1, scale=cm2inch, ylab='Length (inch)')

plot_Length(Hist1, years=2000:2010)
plot_Length(HistList)
plot_Length(MSE1)
plot_Length(MSEList)
plot_Length(MSEList, years=2000:2010)

plot_Weight(Hist1)
plot_Weight(MSE1)
plot_Weight(HistList)
plot_Weight(MSEList)

plot_Maturity(Hist1)
plot_Maturity(MSE1)
plot_Maturity(HistList)
plot_Maturity(MSEList)

plot_N.Mortality(Hist1)
plot_N.Mortality(MSE1)
plot_N.Mortality(HistList)
plot_N.Mortality(MSEList)

plot_Select(Hist1)
plot_Select(MSE1)
plot_Select(HistList)
plot_Select(MSEList)

plot_Retention(Hist1)
plot_Retention(MSE1)
plot_Retention(HistList)
plot_Retention(MSEList)

plot_Select_Maturity(Hist1)
plot_Select_Maturity(MSE1)
plot_Select_Maturity(HistList)
plot_Select_Maturity(MSEList)



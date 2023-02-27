# packages
# authors: AH, RF

# # https://github.com/robynforrest/FSERP-herring - RF plotting functions
# hist_hMSEs <- readRDS("C:/Users/User/Documents/GitHub/FSERP-herring/MSEs/hist_hMSEs.rda")
# MSEs <- readRDS("C:/Users/User/Documents/GitHub/FSERP-herring/MSEs/HG/hMSEs_NF.rda")

# Create some Hist and MSE objects for development
library(MSEtool)
OM1 <- MSEtool::testOM
OM2 <- MSEtool::testOM
OM1@CurrentYr <- 2023
OM2@CurrentYr <- 2023
OM2@seed <- OM2@seed * 2

Hist1 <- MSEtool::Simulate(OM1)
Hist2 <- MSEtool::Simulate(OM2)

# need to edit MSE object to always include Hist
MSE1 <- MSEtool::Project(Hist1, extended = TRUE)
MSE2 <- MSEtool::Project(Hist2, extended = TRUE)

HistList <- list(Model_1=Hist1, Model_2=Hist2, Model_3=Hist1, Model_4=Hist2)
MSEList <- list(Model_1=MSE1, Model_2=MSE2, Model_3=MSE1, Model_4=MSE2)


multiHist <- readRDS("C:/Users/User/Documents/GitHub/SAFMC-MSE/Hist_Objects/RS_SEDAR.hist")

# devtools::load_all()

# Everything below uses only MSEgraph package (and dependencies)
library(MSEgraph)

# ---- Get time-series data.frames ----

get_SSB(Hist1)
get_SSB(MSE1)
get_SSB(HistList)
get_SSB(MSEList)

divide_1000 <- function(x) x/1000
get_SSB(multiHist, scale=divide_1000)

get_Biomass(Hist1)
get_Biomass(MSE1)
get_Biomass(HistList)
get_Biomass(MSEList)

get_Biomass(multiHist)

get_Landings(Hist1)
get_Landings(MSE1)
get_Landings(HistList)
get_Landings(MSEList)

get_Removals(Hist1)
get_Removals(MSE1)
get_Removals(HistList)
get_Removals(MSEList)

get_Removals(multiHist)


get_Recruits(Hist1)
get_Recruits(HistList)
get_Recruits(MSE1) # doesn't work yet - requires latest version of MSEtool
get_Recruits(MSEList) # doesn't work yet - requires latest version of MSEtool


get_at_Age <- function(x, model='Model 1') {
  UseMethod("get_at_Age")
}

#' @export
#' @rdname get_ts
get_at_Age.Hist <- function(x, model='Model 1') {

  Vars <- c('Length', 'Weight', 'Select', 'Retention',
            'Maturity', 'N.Mortality', 'Z.Mortality', 'F.Mortality',
            'Number')
  years <- get_years(x)
  nage <- length(x@AtAge$Length[1,,1])
  Ages <- 0:(nage-1)

  df_out <- data.frame(Year=rep(years$Year, each=nage),
                       Age=Ages,
                       Model=model)
  for (i in seq_along(Vars)) {
    var <- Vars[i]
    dd <- dim(x@AtAge[[var]])
    if (length(dd)==3) {
      df_out[[var]] <- as.vector(x@AtAge[[var]][1,,])
    } else {
      df_out[[var]] <- as.vector(x@AtAge[[var]][1,,,1])
    }
  }
  df_out

}
get_at_Age.Hist(Hist1)



p <- plot_SSB(Hist1, print=FALSE)
p$plot
p$df

plot_SSB(Hist1)
plot_SSB(Hist1, facet=FALSE)
plot_SSB(Hist1, facet=FALSE, inc.Legend=FALSE)
plot_SSB(HistList)
plot_SSB(HistList, facet=FALSE)

plot_SSB(multiHist)


plot_SSB(MSE1)
plot_SSB(MSE1, facet=FALSE)
plot_SSB(MSE1, inc.Hist=TRUE)
plot_SSB(MSE1, inc.Hist=TRUE, facet=FALSE)

plot_SSB(MSEList)
plot_SSB(MSEList, facet=FALSE)

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

# Recruits

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



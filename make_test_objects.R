remotes::install_github("Blue-Matter/SAMtool") # version 1.5.1 or later
remotes::install_github("Blue-Matter/MSEtool") # v3.7.0 or later

make_test_objects <- function() {

  # Create some Hist and MSE objects for development
  library(MSEtool)
  library(SAMtool)
  OM1 <- MSEtool::testOM
  OM2 <- MSEtool::testOM
  OM1@CurrentYr <- 2023
  OM2@CurrentYr <- 2023
  OM2@seed <- OM2@seed * 2

  Hist1 <- MSEtool::Simulate(OM1)
  Hist2 <- MSEtool::Simulate(OM2)

  SCA_40_10 <- make_MP(SCA, HCR40_10, diagnostic = "full")
  SCA_60_20 <- make_MP(SCA, HCR60_20, diagnostic = "full")

  MPs <- c('FMSYref', 'FMSYref50', 'SCA_40_10', 'SCA_60_20' )


  MSE1 <- MSEtool::Project(Hist1, MPs=MPs, extended = TRUE)
  MSE2 <- MSEtool::Project(Hist2, MPs=MPs, extended = TRUE)

  HistList <- list(Model_1=Hist1, Model_2=Hist2, Model_3=Hist1, Model_4=Hist2)
  MSEList <- list(Model_1=MSE1, Model_2=MSE2, Model_3=MSE1, Model_4=MSE2)

  MOM <- SWOMSE::MOM_000
  MOM@nsim <- 5
  MOM@Fleets[[1]][[2]] <- MOM@Fleets[[1]][[1]]
  MOM@Fleets[[2]][[2]] <- MOM@Fleets[[2]][[1]]
  names(MOM@Fleets[[1]]) <- c('Fleet 1', 'Fleet 2')
  names(MOM@Fleets[[2]]) <- c('Fleet 1', 'Fleet 2')
  MOM@cpars[[1]][[2]] <- MOM@cpars[[1]][[1]]
  MOM@cpars[[2]][[2]] <- MOM@cpars[[2]][[1]]
  MOM@CatchFrac[[1]] <- matrix(0.5, nrow=48, ncol=2)
  MOM@CatchFrac[[2]] <- matrix(0.5, nrow=48, ncol=2)

  multiHist <- SimulateMOM(MOM)

  out <- list()
  out$Hist1 <- Hist1
  out$Hist2 <- Hist2
  out$MSE1 <- MSE1
  out$MSE2 <- MSE2
  out$HistList <- HistList
  out$MSEList <- MSEList
  out$multiHist <- multiHist


  saveRDS(out, 'test-objects.rda')

}

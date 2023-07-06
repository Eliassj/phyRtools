test_that("Loading data check", {
  spike <- readRDS(test_path("fixtures", "spiketimes.rds"))
  info <- readRDS(test_path("fixtures", "info.rds"))
  trig <- readRDS(test_path("fixtures", "triggers.rds"))
  ld <- loadSpikes(
    path = test_path("fixtures",
                     "Våra"),
    triggerfile = "768.exported.imec0.ap.csv"
  )
  expect_equal(ld[["spiketimes"]], spike)
  expect_equal(ld[["info"]], info)
  expect_equal(ld[["triggers"]], trig)
})

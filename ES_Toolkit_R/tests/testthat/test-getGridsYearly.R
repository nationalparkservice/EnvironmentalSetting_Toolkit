library(testthat)
library(EnvironmentalSettingToolkit)

# Note: does not encompass getMetricGrids() operations

context("Tests for getGrids() yearly")
test_that("Tests for getGrids() yearly",
          {
            expect_s4_class(
              getGrids(
                unitCode = list("DEVA"),
                sdate = "2016",
                edate = "2017",
                climateParameters = list("yly_mint", "yly_maxt"),
                duration = "yly"
              ),
              "RasterStack"
            )
          })
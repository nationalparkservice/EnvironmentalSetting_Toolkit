library(testthat)
library(EnvironmentalSettingToolkit)

# Note: does not encompass getMetricGrids() operations

context("Tests for getGrids() monthly")
test_that("Tests for getGrids() monthly",
          {
            expect_s4_class(
              getGrids(
                unitCode = list("PRWI"),
                sdate = "201606",
                edate = "201607",
                climateParameters = list("mly_mint", "mly_maxt"),
                duration = "mly"
              ),
              "RasterStack"
            )
          })

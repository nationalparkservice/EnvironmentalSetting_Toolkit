library(testthat)
library(EnvironmentalSettingToolkit)

# Note: does not encompass getMetricGrids() operations

context("Tests for getGrids()")
test_that("Tests for getGrids()",
          {
            # Testing with inputs to getGrids() calls
            # RasterStack is an S4 object, expect_output() will not detect that...
            expect_s4_class(
              getGrids(
                unitCode = list("GRSM"),
                sdate = "20160615",
                edate = "20160616",
                climateParameters = list("mint", "maxt")
              ),
              "RasterStack"
            )
          })

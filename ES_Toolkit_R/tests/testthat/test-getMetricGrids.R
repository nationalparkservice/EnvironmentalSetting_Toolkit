library(testthat)
library(EnvironmentalSettingToolkit)

context("Tests for getMetric()")
test_that("Tests for getMetric()",
          {
            # Testing with inputs to getMetricGrids() calls
            # RasterStack is an S4 object, expect_output() will not detect that...
            setwd("D:/trash/ToolkitTests")
            unitAOA <-
              getAOAFeature(unitCode = "APIS", aoaExtent = "km30")
            
            expect_s4_class(
              getMetricGrids(
                featurePolygon = unitAOA,
                metric = "CGT1",
                unitCode = "APIS",
                sdate = "2017",
                edate = "2017",
                outputNormals = FALSE
              ),
              "RasterStack"
            )
          })

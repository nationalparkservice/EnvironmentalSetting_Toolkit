library(testthat)
library(EnvironmentalSettingToolkit)

context("Tests for findStation()")
test_that("Tests for findStation()",
          {
            # Testing basic requests for known problematic station set
            expect_type(findStation(
              unitCode = "AGFO",
              distance = 30,
              climateParameters = list('pcpn')
            ),
            "list")
            expect_length(findStation(
              unitCode = "AGFO",
              distance = 30,
              climateParameters = list('maxt')
            ),
            19)
            # Testing request for degree day parameters
            expect_type(findStation(
              unitCode = "FOBU",
              climateParameters = list('gdd', 'hdd', 'cdd')
            ),
            "list")
            expect_length(findStation(
              unitCode = "FOBU",
              climateParameters = list('gdd', 'hdd', 'cdd')
            ),
            19)
            # Testing custom bounding box as used with the getStationMetrics() function
            expect_type(findStation(
              unitCode = "CHIR",
              distance = 1,
              customBBox = "-109.75807963885875, 31.663585919768266, -108.9370618183587, 32.35854176354032"
            ),
            "list")
            expect_length(findStation(
              unitCode = "CHIR",
              distance = 1,
              customBBox = "-109.75807963885875, 31.663585919768266, -108.9370618183587, 32.35854176354032",
              climateParameters = list('gdd', 'hdd', 'cdd')
            ),
            19)
          })
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
            # Testing custom bounding box as used with the getStationMetrics() function for known problematic station set
            expect_type(findStation(
              unitCode = "ABLI",
              distance = 1,
              customBBox = "-86.1175780158213, 37.22600436021451, -85.25506794116819, 37.91959868546151"
            ),
            "list")
            expect_length(findStation(
              unitCode = "ABLI",
              distance = 1,
              customBBox = "-86.1175780158213, 37.22600436021451, -85.25506794116819, 37.91959868546151"
            ),
            19)
            # Testing request for degree day parameters
            expect_type(findStation(
              unitCode = "ABLI",
              distance = 1,
              customBBox = "-86.1175780158213, 37.22600436021451, -85.25506794116819, 37.91959868546151",
              climateParameters = list('gdd', 'hdd', 'cdd')
            ),
            "list")
            expect_length(findStation(
              unitCode = "ABLI",
              distance = 1,
              customBBox = "-86.1175780158213, 37.22600436021451, -85.25506794116819, 37.91959868546151",
              climateParameters = list('gdd', 'hdd', 'cdd')
            ),
            19)
          })
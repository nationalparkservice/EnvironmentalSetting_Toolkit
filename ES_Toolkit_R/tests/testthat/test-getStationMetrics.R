library(testthat)
library(EnvironmentalSettingToolkit)

context("Tests for getStationMetrics()")
test_that("Tests for getStationMetrics()",
          {
            # Testing with inputs to getStationMetrics() calls for three metrics (one derived)
            climateStationsMaxt <-  findStation(
              unitCode = "AGFO",
              distance = 30,
              climateParameters = list('maxt')
            )
            
            expect_type(
              suppressWarnings(getStationMetrics(
                climateStations = unique(climateStationsMaxt$uid),
                sdate = "1958-01-01",
                edate = "2018-12-31",
                metric = "CST5",
                filePathAndRootname = "D:/trash/ToolkitTests/cst5"
              )),
              "list"
            )
            expect_length(
              suppressWarnings(getStationMetrics(
                climateStations = unique(climateStationsMaxt$uid),
                sdate = "1958-01-01",
                edate = "2018-12-31",
                metric = "CST5",
                filePathAndRootname = "D:/trash/ToolkitTests/cst5"
              )),
              16
            )
            
            climateStationsPcpn <-  findStation(
              unitCode = "AGFO",
              distance = 30,
              climateParameters = list('pcpn')
            )
            
            expect_type(
              suppressWarnings(getStationMetrics(
                climateStations = unique(climateStationsPcpn$uid),
                sdate = "1958-01-01",
                edate = "2018-12-31",
                metric = "CSP1",
                filePathAndRootname = "D:/trash/ToolkitTests/csp1"
              )),
              "list"
            )
            expect_length(
              suppressWarnings(getStationMetrics(
                climateStations = unique(climateStationsPcpn$uid),
                sdate = "1958-01-01",
                edate = "2018-12-31",
                metric = "CSP1",
                filePathAndRootname = "D:/trash/ToolkitTests/csp1"
              )),
              16
            )
            
            expect_type(
              suppressWarnings(getStationMetrics(
                climateStations = unique(climateStationsPcpn$uid),
                sdate = "1958-01-01",
                edate = "2018-12-31",
                metric = "CSP3",
                filePathAndRootname = "D:/trash/ToolkitTests/csp3"
              )),
              "list"
            )
            expect_length(
              suppressWarnings(getStationMetrics(
                climateStations = unique(climateStationsPcpn$uid),
                sdate = "1958-01-01",
                edate = "2018-12-31",
                metric = "CSP3",
                filePathAndRootname = "D:/trash/ToolkitTests/csp3"
              )),
              5
            )
          })

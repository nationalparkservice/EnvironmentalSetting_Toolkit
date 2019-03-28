library(testthat)
library(EnvironmentalSettingToolkit)

context("Tests for getWxObservations()")
test_that("Tests for getWxObservations()",
          {
            # Testing with inputs to getStationMetrics() calls for three metrics
            climateStationsMaxt <-  findStation(
              unitCode = "AGFO",
              distance = 30,
              climateParameters = list('maxt')
            )
            expect_type(suppressWarnings(
              getWxObservations(
                climateStations = climateStationsMaxt,
                climateParameters = list("maxt"),
                sdate = "1958-01-01",
                edate = "2018-12-31",
                duration = "yly",
                interval = "yly",
                reduceCodes = "cnt_ge_90.0",
                maxMissing = 999,
                metric = "CST1"
              )
            ),
            "list")
            expect_length(suppressWarnings(
              getWxObservations(
                climateStations = climateStationsMaxt,
                climateParameters = list("maxt"),
                sdate = "1958-01-01",
                edate = "2018-12-31",
                duration = "yly",
                interval = "yly",
                reduceCodes = "cnt_ge_90.0",
                maxMissing = 999,
                metric = "CST1"
              )
            ),
            16)
            
            expect_type(suppressWarnings(
              getWxObservations(
                climateStations = climateStationsMaxt,
                climateParameters = list("gdd"),
                sdate = "1958-01-01",
                edate = "2018-12-31",
                duration = "yly",
                interval = "yly",
                reduceCodes = "cnt_gt_0",
                maxMissing = 999,
                metric = "CST5"
              )
            ),
            "list")
            expect_length(suppressWarnings(
              getWxObservations(
                climateStations = climateStationsMaxt,
                climateParameters = list("gdd"),
                sdate = "1958-01-01",
                edate = "2018-12-31",
                duration = "yly",
                interval = "yly",
                reduceCodes = "cnt_gt_0",
                maxMissing = 999,
                metric = "CST5"
              )
            ),
            16)
            
            climateStationsPcpn <-  findStation(
              unitCode = "AGFO",
              distance = 30,
              climateParameters = list('pcpn')
            )
            expect_type(suppressWarnings(
              getWxObservations(
                climateStations = climateStationsPcpn,
                climateParameters = list("pcpn"),
                sdate = "1958-01-01",
                edate = "2018-12-31",
                duration = "yly",
                interval = "yly",
                reduceCodes = "cnt_ge_1.0",
                maxMissing = 999,
                metric = "CSP1"
              )
            ),
            "list")
            expect_length(suppressWarnings(
              getWxObservations(
                climateStations = climateStationsPcpn,
                climateParameters = list("pcpn"),
                sdate = "1958-01-01",
                edate = "2018-12-31",
                duration = "yly",
                interval = "yly",
                reduceCodes = "cnt_ge_1.0",
                maxMissing = 999,
                metric = "CSP1"
              )
            ),
            16)
          })
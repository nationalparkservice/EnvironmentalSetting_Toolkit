library(testthat)
library(EnvironmentalSettingToolkit)

context("Tests for getWxObservations() derived metrics")
test_that("Tests for getWxObservations() derived metrics",
          {
            # Testing with inputs to getStationMetrics() calls for three derived metrics
            climateStationsMaxt <-  findStation(
              unitCode = "AGFO",
              distance = 30,
              climateParameters = list('maxt')
            )
            
            expect_type(
              suppressWarnings(getWxObservations(
                climateStations = climateStationsMaxt,
                climateParameters = list("avgt"),
                sdate = "1958-01-01",
                edate = "2018-12-31",
                duration = "dly",
                interval = "dly",
                normal = "departure",
                maxMissing = 10,
                metric = "CST8and9"
              )),
              "list"
            )
            expect_length(
              suppressWarnings(getWxObservations(
                climateStations = climateStationsMaxt,
                climateParameters = list("avgt"),
                sdate = "1958-01-01",
                edate = "2018-12-31",
                duration = "dly",
                interval = "dly",
                normal = "departure",
                maxMissing = 10,
                metric = "CST8and9"
              )),
              15
            )
            
            climateStationsPcpn <-  findStation(
              unitCode = "AGFO",
              distance = 30,
              climateParameters = list('pcpn')
            )
            
            expect_type(
              suppressWarnings(getWxObservations(
                climateStations = unique(climateStationsPcpn$uid),
                climateParameters = list("pcpn"),
                sdate = "1958-01-01",
                edate = "2018-12-31",
                duration = "yly",
                interval = "yly",
                reduceCodes = "run_le_0.01",
                maxMissing = 10,
                metric = "CSP3"
              )),
              "list"
            )
            expect_length(
              suppressWarnings(getWxObservations(
                climateStations = unique(climateStationsPcpn$uid),
                climateParameters = list("pcpn"),
                sdate = "1958-01-01",
                edate = "2018-12-31",
                duration = "yly",
                interval = "yly",
                reduceCodes = "run_le_0.01",
                maxMissing = 10,
                metric = "CSP3"
              )),
              16
            )
            
            expect_type(
              suppressWarnings(getWxObservations(
                climateStations = climateStationsPcpn,
                climateParameters = list("pcpn"),
                sdate = "1958-01-01",
                edate = "2018-12-31",
                duration = "dly",
                interval = "dly",
                normal = "departure",
                maxMissing = 10,
                metric = "CSP7and8"
              )),
              "list"
            )
            expect_length(
              suppressWarnings(getWxObservations(
                climateStations = climateStationsPcpn,
                climateParameters = list("pcpn"),
                sdate = "1958-01-01",
                edate = "2018-12-31",
                duration = "dly",
                interval = "dly",
                normal = "departure",
                maxMissing = 10,
                metric = "CSP7and8"
              )),
              15
            )
          })
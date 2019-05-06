library(testthat)
library(EnvironmentalSettingToolkit)

context("Tests for getWxObservationsDailyFlags()")
test_that("Tests for getWxObservationsDailyFlags()",
          {
            # Testing with single-station input generating a CSV
            stationsPcpn <-  list(31465) # Station from AGFO; approx 52k records
            suppressWarnings(
              getWxObservationsDailyFlags(
                climateStations = stationsPcpn,
                edate = "2018-12-31",
                duration = "dly",
                interval = "dly",
                filePathAndName = "DailyStationData_Test.csv"
              )
            )
            
            expect_type(
              suppressWarnings(df <- read.csv("DailyStationData_Test.csv")),
              "list"
            )
            expect_length(
              suppressWarnings(df <- read.csv("DailyStationData_Test.csv")),
              22
            )
            
            df <- read.csv("DailyStationData_Test.csv")
            uidColumn <- df[sample.int(NROW(df) , 1),]$uid
            
            expect_is(suppressWarnings(uidColumn),
                      "integer")
            
            
            dateColumn <-
              as.Date(df[sample.int(NROW(df) , 1),]$date)
            expect_is(suppressWarnings(dateColumn),
                      "Date")
          })
#' Get station data for specified parameter(s) and station(s)
#'
#' Using one or more stations (comma-delimied IDs or a list of IDs), requests weather observation data from NOAA Regional Climate Centers' Applied Climate Information System (ACIS) web services \url{http://www.rcc-acis.org/docs_webservices.html}. ACIS overview: \url{http://www.rcc-acis.org/index.html}  ACIS journal reference: \url{https://doi.org/10.1175/BAMS-D-13-00032.1}
#' 
#' 
#' Takes a list of one or more parameters and one or more unique station IDs, requests station data, and returns it as a data frame. Note: For monthly data, value vectors returned as character format to accommodate missing records ("M")
# @param dataURL URL for ACIS data service vending station data
#' @import jsonlite httr 
#' @importFrom utils write.table
#' @param climateStations A list of one or more unique identifiers (uid) for climate stations. Can be a single item, a list of items, or a data frame of the findStation response.
#' @param climateParameters A list of one or more climate parameters (e.g. pcpn, mint, maxt, avgt, obst, snow, snwd, gdd, hdd, cdd). If not specified, defaults to all parameters except degree days. See Table 3 on ACIS Web Services page: \url{http://www.rcc-acis.org/docs_webservices.html}
#' @param sdate (optional) Default is period of record ("por"). If specific start date is desired, format as a string (yyyy-mm-dd or yyyymmdd). The beginning of the desired date range.
#' @param edate (optional) Default is period of record ("por"). If specific end date is desired, format as a string (yyyy-mm-dd or yyyymmdd). The end of the desired date range.
#' @param duration (optional) Duration of summarization period. Default is daily ("dly"). Use "mly" for monthly or "yly" for yearly. If not "dly", must specify reduce_codes.
#' @param interval (optional) Time step for results. Default is daily ("dly"). Use "mly" for monthly or "yly" for yearly. If not "dly", must match duration value.
#' @param reduceCodes (optional) For monthly or yearly requests, a list of one or more reduce codes. If missing, defaults to min, max, sum, and mean. See Table 2 on ACIS Web Services page: \url{http://www.rcc-acis.org/docs_webservices.html}
#' @param maxMissing (optional) Maximum number of missing days within a month before the aggregate is not calculated (applied to each parameter). If missing, defaults to 1 (~3.3 percent missing days/month) except for protocol metric growing degree day requests which use 999.
#' @param filePathAndName (optional) File path and name including extension for output CSV file
#' @param normal (optional) 1 for the 30-year climate normal or "departure" for departure from 30-year climate normal. Normal period: 1981-2010.
#' @param metric (optional) A list of one or more climate metrics from the IMD Environmental Setting protocol
#' @return A data frame containing the requested data. Note: date vector is in character format, not date format. See User Guide for more details: \url{https://docs.google.com/document/d/1RKZ2ODNqmb0mYldFu1GivEdGwkDT9WwMEGe73vzmwQE}
#' @examples \dontrun{
#' Precipitation, temperature (daily) weather observations for one station for a specifc date range:
#'
#' getWxObservations(climateParameters=list('pcpn', 'avgt', 'obst', 'mint', 'maxt'), climateStations=25056, sdate="20150801", edate="20150831")
#'
#' The same request written to a CSV file:
#' 
#' getWxObservations(climateParameters=list('pcpn', 'avgt', 'obst', 'mint', 'maxt'), climateStations=25056, sdate="20150801", edate="20160831", filePathAndName = "D:\\temp\\trash\\dailyObs_station25056.csv")
#'  
#' All daily weather observations for a station for its period of record
#'
#' getWxObservations(climateStations=60903)
#' 
#' All monthly weather observations for two stations for a specified date range:
#' 
#' getWxObservations(climateStations = list(61193, 26215), sdate="201401", edate = "201501", duration="mly", maxMissing = NULL)
#'
#' Monthly weather observations for precipitation for a station from beginning of record through Sept 2016
#' 
#' getWxObservations(climateStations = list(26215), climateParameters = list('pcpn'), reduceCodes = list('min'), edate= "2016-09", duration="mly", maxMissing = 2)
#'
#' All daily weather observations for all stations (using a findStation response data frame: stationDF) for a specific date range:
#'
#' getWxObservations(climateParameters=list('pcpn', 'avgt', 'obst', 'mint', 'maxt'), climateStations=stationDF, sdate="20150801", edate="20150803")
#' }
#' @export
#' 
getWxObservations <-
  function(climateStations,
           climateParameters = NULL,
           sdate = "por",
           edate = "por",
           duration = "dly",
           interval = "dly",
           reduceCodes = NULL,
           maxMissing = NULL,
           filePathAndName = NULL,
           normal = NULL,
           metric = NULL) {
    # URLs and request parameters:
    # ACIS data services
    baseURL <- "http://data.rcc-acis.org/"
    webServiceSource <- "StnData"
    # Parameter flags: f = ACIS flag, s = source flag; only valid when requesting daily data
    #paramFlags <- c("f,s")
    # Reduce flags: mcnt = count of missing values in the reduction period; add date and run count missing for run summaries and limit to one value (count) 
    if (length(grep("run", reduceCodes)) > 0) {
      reduceFlags <- c("mcnt","date")
      reduceLimit <- 1
    }
    else {
      if (is.null(normal)) reduceFlags <- c("mcnt")
    }
    reduceList <- NULL
    # Interval and duration (TODO: add interval as function param in v1.7)
    if (!is.null(duration)) {
      interval <- duration
    }
    if (!is.null(interval)) {
      interval <- interval    
    }
    else {
      interval <- c("dly")
    }
    #duration <- c("mly")
    metaElements <-
      list('uid', 'll', 'name', 'elev', 'sids', 'state')
    lookups <-
      fromJSON(system.file("ACISLookups.json", package = "EnvironmentalSettingToolkit"),
               flatten = TRUE) # assumes placement in package inst subfolder
    luElements  <- lookups$element
    
    # If climateParameters is NULL, default to all parameters except degree days.
    if (is.null(climateParameters)) {
      climateParameters0 <- lookups$element$code
      # Remove degree days (v1.5); super cheesy... fix at some point
      climateParameters <- climateParameters0[1:7]
      #climateParameters <- list('pcpn', 'mint', 'maxt', 'avgt', 'obst', 'snow', 'snwd')
    }
    if ((duration == "mly" || duration == "yly") || (interval == "mly" || interval == "yly")) {
      # If reduceCodes is NULL, default to min, max, sum, and mean.
      if (is.null(reduceCodes) && is.null(normal)) {
        reduceCodes <- list('min', 'max', 'sum', 'mean')
      }
      if (is.null(normal)) {
        reduceList <- vector('list', length(reduceCodes))
        
        for (j in 1:length(reduceCodes)) {
          r <- list(reduce = unlist(reduceCodes[j]), add = reduceFlags)
          reduceList[[j]] <- r #unlist(c(r))
        }
      }
      # If maxMissing is NULL, default to 1 (~3.3% missing days/month).
      if (is.null(maxMissing)) {
        if (length(grep("cnt", reduceCodes)) > 0) {
          # 'Force' return of cnt and mcnt
          maxMissing <- 999
        }
        else {
          maxMissing <- 1
        }
      }
    }
    
    # Initialize response object
    dfResponse <- NULL
    
    # Format incoming arguments
    dataURL <-  gsub(" ", "", paste(baseURL, webServiceSource))
    climateElems <- paste(climateParameters, collapse = ",")
    paramCount <- length(climateParameters)
    
    # Iterate for each station
    if (is.data.frame(climateStations)) {
      listStations = as.list(climateStations$uid)
    }
    else if (is.list(climateStations)) {
      listStations = climateStations
    }
    else {
      listStations = as.list(climateStations)
    }
    for (s in 1:length(listStations)) {
      df <- NULL
      cUid <- unlist(listStations[s])
      body <- formatRequest(requestType = "getWxObservations", climateParameters = climateParameters, sdate, edate, cUid, duration = duration, interval = interval, reduceList = reduceList, maxMissing = maxMissing, normal = normal)
      
      # This returns the full response - need to use content() and parse
      # content(dataResponseInit) results in a list lacking column names but containing data which needs to be
      # converted to dataFrame with appropriate vectors
      dataResponseInit <-
        POST(
          "http://data.rcc-acis.org/StnData",
          accept_json(),
          add_headers("Content-Type" = "application/json"),
          body = body,
          verbose()
        )
      
      if (grepl("data", content(dataResponseInit, "text")) == FALSE) {
        dfResponse <- content(dataResponseInit, "text")
      }
      else {
        # Format climate data object
        rList <- content(dataResponseInit)
        dataResponseError <- rList$error
        if (is.null(dataResponseError)) {
          df <-
            formatWxObservations(
              rList,
              duration = duration,
              climateParameters = climateParameters,
              reduceCodes = reduceCodes,
              luElements = luElements,
              normal = normal,
              metric = metric 
            )
          # Create output object
          if (is.data.frame(dfResponse)) {
            dfResponse <- rbind(dfResponse, df)
          }
          else {
            dfResponse <- df
          }
        }
        else {
          dfResponse <- dataResponseError
        }
      }
    }
    
    # Output file
    if (!is.null(filePathAndName)) {
      write.table(
        dfResponse,
        file = filePathAndName,
        sep = ",",
        row.names = FALSE,
        qmethod = "double"
      )
    }
    else {
      return (dfResponse)
    }
    return (dfResponse)
  }



#' Get daily data with flags for specified parameter(s) and station(s)
#'
#' Using one or more stations (comma-delimited IDs or a list of IDs), requests daily weather observation data with flags from NOAA Regional Climate Centers' Applied Climate Information System (ACIS) web services \url{http://www.rcc-acis.org/docs_webservices.html}. ACIS overview: \url{http://www.rcc-acis.org/index.html}  ACIS journal reference: \url{https://doi.org/10.1175/BAMS-D-13-00032.1}
#' 
#' 
#' Takes a list of one or more parameters and one or more unique station IDs, requests daily station data for each climate parameter with flags, and returns it as a data frame. Note: Value vectors returned as character format to accommodate missing records ("M")
# @param dataURL URL for ACIS data service vending station data
#' @import jsonlite httr 
#' @importFrom utils write.table
#' @param climateStations A list of one or more unique identifiers (uid) for climate stations. Can be a single item, a list of items, or a data frame of the findStation response.
#' @param climateParameters A list of one or more climate parameters (e.g. pcpn, mint, maxt, avgt, snow, snwd, gdd, hdd, cdd). If not specified, defaults to all parameters except degree days. See Table 3 on ACIS Web Services page: \url{http://www.rcc-acis.org/docs_webservices.html}
#' @param sdate (optional) Default is period of record ("por"). If specific start date is desired, format as a string (yyyy-mm-dd or yyyymmdd). The beginning of the desired date range.
#' @param edate (optional) Default is period of record ("por"). If specific end date is desired, format as a string (yyyy-mm-dd or yyyymmdd). The end of the desired date range.
#' @param duration (optional) Duration of summarization period. Default is daily ("dly"). 
#' @param interval (optional) Time step for results. Default is daily ("dly"). 
#' @param filePathAndName (optional) File path and name including extension for output CSV file
#' @return A data frame containing the requested data or that data frame written to a CSV file. Note: date vector is in character format, not date format. See User Guide for more details: \url{https://docs.google.com/document/d/1RKZ2ODNqmb0mYldFu1GivEdGwkDT9WwMEGe73vzmwQE}
#' @examples \dontrun{
#' Precipitation, temperature weather observations for one station for a specifc date range:
#'
#' getWxObservations(climateParameters=list('pcpn', 'avgt', 'obst', 'mint', 'maxt'), climateStations=25056, sdate="20150801", edate="20150831")
#'
#' The same request written to a CSV file:
#' 
#' getWxObservations(climateParameters=list('pcpn', 'avgt', 'obst', 'mint', 'maxt'), climateStations=25056, sdate="20150801", edate="20160831", filePathAndName = "D:\\temp\\trash\\dailyObs_station25056.csv")
#'  
#' All daily weather observations for a station for its period of record
#'
#' getWxObservations(climateStations=60903)
#' 
#' All monthly weather observations for two stations for a specified date range:
#' 
#' getWxObservations(climateStations = list(61193, 26215), sdate="201401", edate = "201501", duration="mly", maxMissing = NULL)
#'
#' Monthly weather observations for precipitation for a station from beginning of record through Sept 2016
#' 
#' getWxObservations(climateStations = list(26215), climateParameters = list('pcpn'), reduceCodes = list('min'), edate= "2016-09", duration="mly", maxMissing = 2)
#'
#' All daily weather observations for all stations (using a findStation response data frame: stationDF) for a specific date range:
#'
#' getWxObservations(climateParameters=list('pcpn', 'avgt', 'obst', 'mint', 'maxt'), climateStations=stationDF, sdate="20150801", edate="20150803")
#' }
#' @export
#' 
getWxObservationsDailyFlags <-
  function(climateStations,
           climateParameters = NULL,
           sdate = "por",
           edate = "por",
           duration = "dly",
           interval = "dly",
           filePathAndName = NULL) {
    # URLs and request parameters:
    # ACIS data services
    baseURL <- "http://data.rcc-acis.org/"
    webServiceSource <- "StnData"
    # Parameter flags: i = reporting station id, f = ACIS flag, s = source flag, n = reporting network, v = var minor; only valid when requesting daily data
    paramFlags <- c("i,t,f,s,n,v")
    
    metaElements <-
      list('uid', 'll', 'name', 'elev', 'sids', 'state')
    lookups <-
      fromJSON(
        system.file("ACISLookups.json", package = "EnvironmentalSettingToolkit"),
        flatten = TRUE
      ) # assumes placement in package inst subfolder
    luElements  <- lookups$element
    
    climateParameters0 <- lookups$element$code
    climateParameters <-
      climateParameters0[climateParameters0 %in% "obst" == FALSE] # remove obst
    #climateParameters <- list(''mint', 'maxt', 'avgt', pcpn', 'snow', 'snwd', 'gdd', 'cdd', 'hdd')
    
    # Initialize response object
    dfResponse <- NULL
    
    # Format incoming arguments
    dataURL <-  gsub(" ", "", paste(baseURL, webServiceSource))
    climateElems <- paste(climateParameters, collapse = ",")
    paramCount <- length(climateParameters)
    
    # Iterate for each station
    if (is.data.frame(climateStations)) {
      listStations = as.list(climateStations$uid)
    }
    else if (is.list(climateStations)) {
      listStations = climateStations
    }
    else {
      listStations = as.list(climateStations)
    }
    for (s in 1:length(listStations)) {
      df <- NULL
      cUid <- unlist(listStations[s])
      lapply(climateParameters, function(x) {
        body <-
          formatRequest(
            requestType = "getWxObservations",
            climateParameters = x,
            "POR",
            edate,
            cUid,
            duration = duration,
            interval = interval,
            paramFlags = paramFlags
          )
        
        # This returns the full response - need to use content() and parse
        # content(dataResponseInit) results in a list lacking column names but containing data which needs to be
        # converted to dataFrame with appropriate vectors
        dataResponseInit <-
          POST(
            "http://data.rcc-acis.org/StnData",
            accept_json(),
            add_headers("Content-Type" = "application/json"),
            body = body,
            verbose()
          )
        
        if (grepl("data", content(dataResponseInit, "text")) == FALSE) {
          dfResponse <- content(dataResponseInit, "text")
        }
        else {
          # Format climate data object
          rList <- content(dataResponseInit)
          dataResponseError <- rList$error
          if (is.null(dataResponseError)) {
            df <-
              formatWxObservationsDailyFlags(
                rList,
                duration = duration,
                climateParameters = x,
                luElements = luElements
              )
            # Create output object
            if (is.data.frame(dfResponse)) {
              dfResponse <- rbind(dfResponse, df)
            }
            else {
              dfResponse <- df
            }
          }
          else {
            dfResponse <- dataResponseError
          }
        }
        # Output file
        if (!is.null(filePathAndName)) {
          fName <-
            gsub("<eyear>",
                 format(as.Date(edate, format = "%Y-%m-%d"), "%Y"),
                 gsub(
                   "<StationUID>",
                   cUid,
                   gsub(
                     "<runDate>",
                     format(Sys.Date(), "%Y%m%d"),
                     filePathAndName
                   )
                 ))
          if (!file.exists(fName)) {
            write.table(
              dfResponse,
              file = fName,
              append = TRUE,
              sep = ",",
              row.names = FALSE,
              qmethod = "double"
            )
          }
          else {
            write.table(
              dfResponse,
              file = fName,
              append = TRUE,
              sep = ",",
              row.names = FALSE,
              col.names = FALSE,
              qmethod = "double"
            )
          }
        }
        else {
          return (dfResponse)
        }
      }) # end lapply climateParameters
    }
    return (dfResponse)
  }
    

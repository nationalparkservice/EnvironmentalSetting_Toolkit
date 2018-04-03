#' Utility functions for EnvironmentalSetting_Toolkit
#'
#' getStationType function uses the station identifier type code to lookup the station type description
#' @param testType station identifier type code
#' @param testSid first three characters of the station identifier
#' @export
#'

getStationSubtype <- function(testType, testSid) {
  # ACIS lookup
  acisLookup <-
    fromJSON(system.file("ACISLookups.json", package = "EnvironmentalSettingToolkit")) # assumes placement in package inst subfolder
  typeDesc <-
    acisLookup$stationIdType$description[acisLookup$stationIdType$code == testType]
  subtypeDesc <- NULL
  # If subtypes exist for station type, find matching subtype
  if (!is.na(testType)) {
    if (!acisLookup$stationIdType$subtypes[acisLookup$stationIdType$code == testType] == "") {
      subtypes <-
        unlist(acisLookup$stationIdType$subtypes[acisLookup$stationIdType$code == testType])
      if (!length(subtypes) == 0 && !subtypes == "") {
        if (!is.na(names(strsplit(
          unlist(acisLookup$stationIdType$subtypes[acisLookup$stationIdType$code == testType])[testSid], '\n'
        )[testSid]))) {
          tempdf <-
            as.data.frame(strsplit(unlist(
              acisLookup$stationIdType$subtypes[acisLookup$stationIdType$code == testType]
            )[testSid], '\n')[testSid])
          typeDesc <- as.character(as.vector(tempdf[1, ]))
        }
      }
    }
  }
  
  return(typeDesc)
}

#' formatRequest generates the JSON-formatted request to send to ACIS
#' 
#' Requests are made via GET (findStation) or POST (getWxObservations, getGrids)
#' 
#' 
#' @param requestType type of request: getDailyWxObservations, getMonthlyWxObservations, getGrids, (findStation)
#' @param climateParameters A list of one or more climate parameters defined in calling source
#' @param sdate sdate (required) Start data defined in calling source
#' @param edate sdate (required) End date defined in calling source
#' @param cUid (optional) station UID defined in calling source, used for getWXObservation requests
#' @param duration (optional) station data duration specified in calling source; used for getWxObservations and getGrids requests
#' @param interval (optional) time interval for results specified in calling source; used for getWxObservations and getGrids requests
#' @param paramFlags (optional) used for getWxObservations (daily). Parameter flags: f = ACIS flag, s = source flag
#' @param reduceList (optional) used for getWxObservations (monthly). Defaults to min, max, sum, and mean.
#' @param maxMissing (optional) used for getWxObservations (monthly). Defaults to 1 which is approx 3.3 missing days per month.
#' @param normal (optional) 1 = 30-year climate normal or "departure" for departure from 30-year climate normal. Normal period: 1981-2010.
#' @param gridElements grid request values defined in calling source
#' @export
#' 
formatRequest <- function(requestType, climateParameters, sdate, edate, cUid=NULL, duration=NULL, interval=NULL, paramFlags=NULL, reduceList=NULL, maxMissing=NULL, normal=NULL, gridElements=NULL) {
  #print(requestType)
  
  # Hard-coded request elements
  # Parameter flags: f = ACIS flag, s = source flag
  paramFlags <- c("f,s")
  # Metadata elements (not used explicitly)
  metaElements <-
    list('uid', 'll', 'name', 'elev', 'sids', 'state')
  # # Used for grid requests
  # gridElements <-
  #   list(
  #     #interval = "dly",
  #     duration = duration,
  #     gridSource = "PRISM",
  #     dataPrecision = 1,
  #     output = "json",
  #     meta = "ll"
  #   )
  # gridElements <- c(gridElements, grid = luElements[[1]]$code)
  # Reduce flags: mcnt = count of missing values in the reduction period
  #reduceFlags <- c("mcnt")
  paramCount <- length(climateParameters)
  if (!is.null(reduceList)) {
    reduceCount <- length(reduceList)
  }
  if (length(grep("gdd", climateParameters)) > 0) {
    gddBase <- 32
  }
  # List of elements
  eList <- NULL
  
  # Build request
  if (requestType == "getWxObservations") {
    # Build elems list
    if ((duration == "mly" || duration == "yly") || (interval == "mly" || interval == "yly")) {
      if(is.null(normal)) eList <- vector('list', paramCount*reduceCount)
      else eList <- vector('list', paramCount)
      counter <- 1
      # Iterate parameter list to create elems element:
      for (i in 1:paramCount) {
        if (is.null(normal)) {
          for (j in 1:reduceCount) {
            #listJ, listI
            if (is.null(interval)) {
              if (length(grep("gdd", climateParameters)) > 0) {
                e <-
                  list(
                    name = unlist(c(climateParameters[i])),
                    base = gddBase,
                    interval = duration,
                    #"dly",#interval,
                    duration = duration,
                    reduce = c(reduceList[j]),
                    maxmissing = maxMissing #unlist(mmElem)
                  )
              }
              else {
                e <-
                  list(
                    name = unlist(c(climateParameters[i])),
                    interval = duration,
                    #"dly",#interval,
                    duration = duration,
                    reduce = c(reduceList[j]),
                    maxmissing = maxMissing #unlist(mmElem)
                  )
              }
            }
            else {
              if (length(grep("gdd", climateParameters)) > 0) {
                e <-
                  list(
                    name = unlist(c(climateParameters[i])),
                    base = gddBase,
                    interval = interval,
                    duration = duration,
                    reduce = c(reduceList[j]),
                    maxmissing = maxMissing #unlist(mmElem)
                  )
              }
              else {
                e <-
                  list(
                    name = unlist(c(climateParameters[i])),
                    interval = interval,
                    duration = duration,
                    reduce = c(reduceList[j]),
                    maxmissing = maxMissing #unlist(mmElem)
                  )
              }
            }
            eList[[counter]] <- e
            counter <- counter + 1
          }
        }
        else {
          if (is.null(interval)) {
              e <-
                list(
                  name = unlist(c(climateParameters[i])),
                  interval = duration,
                  duration = duration,
                  normal = normal
                  #maxmissing = maxMissing #unlist(mmElem)
                )
              eList[[i]] <- e
            }
            else {
              e <-
                list(
                  name = unlist(c(climateParameters[i])),
                  interval = interval,
                  duration = duration,
                  normal = normal
                  #maxmissing = maxMissing #unlist(mmElem)
                )
              eList[[i]] <- e
            }
          }
        }
      } # mly or yly
    else {
      eList <- vector('list', paramCount)
      # Iterate parameter list to create elems element:
      
      for (i in 1:paramCount) {
        if (is.null(normal)) {
          e <- list(name = unlist(c(climateParameters[i])), add = paramFlags)
          #print(e)
          eList[[i]] <- e
        }
        else {
          e <-
            list(
              name = unlist(c(climateParameters[i])),
              interval = interval,
              duration = duration,
              normal = normal
              #maxmissing = maxMissing #unlist(mmElem)
            )
          eList[[i]] <- e
        }
      }
    }

    # Climate parameters as JSON with flags
    elems <- toJSON(eList, auto_unbox = TRUE)
    # Build body (bList)
    bList <-
      list(
        uid = cUid,
        sdate = sdate,
        edate = edate,
        elems = elems
      )
    reqBody  <- stripEscapes(bList)
  }
  else if ("getGrids") {
    # Build elems list
    # Iterate parameter list to create elems element:
    # Iterate parameter list to create elems element:
    eList <- vector('list', paramCount)
    for (i in 1:paramCount) {
      if (is.null(interval)) {
        e <-
          list(
            name = unlist(c(climateParameters[i])),
            #interval = gridElements$interval,
            duration = gridElements$duration,
            prec = gridElements$dataPrecision
          )
      }
      else {
        e <-
          list(
            name = unlist(c(climateParameters[i])),
            interval = gridElements$interval,
            duration = gridElements$duration,
            prec = gridElements$dataPrecision
          ) 
      }
      #print(e)
      eList[[i]] <- e
    }
    # Climate parameters as JSON with flags
    elems <- toJSON(eList, auto_unbox = TRUE)
    # Build body (bList)
    bList <- c(bList, output = unlist(gridElements$output))
    bList <- c(bList, grid = unlist(gridElements$grid))
    bList <- c(bList, meta = unlist(gridElements$meta))
    
    reqBody  <- stripEscapes(c(bList, elems = elems))
  }
  # else { # placeholder for findStation
  #   # Build elems list
  #   # Iterate parameter list to create elems element:
  #   eList <- NULL
  #   
  #   # Build body (bList)
  # }
  
  # Return request JSON
  return(reqBody)
}

#' formatWxObservations converts get*WxObservation response to a data frame, iterating by date and value
#' @param responseContent list of response arrays containing name/value pairs: meta (default), data (date, values) 
#' @param duration station data duration specified in calling source 
#' @param climateParameters A list of one or more climate parameters defined in calling source
#' @param reduceCodes A list of one or more reduce codes defined in calling source
#' @param luElements lookup values defined in calling source
#' @param metric (optional) Metric code for IMD Environmental
#' @param normal (optional) 1 = 30-year climate normal or "departure" for departure from 30-year climate normal. Normal period: 1981-2010.
#' @export
#'
formatWxObservations  <- function(rList, duration, climateParameters, reduceCodes, luElements, normal, metric) {
  # Initialize return object (table or dataFrame)
  df <- NULL
  dfResponse <- NULL
  # Initialize vectors for SID type
  sid1_type = c()
  sid2_type = c()
  sid3_type = c()
  
  # R does not support the YYYY-MM format for a Date object. Hence, for monthly observations,
  # the date vector is in character format
  dfDate <-
    as.data.frame(cbind(unlist(lapply(
      rList$data, "[", 1
    )[][])))
  colnames(dfDate) <- "date"
  if(duration == "dly") {
    dfDate$date <- as.Date(as.character(as.vector(dfDate$date)), "%Y-%m-%d")
  }
  else {
    dfDate$date <- as.character(as.vector(dfDate$date))
  }
  
  # Populate 'metadata' i.e. station info; accommodate missing and unordered items
  dfMetaInit <-  t(as.data.frame(rList$meta))
  dfMeta <-
    as.data.frame(as.character(as.vector(strsplit(
      dfMetaInit[, 1], " "
    )$uid)))
  #dfMeta <- as.data.frame(as.character(as.vector(dfMetaInit[1, ])))
  colnames(dfMeta)[1]  <- "uid"
  #dfMeta$uid <- as.numeric(dfMeta$uid)
  
  dfMeta  <-
    cbind(dfMeta, as.data.frame(as.character(as.vector(
      paste(strsplit(dfMetaInit[, 1], " ")$name, collapse = " ")
    ))))
  colnames(dfMeta)[2]  <- "name"
  dfMeta  <-
    cbind(dfMeta, as.data.frame(as.numeric(as.vector(
      rList$meta$ll[1]
    ))))
  #dfMeta  <- cbind(dfMeta, as.data.frame(as.numeric(as.vector(dfMetaInit[2, ]))))
  colnames(dfMeta)[3]  <- "longitude"
  dfMeta  <-
    cbind(dfMeta, as.data.frame(as.numeric(as.vector(
      rList$meta$ll[2]
    ))))
  #dfMeta  <- cbind(dfMeta, as.data.frame(as.numeric(as.vector(dfMetaInit[3, ]))))
  colnames(dfMeta)[4]  <- "latitude"
  
  # Assumes sids element contains 3 members (even if 2 are empty)
  # Suppress warnings from getStationSubtype(): raised due to conversion necessary because data.frame vector access technique does not recognize column name
  dfMeta  <-
    cbind(dfMeta, as.data.frame(as.character(as.vector(
      rList$meta$sids[1]
    ))))
  #dfMeta  <- cbind(dfMeta, as.data.frame(as.character(as.vector(dfMetaInit[4, ]))))
  colnames(dfMeta)[5]  <- "sid1"
  dfMeta$sid1  <- as.character(dfMeta$sid1)
  sid1_type <-
    suppressWarnings(getStationSubtype(unlist(strsplit(
      unlist(dfMeta$sid1), " "
    ))[2], substr(unlist(
      strsplit(unlist(dfMeta$sid1), " ")
    )[1], 1, 3)))
  dfMeta  <-
    cbind(dfMeta, as.data.frame(as.character(as.vector(sid1_type))))
  colnames(dfMeta)[6]  <- "sid1_type"
  # Check if SIDS 3 is present (dim 9,1 or dim 10,1 - 4th SID is ignored)
  if (identical(dim(dfMetaInit), as.integer(c(9, 1))) || identical(dim(dfMetaInit), as.integer(c(10, 1)))) {
    dfMeta  <-
      cbind(dfMeta, as.data.frame(as.character(as.vector(
        dfMetaInit[5,]
      ))))
    colnames(dfMeta)[7]  <- "sid2"
    dfMeta$sid2  <- as.character(dfMeta$sid2)
    sid2_type <-
      suppressWarnings(getStationSubtype(unlist(strsplit(
        unlist(dfMeta$sid2), " "
      ))[2], substr(unlist(
        strsplit(unlist(dfMeta$sid2), " ")
      )[1], 1, 3)))
    dfMeta  <-
      cbind(dfMeta, as.data.frame(as.character(as.vector(sid2_type))))
    colnames(dfMeta)[8]  <- "sid2_type"
    dfMeta  <-
      cbind(dfMeta, as.data.frame(as.character(as.vector(
        dfMetaInit[6,]
      ))))
    colnames(dfMeta)[9]  <- "sid3"
    dfMeta$sid3  <- as.character(dfMeta$sid3)
    sid3_type <-
      suppressWarnings(getStationSubtype(unlist(strsplit(
        unlist(dfMeta$sid3), " "
      ))[2], substr(unlist(
        strsplit(unlist(dfMeta$sid3), " ")
      )[1], 1, 3)))
    dfMeta  <-
      cbind(dfMeta, as.data.frame(as.character(as.vector(sid3_type))))
    colnames(dfMeta)[10]  <- "sid3_type"
  }
  else {
    # missing one or more sid elements; first case is sid1 and sid2, no sid3
    if (identical(dim(dfMetaInit), as.integer(c(8, 1)))) {
      dfMeta  <-
        cbind(dfMeta, as.data.frame(as.character(as.vector(
          dfMetaInit[4,]
          #cbind(dfMeta, as.data.frame(as.character(as.vector(
          #dfMetaInit[5,]
        ))))
      colnames(dfMeta)[7]  <- "sid2"
      dfMeta$sid2  <- as.character(dfMeta$sid2)
      sid2_type <-
        suppressWarnings(getStationSubtype(unlist(strsplit(
          unlist(dfMeta$sid2), " "
        ))[2], substr(unlist(
          strsplit(unlist(dfMeta$sid2), " ")
        )[1], 1, 3)))
      dfMeta  <-
        cbind(dfMeta, as.data.frame(as.character(as.vector(
          sid2_type
        ))))
      colnames(dfMeta)[8]  <- "sid2_type"
    }
    else {
      # no sid2 value
      dfMeta  <- cbind(dfMeta, as.data.frame(NA))
      dfMeta[7] <- setNames(dfMeta[7],"sid2")
      colnames(dfMeta)[7]  <- "sid2"
      sid2_type <-  as.data.frame(NA)
      #dfMeta  <- cbind(dfMeta, as.character(sid2_type))
      dfMeta  <-
        cbind(dfMeta, as.data.frame(as.character(as.vector(
          sid2_type
        ))))
      colnames(dfMeta)[8]  <- "sid2_type"
    } # no sid3 value
    dfMeta  <- cbind(dfMeta, as.data.frame(NA))
    colnames(dfMeta)[9]  <- "sid3"
    #dfMeta$sid3  <- as.character(dfMeta$sid3)
    sid3_type <-  as.data.frame(NA)
    dfMeta  <-
      cbind(dfMeta, as.data.frame(as.character(as.vector(sid3_type))))
    colnames(dfMeta)[10]  <- "sid3_type"
  }
  
  if (!is.null(strsplit(dfMetaInit[, 1], " ")$state) && grepl("^[A-Za-z]+$",strsplit(dfMetaInit[, 1], " ")$state) == TRUE) {
    dfMeta  <-
      cbind(dfMeta, as.data.frame(as.character(as.vector(
        strsplit(dfMetaInit[, 1], " ")$state
      ))))
  }
  else {
    dfMeta  <-
      cbind(dfMeta, as.data.frame(NA))
  }
  colnames(dfMeta)[11]  <- "state"
  
  if (!is.null(strsplit(dfMetaInit[, 1], " ")$elev)) {
    dfMeta  <-
      cbind(dfMeta, as.data.frame(as.numeric(as.vector(
        strsplit(dfMetaInit[, 1], " ")$elev
      ))))
  }
  else {
    dfMeta  <-
      cbind(dfMeta, as.data.frame(NA))
  }
  colnames(dfMeta)[12]  <- "elev"
  df$elev <- as.numeric(df$elev)
  
  # For whatever reason, this conversion has to be re-forced again!
  dfMeta$uid <- as.numeric(as.character(dfMeta$uid))
  options(digits = 1)
  dfMeta$elev <- as.numeric(as.character(dfMeta$elev))
  options(digits = 7)
  df <- cbind(dfMeta, dfDate)
  
  # Add the paramter vectors - thanks for the matrix suggestion, Tom!!
  # For monthly data, value vector returned as character to accommodate missing records ("M")
  # Get parameter units from lookup file
  # rangeBase subtraction forces ignoring of first 'column' which is date
  rangeBase <- length(rList$data[[1]]) - 1
  #if(duration == 'dly') {range <- rangeBase - 1}
  #else {range <- rangeBase}
  #print(rangeBase)
  itemCount <- 1
  #for (i in 2:(length(rList$data[[1]])) - 1)  {
  for (i in 1:rangeBase)  {  
    #  == count of parameters
    vUnit <-
      luElements[which(luElements$code == climateParameters[i]),]$unitabbr
    
    if(duration == 'dly') {
      fName <- paste(climateParameters[i], "acis_flag", sep = "_")
      if (is.null(normal)) {
        vName <- paste(climateParameters[i], vUnit, sep = "_")
        valueArray <-
          matrix(unlist(lapply(rList$data, "[", i + 1)), ncol = 3, byrow = TRUE)[, 1]
        flagArray <-
          matrix(unlist(lapply(rList$data, "[", i + 1)), ncol = 3, byrow = TRUE)[, 2]
        sName <-
          paste(climateParameters[i], "source_flag", sep = "_")
        sourceFlagArray <-
          matrix(unlist(lapply(rList$data, "[", i + 1)), ncol = 3, byrow = TRUE)[, 3]
        df[[vName]] <- as.numeric(valueArray)
        df[[fName]] <-
          as.character(replace(flagArray, flagArray == " ", NA))
        df[[sName]] <-
          as.character(replace(sourceFlagArray, sourceFlagArray == " ", NA))
      }
      else {
        if (normal == 1)
          vValue = paste("normal", vUnit, sep = "_")
        else
          vValue = paste("departure", vUnit, sep = "_")
        vName <- paste(climateParameters[i], vValue, sep = "_")
        valueArray <-
          unlist(lapply(matrix(
            lapply(rList$data, "[[", 2), ncol = 1, byrow = TRUE
          )[, 1], "[", 1))
        
        df[[vName]] <- as.numeric(valueArray)
        itemCount <- itemCount + 1
      }
    }
    else { #mly or yly
      if (is.null(normal)) {
        for (j in 1:length(reduceCodes)) {
          vReduce <- unlist(reduceCodes[j])
          if (length(grep("run", reduceCodes[j])) > 0) {
            rName <-
              paste(paste(climateParameters[i], vUnit, sep = "_"), "run", sep = "_")
            yName <-
              paste(paste(climateParameters[i], vUnit, sep = "_"),
                    "runYear",
                    sep = "_")
            vName <-
              paste(paste(climateParameters[i], vUnit, sep = "_"), vReduce, sep = "_")
            dName <-
              paste(paste(climateParameters[i], vUnit, sep = "_"),
                    "runEndDate",
                    sep = "_")
            fName <-
              paste(paste(climateParameters[i], vUnit, sep = "_"),
                    "countMissing",
                    sep = "_")
          }
          else {
            vName <-
              paste(paste(climateParameters[i], vUnit, sep = "_"), vReduce, sep = "_")
            fName <- paste(vName, "countMissing", sep = "_")
            #valueArray <-
            #  matrix(unlist(lapply(rList$data, "[", i + 1)), ncol = 2, byrow = TRUE)[, 1]
          }
          #if (itemCount<=(rangeBase-1)) {
          if (itemCount <= rangeBase) {
            if (length(grep("run", reduceCodes)) > 0) {
              for (k in 1:length(lapply(rList$data, "[[", 1))) {
                # For each run year, compose run count and date columns
                yearArray <-
                  matrix(lapply(rList$data, "[[", 1)[[k]][[1]][[1]], ncol = 1)[, 1]
                #unlist(matrix(lapply(rList$data, "[[", 1), ncol = 1, byrow = TRUE)[,1])
                #matrix(lapply(rList$data, "[[", 1)[[1]][[1]][[itemCount]], ncol = 1)[,1]
                valueArray <-
                  unlist(lapply(matrix(
                    lapply(rList$data, "[[", 2)[[k]][[1]],
                    ncol = 1,
                    byrow = TRUE
                  )[, 1], "[", 1))
                dateArray <-
                  unlist(lapply(matrix(
                    lapply(rList$data, "[[", 2)[[k]][[1]],
                    ncol = 1,
                    byrow = TRUE
                  )[, 1], "[", 2))
                missingArray <-
                  matrix(lapply(rList$data, "[[", 2)[[k]], ncol = 1)[, 1][2]
                #unlist(sapply(matrix(lapply(rList$data, "[[", 2), ncol = 1, byrow = TRUE)[,1], "[", 2))
                df[[yName]][k] <- yearArray
                dfRun <-
                  cbind(as.numeric(valueArray), dName = dateArray)
                colnames(dfRun)[1] <- vName
                colnames(dfRun)[2] <- dName
                df[[rName]][k] <- list(dfRun)
                colnames(df)[15] <- rName
                # For missing count by year data, missing vector returned as character to accommodate missing records ("NA")
                df[[fName]][k] <-
                  as.character(replace(missingArray, missingArray == " ", NA))
              }
              itemCount <- itemCount + 1
            }
            else {
              valueArray <-
                matrix(unlist(lapply(rList$data, "[", itemCount + 1)), ncol = 2, byrow = TRUE)[, 1]
              flagArray <-
                matrix(unlist(lapply(rList$data, "[", itemCount + 1)), ncol = 2, byrow = TRUE)[, 2]
              # For monthly data, value vector returned as character to accommodate missing records ("M")
              df[[vName]] <- as.numeric(valueArray)#valueArray#as.numeric(valueArray)
              df[[fName]] <-
                as.character(replace(flagArray, flagArray == " ", NA))
              itemCount <- itemCount + 1
            }
          }
        }
      }
      else { # !is.null(normal)
        if (normal == 1)
          vValue = paste("normal", vUnit, sep = "_")
        else
          vValue = paste("departure", vUnit, sep = "_")
        vName <- paste(climateParameters[i], vValue, sep = "_")
        valueArray <-
          unlist(lapply(matrix(
            lapply(rList$data, "[[", 2), ncol = 1, byrow = TRUE
          )[, 1], "[", 1))
        
        df[[vName]] <- as.numeric(valueArray)
        itemCount <- itemCount + 1
      }
    }
    if(!is.null(metric)) {
      df$metric <- metric
    }
  }
  
  # Convert factors and booleans to character vectors
  fc  <- sapply(df, is.factor)
  lc <- sapply(df, is.logical)
  df[, fc]  <- sapply(df[, fc], as.character)
  df[, lc]  <- sapply(df[, lc], as.character)
  
  # Create output object
  if (is.data.frame(dfResponse)) {
    dfResponse <- rbind(dfResponse, df)
  }
  else {
    dfResponse <- df
  }
  return(dfResponse)
}

#' stripEscapes strips escape characters from input string
#' @param inputStr input from which escape characters are to be stripped
#' @export
#'

stripEscapes <- function(inputStr) {
  # Yes, this is crappy code but it works
  # TODO: Clean this up!!!
  iJSON <- gsub("\\\\", "T", toJSON(inputStr, auto_unbox = TRUE))
  f = gsub("\"\\[", "\\[", iJSON)
  g = gsub("\\]\"", "\\]", f)
  h = gsub("T", "", g)
  i = gsub("\"\"\\{", "\\{", h)
  outputJSON <- gsub("\\}\"\"", "\\}", i)
  
  return(outputJSON)
}

#' stripEscapesGrid strips escape characters from input string (used to format getDailyGrids response)
#' @param inputStr input from which escape characters are to be stripped
#' @export
#'

stripEscapesGrid <- function(inputStr) {
  # Yes, this is crappy code but it works
  # TODO: Clean this up!!!
  dd1  <- gsub("\"\\[", "\\[", inputStr)
  dd2  <- gsub("\\\\", "T", dd1)
  dd3  <- gsub("Tn", "", dd2)
  outputJSON  <- gsub("T", "", dd3)
  
  return(outputJSON)
}

#' getBBox retrieves bounding box from IRMA Unit service and buffers it by specified distance
#' 
#' @param unitCode unitCode One NPS or FWS unit code as a string
#' @param bboxExpand buffer distance in decimal degrees (assumes WGS84)
#' @export
#'
getBBox <- function (unitCode, expandBBox, bboxCustom=NULL) {
  
  if (is.null(bboxCustom)) {
    # NPS codes are alphabetic; FWS codes are alphanumeric
    if (grepl('^[A-Za-z]+$', unitCode) == TRUE) {
      bboxURLBase <-
        "http://irmaservices.nps.gov/v2/rest/unit/CODE/geography?detail=envelope&dataformat=wkt&format=json"
    }
    else {
      bboxURLBase <-
        "https://ecos.fws.gov/ServCatServices/v2/rest/unit/CODE/geography?detail=envelope&dataformat=wkt&format=json"
    }
    config <- add_headers(Accept = "'Accept':'application/json'")
    # Get bounding box for park(s)
    bboxURL <- gsub("CODE", unitCode, bboxURLBase)
    # Counter-clockwise vertices (as WKT): LL, LR, UR, UL
    bboxWKT <-
      strsplit(content(GET(bboxURL, config))[[1]]$Geography, ",")
    # Extract vertices and 'buffer' by bboxExpand distance or default of 0.011 degrees (~33 km)
    # TODO: add Eastern Hemisphere detection
    LL <- strsplit(substring(bboxWKT[[1]][1], 11), " ")
    LR <- strsplit(substring(bboxWKT[[1]][2], 2), " ")
    UR  <- strsplit(substring(bboxWKT[[1]][3], 2), " ")
    UL  <-
      strsplit(gsub("))", "", substring(bboxWKT[[1]][4], 2)), " ")
    
    LLX  <- as.numeric(LL[[1]][1]) - expandBBox
    LLY  <- as.numeric(LL[[1]][2]) - expandBBox
    LRX  <- as.numeric(LR[[1]][1]) + expandBBox
    LRY  <- as.numeric(LR[[1]][2]) - expandBBox
    URX  <- as.numeric(UR[[1]][1]) + expandBBox
    URY  <- as.numeric(UR[[1]][2]) + expandBBox
    ULX  <-  as.numeric(UL[[1]][1]) - expandBBox
    ULY  <- as.numeric(UL[[1]][2]) + expandBBox
  }
  
  else {
    # Custom bounding box provided
    bboxRAW <- strsplit(bboxCustom, ",")
    LLX  <- as.numeric(bboxRAW[[1]][1]) - expandBBox
    LLY  <- as.numeric(bboxRAW[[1]][2]) - expandBBox
    URX  <- as.numeric(bboxRAW[[1]][3]) + expandBBox
    URY  <- as.numeric(bboxRAW[[1]][4]) + expandBBox
  }
  bbox  <- paste(c(LLX, LLY, URX, URY), collapse = ", ")
  return(bbox)
}

#' getUSHCN retrieves the list of USHCN (U.S. Historical Climatology Network) station identifiers and compares that to the set of stations requested. Matches are returned as a vector with flag values (N = not HCN, Y = HCN).
#' 
#' @param responseList list of response array of requested station codes (sid)
#' @export getUSHCN
#'
getUSHCN <- function (responseList) {
  # Last updated 20121009 and URL may change in March 2018
  # Reference: https://doi.org/10.1175/JTECH-D-11-00103.1
  # Hard-coded (local) file used because of issues reading directly from online files
  # Re-factor to save to local file, remove # characters, and then read in with read.fwf()
  #hcnURL <- "http://cdiac.ess-dive.lbl.gov/ftp/ushcn_daily/ushcn-stations.txt"
  #hcnURL <- "ftp://ftp.ncdc.noaa.gov/pub/data/ushcn/v2.5/ushcn-v2.5-stations.txt"
  config <- add_headers(Accept = "'Accept':'application/text'")
  # Initialize data frame objects
  df <- NULL
  dfResponse <- NULL
  hcnColNames <- c("coopid","latitude","longitude","elevation","statecode", "name","component1","component2","component3","UTCoffset")
  # Initialize vector for HCN 'flag'
  hcnFlags0 <- rep("N", nrow(responseList))
  
  # Read txt file - using local copy because '#' character in URL file breaks both read.fwf() and read.table()
  hcnStations0 <- read.fwf(system.file("ushcn-v2.5-stations2.txt", package = "EnvironmentalSettingToolkit"), c(13,9,9,7,3,31,7,7,7,2), header=FALSE)
  #hcnStations0 <- read.fwf(file="inst/ushcn-v2.5-stations2.txt", c(13,9,9,7,3,31,7,7,7,2),header=FALSE)  #hcnStations0 <- content(GET(hcnURL))
  #hcnStations0 <- read.fwf(file=url(hcnURL), c(8,9,9,7,3,31,7,7,7,2),header=FALSE)
  #hcnStations0 <- read.fwf(file=url(hcnURL), c(13,9,9,7,3,31,7,7,7,2),header=FALSE)
  hcnStations <- as.data.frame(hcnStations0)
  setNames(hcnStations,hcnColNames)
  # Compare station sids to HCN ids and update vector indicating matches
  hcnMatches <- substr(responseList$sid1,1,6) %in% trimws(substr(hcnStations$V1,6,11), "both")
  #Column 6 (name) has |in place of # so need to replace then do a name 
  # match check with the name column of responseList; hits have hcnFlag = Y
  hcnFlags <- replace(hcnFlags0, hcnMatches=="TRUE", "Y")
  #browser()
  return(hcnFlags)
}

#' getProtocolStations function retrieves climate station monitoring locations used to request station-based metrics of the IMD Environmental Setting Protocol
#' @param dbInstance database server and instance, for example INPNISCVDBNRSST\\\\IMDGIS (note double back slash)
#' @param dbName database name
#' @param dbTable table name containing monitoring locations
#' @return A list of station UIDs
#' @export getProtocolStations
#'
getProtocolStations <- function(dbInstance, dbName, dbTable) {
  # Open database connection
  library(RODBC)
  connString <- paste0("driver={SQL Server};server=",dbInstance,";database=",dbName,";uid=Report_Data_Reader;pwd=ReportDataUser")
  dbConn <- odbcDriverConnect(connString)
  
  # Get station list
  res <- sqlQuery(dbConn, paste0("select uid from ",dbTable))
  
  # Close connection
  odbcClose(dbConn)
  
  # Return list
  return(res)
}

#' getDepartureCounts function calculates day counts by year or month for the station-based above and below normal metrics (CST8 and 9; CSP7 and 8) of the IMD Environmental Setting Protocol
#' @param rawDepartures Daily departures for a climate parameter (use getWxObservations() with normal="departure" to generate)
#' @param duration Duration of summarization period. Default is yearly ("yly"). Use "mly" for monthly.
#' @param metric (optional) One climate metric from the IMD Environmental Setting protocol
#' @param filePathAndName (optional) File path and name including extension for output CSV file
#' @return A data frame of stations, dates, and their departure counts above or below the 30-year climate normal (1981-2010)
#' @export getDepartureCounts
#'
getDepartureCounts <- function(rawDepartures, duration="yly", metric=NULL, filePathAndName=NULL) {
  dfResponse0 <- NULL
  aArray <- NULL
  bArray <- NULL
  dArray <- NULL
  idArray <- NULL
  nArray <- NULL
  metricArray <- NULL
  
  # Get date vector from rawDepartures
  dates <- as.Date(rawDepartures$date, "%Y-%m-d")
  
  if (duration == "yly") countDuration <- unique(format(dates, "%Y"))
  else countDuration <- unique(format(dates, "%Y-%m"))
  # Initialize output vectors: length == # years * # stations
  yearCount <- length(countDuration)
  stationCount <- length(unique(rawDepartures$uid))
  rowCount <- yearCount*stationCount
  aArray <- vector(mode = "integer", length = rowCount)
  bArray <- vector(mode = "integer", length = rowCount)
  idArray <- vector(mode = "integer", length = rowCount)
  nArray <- vector(mode = "character", length = rowCount)
  dArray <- vector(mode = "character", length = rowCount)
  metricArray <- rep(metric, rowCount)
  k <- 1
  for (i in 1:yearCount) {
    # Get rows with those dates (year)
    toCount <- subset(rawDepartures, format(rawDepartures$date,"%Y") == countDuration[i])
    for (j in 1:length(unique(toCount$uid))) {
      # Get data for each station for year
      byStation <- subset(toCount, uid == unique(toCount$uid)[j])
      # Count above normal (+) and below normal (-)
      if ("avgt_departure_F" %in% colnames(byStation)) {
        above <- nrow(subset(byStation, avgt_departure_F > 0))
        below <- nrow(subset(byStation, avgt_departure_F < 0))
      }
      else {
        above <- nrow(subset(byStation, pcpn_departure_in > 0))
        below <- nrow(subset(byStation, pcpn_departure_in < 0))
      }
      idArray[k] <- byStation$uid[1] # rawDepartures$uid[i]
      nArray[k] <- byStation$name[1]
      dArray[k] <- format(byStation$date[1],"%Y")  #format(rawDepartures$date[i],"%Y")
      aArray[k] <- above
      bArray[k] <- below
      # Set array index
      k <- k+1
    }
    toCount <- NULL
  }
  dfResponse0 <- cbind(idArray, nArray, dArray, aArray, bArray, metricArray)
  dfResponse <- as.data.frame(dfResponse0)
  colnames(dfResponse)[1] <- "uid"
  colnames(dfResponse)[2] <- "name"
  colnames(dfResponse)[3] <- "date"
  colnames(dfResponse)[4] <- "cntAboveNormal"
  colnames(dfResponse)[5] <- "cntBelowNormal"
  colnames(dfResponse)[6] <- "metric"
  
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
  
  return(dfResponse)
}

#' getRunCounts summarizes raw run response into a data frame with year and count of runs >= specified # of days
#' @param rawCounts run counts for a climate parameter (use getWxObservations() with a reduce code of 'run*' to generate)
#' @param runLength number of run days used to filter raw run counts
#' @param metric (optional) One climate metric from the IMD Environmental Setting protocol
#' @param filePathAndName (optional) File path and name including extension for output CSV file
#' @return A data frame of stations, dates, and their run counts greater than or equal to the specified runLength
#' @export getRunCounts
#' 
getRunCounts <-
  function(rawCounts,
           runLength,
           metric = NULL,
           filePathAndName = NULL) {
    dfResponse0 <- NULL
    idArray <-  NULL
    nArray <- NULL
    dArray <- NULL
    cArray <- NULL
    metricArray <- NULL
    
    # Get date vector from rawCounts
    dates <- as.Date(rawCounts$date, "%Y")
    countDuration <- unique(format(dates, "%Y"))
    yearCount <- length(countDuration)
    stationCount <- length(unique(rawCounts$uid))
    rowCount <- yearCount * stationCount
    
    idArray <- vector(mode = "integer", length = rowCount)
    nArray <- vector(mode = "character", length = rowCount)
    dArray <- vector(mode = "character", length = rowCount)
    cArray <- vector(mode = "integer", length = rowCount)
    metricArray <- rep(metric, rowCount)
    k <- 1
    
    for (i in 1:yearCount) {
      # Get rows with those dates (year)
      toCount <-
        subset(rawCounts, rawCounts$date == countDuration[i])
      for (j in 1:length(unique(toCount$uid))) {
        # Get data for each station for year
        #print(unique(toCount$uid)[j])
        byStation <- subset(toCount, uid == unique(toCount$uid)[j])
        # Count total greater than or equal to runLength
        #length(csp3check$pcpn_in_run[14][[1]][,1][as.numeric(csp3check$pcpn_in_run[14][[1]][,1]) >= 7])
        countTotal <- length(byStation$pcpn_in_run[1][[1]][,1][as.numeric(byStation$pcpn_in_run[1][[1]][,1]) >= 7])
        
        idArray[k] <- byStation$uid[1] 
        nArray[k] <- byStation$name[1]
        dArray[k] <- byStation$date[1]  
        cArray[k] <- countTotal
        # Set array index
        k <- k + 1
      }
      toCount <- NULL
    }
    dfResponse0 <- cbind(idArray, nArray, dArray, as.numeric(cArray), metricArray)
    dfResponse <- as.data.frame(dfResponse0)
    cntName <- paste("cntGERunLength",runLength, sep = "_")
    colnames(dfResponse)[1] <- "uid"
    colnames(dfResponse)[2] <- "name"
    colnames(dfResponse)[3] <- "date"
    colnames(dfResponse)[4] <- cntName
    colnames(dfResponse)[5] <- "metric"
    
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
    
    return(dfResponse)
  }

#' getStationMetrics requests Environmental Setting protocol metrics for a set of stations
#' @param climateStations A list of one or more unique identifiers (uid) for climate stations. Can be a single item, a list of items, or a data frame of the findStation response.
#' @param climateParameters A list of one or more climate parameters (e.g. pcpn, mint, maxt, avgt, obst, snow, snwd).  If not specified, defaults to all parameters except degree days. See Table 3 on ACIS Web Services page: \url{http://www.rcc-acis.org/docs_webservices.html}
#' @param sdate (optional) Default is period of record ("por"). If specific start date is desired, format as a string (yyyy-mm-dd or yyyymmdd). The beginning of the desired date range.
#' @param edate (optional) Default is period of record ("por"). If specific end date is desired, format as a string (yyyy-mm-dd or yyyymmdd). The end of the desired date range.
#' @param filePathAndRootname File path and root name for output CSV files. Do not include extension.
#' @return CSV files by metric
#' @examples \dontrun{
#' }
#' @export getStationMetrics
#'
getStationMetrics <-
  function(climateStations,
           sdate = "por",
           edate = "por",
           filePathAndRootname) {
    # Iterate list of parameters, creating output data frames (and optionally, CSV files), by metric
    #acisLookup <-
    #  fromJSON(system.file("ACISLookups.json", package = "EnvironmentalSettingToolkit")) # assumes placement in package inst subfolder

    # metricIdx <- grep('CS', acisLookup$climateMetrics$Metric)
    # for (i in 1:length(metricIdx)) {
    #   idx = metricIdx[[i]]
    #   if (acisLookup$climateMetrics[idx, ]$Metric != "CST8") {
    #     # do something
    #   }
    # }
    # Re-factor to read from lookups
    #metricListTemperature <- list("CST1","CST2","CST3","CST4","CST5","CST6","CST7")
    #metricListPrecipitation <- list("CPT1","CPT2","CST3","CPT4","CPT5","CPT6")
    # Get Temperature Metrics
    # for (i in 1:length(metricListTemperature)) {
    #   metric <- metricListTemperature[[i]]
    #   reduceValue <- NULL
    #   # Get reduce elements
    #   #acisLookup$stationIdType$description[acisLookup$stationIdType$code == testType]
    #   #reduceValue <- acisLookup$climateMetrics$Request[acisLookup$climateMetrics$Metric == metric]
    #   # Call getWxObservations
    # }
    # Get CST1: Hot Days (annual count)
    CST1Data <-
      getWxObservations(
        climateStations = climateStations,
        climateParameters = list("maxt"),
        sdate = sdate,
        edate = edate,
        duration = "yly",
        interval = "yly",
        reduceCodes = "cnt_ge_90",
        metric = "CST1"
      )
    outputMetricFile(CST1Data, "CST1",
                     filePathAndRootname)
    
    # Get CST2: Cold Days (annual count)
    CST2Data <-
      getWxObservations(
        climateStations = climateStations,
        climateParameters = list("maxt"),
        sdate = sdate,
        edate = edate,
        duration = "yly",
        interval = "yly",
        reduceCodes = "cnt_le_32",
        metric = "CST2"
      )
    outputMetricFile(CST2Data, "CST2",
                     filePathAndRootname)
    
    # Get CST3: Sub-freezing Days (annual count)
    CST3Data <-
      getWxObservations(
        climateStations = climateStations,
        climateParameters = list("mint"),
        sdate = sdate,
        edate = edate,
        duration = "yly",
        interval = "yly",
        reduceCodes = "cnt_le_32",
        metric = "CST3"
      )
    outputMetricFile(CST3Data, "CST3",
                     filePathAndRootname)
    
    # Get CST4: Days at or below 0 (annual count)
    CST4Data <-
      getWxObservations(
        climateStations = climateStations,
        climateParameters = list("mint"),
        sdate = sdate,
        edate = edate,
        duration = "yly",
        interval = "yly",
        reduceCodes = "cnt_le_0",
        metric = "CST4"
      )
    outputMetricFile(CST4Data, "CST4",
                     filePathAndRootname)
    
    # Get CST5: Growing degree days (base temperature >= 32) (annual count)
    CST5Data <-
      getWxObservations(
        climateStations = climateStations,
        climateParameters = list("gdd32"),
        sdate = sdate,
        edate = edate,
        duration = "yly",
        interval = "yly",
        reduceCodes = "cnt_gt_0",
        metric = "CST5"
      )
    outputMetricFile(CST5Data, "CST5",
                     filePathAndRootname)
    
    # Get CST6: Heating degree days (default base temperature >= 65) (annual count)
    CST6Data <-
      getWxObservations(
        climateStations = climateStations,
        climateParameters = list("hdd"),
        sdate = sdate,
        edate = edate,
        duration = "yly",
        interval = "yly",
        reduceCodes = "cnt_gt_0",
        metric = "CST6"
      )
    outputMetricFile(CST6Data, "CST6",
                     filePathAndRootname)
    
    # Get CST7: Cooling degree days (default base temperature >= 65) (annual count)
    CST7Data <-
      getWxObservations(
        climateStations = climateStations,
        climateParameters = list("cdd"),
        sdate = sdate,
        edate = edate,
        duration = "yly",
        interval = "yly",
        reduceCodes = "cnt_gt_0",
        metric = "CST7"
      )
    outputMetricFile(CST7Data, "CST7",
                     filePathAndRootname)
    
    # Get CST 8 and 9: Above and Below Normal Temperature Days
    CST8and9Source <- 
      getWxObservations(
        climateStations = climateStations,
        climateParameters = list("avgt"),
        sdate = sdate,
        edate = edate,
        duration = "dly",
        interval = "dly",
        normal = "departure",
        maxMissing = 10,
        metric = "CST8and9"
      )
    if(typeof(CST8and9Source) == "list") {
      CST8and9Data <-
        getDepartureCounts(rawDepartures = CST8and9Source,
                           duration = "yly",
                           metric = "CST8and9")
      outputMetricFile(CST8and9Data, "CST8and9",
                       filePathAndRootname)
    }
    
    # Get CSP1: Heavy precip days
    CSP1Data <-
      getWxObservations(
        climateStations = climateStations,
        climateParameters = list("pcpn"),
        sdate = sdate,
        edate = edate,
        duration = "yly",
        interval = "yly",
        reduceCodes = "cnt_ge_1.0",
        metric = "CSP1"
      )
    outputMetricFile(CSP1Data, "CSP1",
                     filePathAndRootname)
    
    #Get CSP2: Extreme precip days
    CSP2Data <-
      getWxObservations(
        climateStations = climateStations,
        climateParameters = list("pcpn"),
        sdate = sdate,
        edate = edate,
        duration = "yly",
        interval = "yly",
        reduceCodes = "cnt_ge_2.0",
        metric = "CSP2"
      )
    outputMetricFile(CSP2Data, "CSP2",
                     filePathAndRootname)
    
    # Get CSP3: Micro-drought
    CSP3Source <-
      getWxObservations(
        climateStations = climateStations,
        climateParameters = list("pcpn"),
        sdate = sdate,
        edate = edate,
        duration = "yly",
        interval = "yly",
        reduceCodes = "run_le_0.01",
        metric = "CSP3"
      )
    if(typeof(CSP3Source) == "list") {
      CSP3Data <-
        getRunCounts(rawCounts = CSP3Source,
                     runLength = 7,
                     metric = "CSP3")
      outputMetricFile(CSP3Data, "CSP3",
                       filePathAndRootname)
    }
    
    # Get CSP4: Measurable snow days
    CSP4Data <-
      getWxObservations(
        climateStations = climateStations,
        climateParameters = list("snow"),
        sdate = sdate,
        edate = edate,
        duration = "yly",
        interval = "yly",
        reduceCodes = "cnt_ge_0.1",
        metric = "CSP4"
      )
    outputMetricFile(CSP4Data, "CSP4",
                     filePathAndRootname)
    
    # Get CSP5: Moderate snow days
    CSP5Data <-
      getWxObservations(
        climateStations = climateStations,
        climateParameters = list("snow"),
        sdate = sdate,
        edate = edate,
        duration = "yly",
        interval = "yly",
        reduceCodes = "cnt_ge_3.0",
        metric = "CSP5"
      )
    outputMetricFile(CSP5Data, "CSP5",
                     filePathAndRootname)
    
    # Get CSP6: Heavy snow days
    CSP6Data <-
      getWxObservations(
        climateStations = climateStations,
        climateParameters = list("snow"),
        sdate = sdate,
        edate = edate,
        duration = "yly",
        interval = "yly",
        reduceCodes = "cnt_ge_5.0",
        metric = "CSP6"
      )
    outputMetricFile(CSP6Data, "CSP6",
                     filePathAndRootname)
    
    # Get CSP 7 and 8: Above and Below Normal Precipitation Days
    CSP7and8Source <- 
      getWxObservations(
        climateStations = climateStations,
        climateParameters = list("pcpn"),
        sdate = sdate,
        edate = edate,
        duration = "dly",
        interval = "dly",
        normal = "departure",
        maxMissing = 10,
        metric = "CSP7and8"
      )
    if(typeof(CSP7and8Source) == "list") {
      CSP7and8Data <-
        getDepartureCounts(rawDepartures = CSP7and8Source,
                           duration = "yly",
                           metric = "CSP7and8")
      outputMetricFile(CSP7and8Data, "CSP7and8",
                       filePathAndRootname)
    }
    
    return("SUCCESS")
    
  }

#' outputMetricFile writes metric data frames to a CSV file
#' @param metricData
#' @param metricName
#' @param filePathAndRootName
#'
outputMetricFile <- function(metricData, metricName, filePathAndRootName) {
  outFile <- paste(filePathAndRootName,gsub("METRIC",metricName,"_METRIC.csv"), sep = "")
  write.table(
    metricData,
    file = outFile,
    sep = ",",
    row.names = FALSE,
    qmethod = "double"
  )
}

#' outputAscii formats grid(s) as ASCII (*.asc) with headers and projection (*.prj)
#' @param gridResponse grid (dataframe format) returned from ACIS request (by date)
#' @param filePath full file path for ASCII output
#' @param lonCen longitude of lower left grid cell
#' @param lonCen latitude of lower left grid cell
#' @param luSource ACIS lookup source (as dataframe)
#' @export
#'
outputAscii <-
  function(gridResponse,
           fullFilePath,
           lonCen,
           latCen,
           luSource) {
    #xcen <- (as.numeric(unlist(strsplit(bbox, ","))[3]) - as.numeric(unlist(strsplit(bbox, ","))[1])) / 2 + as.numeric(unlist(strsplit(bbox, ","))[3])
    #ycen <- (as.numeric(unlist(strsplit(bbox, ","))[4]) - as.numeric(unlist(strsplit(bbox, ","))[2])) / 2 + as.numeric(unlist(strsplit(bbox, ","))[2])
    write(paste("ncols ", length(gridResponse[1, ])), fullFilePath)
    write(paste("nrows ", length(gridResponse[, 1])), fullFilePath, append =
            TRUE)
    write(paste("xllcenter ", lonCen), fullFilePath, append = TRUE)
    write(paste("yllcenter ", latCen), fullFilePath, append = TRUE)
    #write(paste("xllcorner ", unlist(strsplit(bbox, ","))[1]), fullFilePath, append=TRUE)
    #write(paste("yllcorner ", unlist(strsplit(bbox, ","))[2]), fullFilePath, append=TRUE)
    write(paste("cellsize ", luSource$cellSize), fullFilePath, append = TRUE)
    write(paste("NODATA_value ", luSource$missingValue),
          fullFilePath,
          append = TRUE)
    write.table(
      gridResponse,
      fullFilePath,
      row.names = FALSE,
      col.names = FALSE,
      append = TRUE
    )
    write(luSource$projection, gsub(".asc", ".prj", fullFilePath))
    return("Success")
  }


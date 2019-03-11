Tools supporting the NPS IMD Environmental Setting protocol.   

#### Overview ####

This package provides data access via ACIS web services (http://www.rcc-acis.org/docs_webservices.html) to station information and station data that:

+ Supports basic data request functions:

  * findStation
  * requestData

+ Formats outputs consistently:

  * Limits alteration to data returned from ACIS web services
  * Returns data frames or spatially referenced raster stacks
  
In addition, the package contains convenience functions for requesting Environmental Setting metrics, both the station-based metrics sourced from ACIS and the raster-based metrics sourced from 800m PRISM or climate indexes. 

The Environmental Setting Toolkit is the next iteration of and was forked from the IM_Climate toolkit created in 2016-2017. That toolkit can be found here: (https://github.com/IMDProjects/IM_Climate)

#### Draft Development Timeline

	Version 2.0 - March 2018
		Bug fixes
		Parameter date range availability
		Period of record summary 
		Station data (day count parameters) - standard and custom AOAs
		Station data (departures from normals) - standard and custom AOAs

	Version 2.1 - April 2018
		AOA polygon-based metrics (departures from normals gridded metrics using 800m PRISM data)
		Enhancements/bug fixes for station-based metrics
		
	Version 2.1.1 - June 2018
	  Performance enhancements for gridded metric generation
	  
	Version 2.1.2 - September 2018
	  Bug fixes 

	Version 2.2 - June 2018
	  Mirror capabilities for Python version of Toolkit
		Index-based metrics
		Upload polygon
		
	Version 2.3 - August 2018
		Generic capabilities: add DayMet/NASA NEX as sources 

#### Release 2.1.2 - 20180924 #### 
[Release Notes](https://github.com/nationalparkservice/EnvironmentalSetting_Toolkit/releases/tag/v2.1.2)
Bug fixes for getStationMetrics() function.  For protocol metric examples, see the two RMarkdown-produced PDFs: ES_StationMetricExamples.pdf and ES_GriddedMetricExamples.pdf.


#### Release 2.1 - 20180531 ####
[Release notes](https://github.com/nationalparkservice/EnvironmentalSetting_Toolkit/releases)

For protocol metric examples, see the two RMarkdown-produced PDFs: ES_StationMetricExamples.pdf and ES_GriddedMetricExamples.pdf.

		
#### Release 2.0 - 20180322 ####

[Release notes](https://github.com/nationalparkservice/EnvironmentalSetting_Toolkit/releases)

#### R - Installing the ES_Toolkit_R Package ####

The package can be installed from this GitHub repository by first installing and loading the [devtools](https://github.com/hadley/devtools) library from CRAN. __If you are on the NPS network__, run the

```R
library(httr)
set_config( config( ssl_verifypeer = 0L ) )
library(devtools)
install_github("nationalparkservice/EnvironmentalSetting_Toolkit/ES_Toolkit_R")
```
operation to grab the package code and install it locally. 


Otherwise, run the

```R
library(devtools)
install_github("nationalparkservice/EnvironmentalSetting_Toolkit/ES_Toolkit_R")
```
operation to grab the package code and install it locally.

#### Disclaimer ####
This software is in the public domain because it contains materials from the U.S. National Park Service, an agency of the United States Department of Interior.

Although this software package has been used by the U.S. National Park Service (NPS), no warranty, expressed or implied, is made by the NPS or the U.S. Government as to the accuracy and functioning of the package and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the NPS in connection therewith.

This software is provided "AS IS."

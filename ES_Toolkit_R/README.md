Tools supporting the NPS IMD Environmental Setting protocol.   

#### Overview ####

This packaage provides data access via ACIS web services (http://www.rcc-acis.org/docs_webservices.html) to station information and station data that:

+ Supports basic data request functions:

  * findStation
  * requestData

+ Formats outputs consistently

  * Limits alteration to data returned from ACIS web services
  
The Environmental setting Toolkit is the next iteration of and was forked from the IM_Climate toolkit created in 2016-2017. That toolkit can be found here: (https://github.com/IMDProjects/IM_Climate)

#### Draft Development Timeline

	Version 2.0 - March 2018
		Bug fixes
		Parameter date range availability
		Period of record summary 
		Station data (day count parameters) - standard and custom AOAs
		Station data (departures from normals) - standard and custom AOAs

	Version 2.1 - April 2018
		AOA polygon clipping (generic gridded metrics)

	Version 2.2 - June 2018
		AOA polygon clipping (departures from normals gridded metrics)
		Index-based metrics
  
#### Release 2.0 - Planned for March 2018 ####

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

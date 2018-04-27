import os
#import ES_Toolkit_py

os.chdir(r"D:\Project_Workspace\EnvironmentalSetting_Toolkit\ES_Toolkit_py")


from StationFinder import StationFinder
from StationDataRequestor import StationDataRequestor
from GridRequestor import GridRequestor
# from ES_Toolkit_py.StationFinder import StationFinder
# from ES_Toolkit_py.StationDataRequestor import StationDataRequestor
# from ES_Toolkit_py.GridRequestor import GridRequestor



sf = StationFinder()
#Locate all stations around MABI within a 30km buffer. Save results as MABI_Stations.csv
#wxStations = sf.findStation(unitCode = 'MABI', distance = 30, climateParameters = 'mint, maxt')

wxStations = sf.findStation(unitCode = 'MABI', distance = 30, climateParameters = 'mint,maxt', customBBox = '-114.291153779, 35.5612153111, -111.252315168, 37.0351548001')
print(wxStations)
# Ignition Model

The purpose of this model is to predict the cause of a bushfire.

# Data integration

- [x] Get Fire origins 
- [x] Join fuel layer
- [x] Join weather data
  - [x] rainfall, temperature and solar exposure
  - [x] wind speed
- [x] Join recreation site
- [x] Join CFA fire station
- [x] Join Road map

Code: combine_his.R

Result: training.csv

## training.csv

Fields:

- EVENTID: A unique identifier for a single fire created by CFA
- FIRE_NAME: The name of the fire
- FIRE_DIST: Not sure it is the CFA district or just district
- FIRE_STAT: Status of the fire when it being recorded (updated by CFA, so almost always 'safe')
- FIRE_NUM: Not sure what is the meaning of this variable. Fire numbers in a single entry?
- FIRE_START: The start date of the fire
- CAUSE: Cause reported by CFA
- lon: Longitude
- lat: Latitude


For FOR_CODE, FOR_TYPE, FOR_CAT, COVER, HEIGHT, FOREST, please read

[Attribute lookup tables (LUT) for the Forests of Australia (2018) dataset](https://www.agriculture.gov.au/sites/default/files/abares/forestsaustralia/documents/datasets/sofr2018/Forests_of_Australia_2018_Lookup_tables.pdf)

- id: A unique id set by me
- rf: Rainfall on that day
- arf7: Average rainfall in past 7 days
- arf14, arf28, arf60, arf90, arf180, arf360, arf720: Similar meaning
- se: Solar exposure on that day
- ase7: Average solar exposure in past 7 days
- ase14, ase28, ase60, ase90, ase180, ase360, ase720: Similar meaning
- maxt: Maximum temperature on that day
- amaxt7: Average maximum temperature in past 7 days
- amaxt14, amaxt28, amaxt60, amaxt90, amaxt180, amaxt360, amaxt720: Similar meaning
- mint: Minimu temperature on that day
- amint7: Average minimum temperature in past 7 days
- amint14, amint28, amint60, amint90, amint180, amint360, amint720: Similar meaning
- ws: Average wind speed on that day
- aws_mo: Average wind speed on that month
- aws-m1: Average wind speed in last month
- aws_m3: Average wind speed in last 3 months
- aws_m6: Average wind speed in last 6 months
- aws_m12: Average wind speed in last 12 months
- aws_m24: Average wind speed in last 24 months
- dist_cfa: Distance to the nearest CFA station
- dist_camp: Distance to the nearest recreation site
- dist_road: Distance to the nearest road
- new_cause: Recategorize cause of fire

## Data needed

[DataVic](https://www.data.vic.gov.au/)

VIC recreation site location data

CFA fire station location data

Fire Origins - Current and Historical

[Near-Surface Wind Speed](https://data.csiro.au/dap/landingpage?pid=csiro%3AWind_Speed)

Near-Surface Wind Speed - CSRIO

[osm - GeoFabrik](http://download.geofabrik.de/australia-oceania.html)

openstreetmap data for AU

[FOA 2018](https://www.agriculture.gov.au/abares/forestsaustralia/forest-data-maps-and-tools/spatial-data/forest-cover)

Forest of Australia 2018

[Bomrang](https://github.com/ropensci/bomrang)

Daily weather data in Australia

# Exploratory data analysis

# Modelling 

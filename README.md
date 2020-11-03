# Cause of bushfire ignitions in Victoria during the Australian 2019-2020 bushfire season

## Overview

This is a one-year research of bushfire ignitions in Victoria during the Australian 2019-2020 bushfires season. The outcomes include a [thesis for Weihao Li's honours degree](https://github.com/TengMCing/bushfire-paper/tree/master/thesis), a [thesis for Chang Liu's master degree](https://github.com/timtam3/Bushfire/tree/master/Australian%20Fire) and a [shiny app](https://github.com/timtam3/Bushfire/tree/master/Code) (credit to Chang Liu) for presentation. 

Three main aims of this research are:

1. Develop an spatio-temporal clustering algorithm to detect bushfire ignitions from satellite hotspot data
2. Build an classification model to predict the cause of bushfire ignitions in Victoria during the 2019-2020 season given 
    - ignition location, ignition time, 
    - temperature, rainfall, solar exposure, wind speed,
    - vegetation type, forest height, forest crown cover,
    - proximity to the nearest road, proximity to the nearest recreation site and proximity to the nearest fire station
3. Develop an interactive shiny app to visually explore bushfire ignition data

Researcher:

[Weihao Li](https://github.com/TengMCing)
- Honours student, Department of Econometrics and Business Statistics, Monash University

[Chang Liu](https://github.com/timtam3)
- Master student, Department of Econometrics and Business Statistics, Monash University

Supervisors:

[Dianne Cook](https://github.com/dicook)
- Professor of Business Analytics, Department of Econometrics and Business Statistics, Monash University

[Emily Dodwell](https://github.com/emdodwell)
- Principal Inventive Scientist - Data Science and AI Research, AT&T Labs

## Data source

Himawari-8 satellite hotspots data - Japan Aerospace Exploration Agency 

- [Ptree](https://www.eorc.jaxa.jp/ptree/index.html)

Daily weather data in Australia - Bureau of Meteorology

- [Bomrang](https://github.com/ropensci/bomrang)

Victoria recreation site locations - Department of Environment, Land, Water and Planning
Victoria fire station locations - Department of Environment, Land, Water and Planning
Victoria fire Origins - Department of Environment, Land, Water and Planning

- [DataVic](https://www.data.vic.gov.au/)

Australia road map - Openstreetmap

- [OSM - GeoFabrik](http://download.geofabrik.de/australia-oceania.html)

Forest of Australia 2018 - Australian Bureau of Agricultural and Resource Economics

- [FOA 2018](https://www.agriculture.gov.au/abares/forestsaustralia/forest-data-maps-and-tools/spatial-data/forest-cover)

Near-Surface Wind Speed - CSRIO

- [Near-Surface Wind Speed](https://data.csiro.au/dap/landingpage?pid=csiro%3AWind_Speed)

## Folders in this repository

### Research_plan

A 1500 words research plan includes introduction, research aims, review of literature, project design, timeline and expected outcome submitted in semester 1, 2020.

### presentation_1

A 10 minutes presentation delivered in week 10, semester 1, 2020. It covers the topic, research plan and preliminary work.

### presentation_2

A 30 minutes presentation delivered in week 10, semeseter 2, 2020. It covers the motivation, contribution, methodology, results, discussion and conslusion.

### thesis

A thesis for Weihao Li's honours degree.

### utility

Deprecated scripts and resources developed in an early stage.


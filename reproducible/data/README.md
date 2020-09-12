# Data Requirement

Due to some restriction on the data usage, we can not redistribute the data to the third parties. However, data used in this research are all open-source. Thus, to reproduce this research, you need to download serveral datasets first.

## Himawari-8 hotspots data

To download Himawari-8 hotspots data, you need to first submit an account request for data use on https://www.eorc.jaxa.jp/ptree/registration_top.html. Then you will receive an email which will contain the address of the **FTP** server, your ID and your password. Use your web browser to access to that **FTP** server. Under the path `/pub/himawari/L3/WLF/bet/`, you will find the hotspots data store in different folders. For example, `201507/` represents the hotspots data in July 2015. We need the monthly data from October 2019 to March 2020. Thus, go to `/pub/himawari/L3/WLF/bet/201910/monthly` and download the csv file `H08_20191001_0000_1MWLFbet_FLDK.06001_06001.csv`. And repeat this step for `201911`, `201912`, `202001`, `202002` and `202003`. The downloaded csv file should be placed in this folder `data`.

## BOM weather data

For BOM weather data, you can use the `get_weather_data.py` script to download the weather data we used in this research. This python script will access to my Google drive and grab the pre-download version of the BOM weather data for you. If you don't mind to spend an hour in downloading, you can use another script `get_weather_data.R` to download all the historical weather information using the `Bomrang` package. 

## Near surface wind speed data

To download near surface wind speed data from CSRIO, you need to go to this website https://data.csiro.au/dap/landingpage?pid=csiro%3AWind_Speed. Under the tab `Files`, you need to select all files in the `2deg` folder and send an request to download all files as Zip archive. Using the link in your email to download the data. Then you will need to place the zip file in `data` folder and unzip it. Notice that under the path `data/Near-Surface_Wind_Speed/mcvicar_etal_grl2008/2deg` you will still find some zip files, which are as expected. So, don't worry.

## Forest of Australia data

To download the forest of Australia data, using this link https://www.agriculture.gov.au/sites/default/files/abares/forestsaustralia/documents/datasets/sofr2018/aus_for18_publish.zip. Then you need to unzip it and place it in the `data` folder.

## Road map data

To download the road map from one of the distributions of OSM, using this link http://download.geofabrik.de/australia-oceania/australia-latest-free.shp.zip. Then you need to unzip it and place it in the `data` folder.

## ASOS wind data

To download ASOS wind data, go to this website https://mesonet.agron.iastate.edu/request/download.phtml. Switch the network to `Australia ASOS`, then select stations "YMAV", "YMAY", "YWGT", "YSWG", "YGTH", "YMEN", "YMTG", "YMIA" "YMER", "YNAR", "YCOM", "YPAD" and "YMML". Select `Wind Speed [mph]` from the available data. Specify the start date as `2017-01-01` and the end date as `2020-08-17`. Use the UTC time zone option. Set the data format to be `Comma Delimited (No DEBUG headers)`. Include latitude and longitude. Use blank/empty string to represent missing data. Choose the option `Save result data to file on compute`. Click the `Get Data` button, then you will have a file `asos.txt`. Place it in the `data` folder.

## Fire origins

Go to the website https://discover.data.vic.gov.au/dataset/fire-origins-current-and-historical. Type in your email and order the data. The website will redirect you to an interface. Select `Whole of State` as the Area Type. Put the `VIC` into the Selected Areas. Then click the `Apply to All` button and the `submit` button. Using the link in your email to download the data. Unzip it and place it in the `data` folder.

## Recreation site

Go to the website https://discover.data.vic.gov.au/dataset/recreation-sites and follow the same step as above.

## CFA stations

Go to the website https://discover.data.vic.gov.au/dataset/cfa-fire-station-vmfeat-geomark_point and follow the same step as above.
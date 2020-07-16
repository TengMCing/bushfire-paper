# access Australia weather data from 2019.10.01 to 2020.03.31 from BOM 

import requests
import time
import shutil

dates = ['201910', '201911', '201912', '202001', '202002', '202003']

base_url = "http://www.bom.gov.au/climate/dwo/yyyymm/text/IDCJDW_4digitcode.yyyymm.csv"
_4digitcode = 2801

for _4digitcode in range(10000):

	time.sleep(0.1)

	print(_4digitcode)

	url = base_url.replace("_4digitcode", str(_4digitcode).zfill(4))
	url = url.replace("yyyymm", "202001")

	response = requests.get(url)

	if '!DOCTYPE' in response.text:
		continue

	for date in dates:
		url = base_url.replace("_4digitcode", str(_4digitcode).zfill(4))
		url = url.replace("yyyymm", date)
		response = requests.get(url)

		with open('scrapy_data/IDCJDW' + str(_4digitcode).zfill(4) + '.' + date + '.csv', 'w') as f:
			f.write(response.text)

shutil.make_archive("scrapy_data", 'zip', "scrapy_data")
shutil.rmtree("scrapy_data")
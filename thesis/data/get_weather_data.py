import requests

weather_data_url = 'https://docs.google.com/uc?export=download&id=1DIRHiFzRZaNg9vj1L7G25XQZWyoEuAQ_'

r = requests.get(weather_data_url, allow_redirects=True)

open('weather.csv', 'wb').write(r.content)
import requests
import json
import pandas as pd

GEOCODE_BASE_URL = "https://maps.googleapis.com/maps/api/geocode/json?address="
URL_OPTIONS      = "&key=AIzaSyDcvsV2AslfSxtFSs6GQZKBHuILUh2vL8g"

def get_coordinates_from_address(address):
    response = requests.get(GEOCODE_BASE_URL+address+URL_OPTIONS)
    response = json.loads(response.text)
    lat = response["results"][0]["geometry"]["location"]["lat"]
    lon = response["results"][0]["geometry"]["location"]["lng"]
    return lat, lon

if __name__ == "__main__":
    filename = "public_schools.csv"
    df = pd.read_csv(filename)
    for i, row in df.iterrows():
        lat, lon = get_coordinates_from_address(row["PRIMARY ADDRESS"])
        df.loc[i, "LAT"] = lat
        df.loc[i, "LON"] = lon
    df.to_csv(filename+"_with_coordinates.csv", index=False)

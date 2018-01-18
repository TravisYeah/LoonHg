import utm
import csv

dictObj = []

with open("D:/Projects/USGS_R/loons/Travis/Final Re-Work 2017-12-08/coords/coordinates.csv", "rb") as csvfile:
    reader = csv.DictReader(csvfile)
    for row in reader:
        dictObj.append(row)

for row in dictObj:
    if (row["X_Utm"] != "") & (row["Y_Utm"] != ""):
        coord = utm.to_latlon(float(row["X_Utm"]), float(row["Y_Utm"]), 15, "T")
        row["X_Utm"] = coord[0]
        row["Y_Utm"] = coord[1]

with open("D:/Projects/USGS_R/loons/Travis/Final Re-Work 2017-12-08/coords/coordinatesConv.csv", "wb") as csvfile:
    fieldnames = ('X_Utm', 'Y_Utm', 'Long', 'Lat', 'DOWID', 'WATERWAY')
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames)

    writer.writeheader()
    for row in dictObj:
        writer.writerow(row)


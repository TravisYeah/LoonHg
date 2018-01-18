# Python program for computing the coefficient of determination
# for the NDMMF parameter set that was fitted for the Southeastern
# United States in April, 2014.

import math

fname1 = ".\R_SquaredComputationJune24_2015.csv"
fname2 = ".\RsquaredReport.txt"

INP = open(fname1, "rb")
OUT = open(fname2, "wb")

FieldNames = ["Recno", "DL", "Hg", "Length", "SPC", "Event",
              "Weight", "SamInEve", "Upper", "Lower", "LLength",
              "Weight1", "PROB", "pred6", "SPCParam", "EventParam",
              "Prediction"]
Fields = []


for inputline in INP:
    field = inputline.split(",")
    Fields.append(field)

numLines = len(Fields)
HgdiffTotal = 0.0
HgdiffMax = 0.0

for i in range(0,numLines):
     SPCParam = float(Fields[i][14])
     EventParam = float(Fields[i][15])
     Length = float(Fields[i][3])
     Prediction = float(Fields[i][16])
     Hgpred = (math.exp(EventParam+SPCParam*math.log(Length+1.0))-1.0)/1000.0
     Hgdiff = Prediction - Hgpred
     HgdiffTotal = HgdiffTotal + abs(Hgdiff)
     if abs(Hgdiff) > HgdiffMax:
          HgdiffMax = Hgdiff
     OUT.write("Line Number:  " + '{0:5d}'.format(i) + "   ")
     OUT.write('{0:+8.6f}'.format(Hgdiff)+'\n')

HgdiffAvg = HgdiffTotal/float(numLines)
OUT.write("\n\nAverage of absolute value of Hgdiff: "
          + '{0:8.6f}'.format(HgdiffAvg))
OUT.write("\nMaximum of absolute value of Hgdiff: "
          + '{0:8.6f}'.format(HgdiffMax))

INP.close()
OUT.close()

				 

# Python program for computing the coefficient of determination
# for the NDMMF parameter set that was fitted for the Southeastern
# United States in April, 2014.
#
# This program was prepared on June 24, 2015 by D. Donato

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
WtSampCount = 0
WtSumHg = 0.0

for inputline in INP:
    field = inputline.split(",")
    Fields.append(field)
    WtSampCount = WtSampCount + abs(int(field[6]))
    WtSumHg = WtSumHg + (float(field[2]) * float(abs(int(field[6]))))

numLines = len(Fields)
WtAvgHg = WtSumHg/float(WtSampCount)
RawSumDevs = 0.0
ModSumDevs = 0.0

for i in range(0,numLines):
    Hg = float(Fields[i][2])
    Weight = float(abs(int(Fields[i][6])))
    SPCParam = float(Fields[i][14])
    EventParam = float(Fields[i][15])
    Length = float(Fields[i][3])
    Prediction = float(Fields[i][16])
    Hgpred = (math.exp(EventParam+SPCParam*math.log(Length+1.0))-1.0)/1000.0
    RawSumDevs = RawSumDevs + (Weight * (math.pow(abs(Hg - WtAvgHg),2.0)))
    ModSumDevs = ModSumDevs + (Weight * (math.pow(abs(Hg - Hgpred),2.0)))
     
Rsquared = (RawSumDevs - ModSumDevs)/RawSumDevs
OUT.write("The coefficient of determination is:  "
          + '{0:8.6f}'.format(Rsquared) + "\n\n")

INP.close()
OUT.close()


#OUT.write("Line Number:  " + '{0:5d}'.format(i) + "   ")
#OUT.write('{0:+8.6f}'.format(Hgdiff)+'\n')
				 

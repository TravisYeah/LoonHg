#! /usr/bin/python
#
# Recode SpC Codes
#

NewSpCCode = 0
PriorCode = 0


INP = open("ObservationsS.dat", "r")
OT1 = open("ObservationsSrc.dat", "w")
OT2 = open("SpCCodes.dat", "w")


for line in INP:

     Recno = int(line[ 0:10])
     SpCut = int(line[10:20])
     Event = int(line[20:30])
     DLinf = int(line[30:40])

     if PriorCode <> SpCut:
          NewSpCCode = NewSpCCode + 1
          OT2.write ( '%10d%10d\n' % (NewSpCCode, SpCut) )
          PriorCode = SpCut
     SpCut = NewSpCCode
     OT1.write ( '%10d%10d%10d%10d\n' % (Recno, SpCut, Event, DLinf) )
 

INP.close()
OT1.close()
OT2.close()

print "Done\n"

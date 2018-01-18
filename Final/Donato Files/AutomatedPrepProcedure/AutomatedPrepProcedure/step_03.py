#! /usr/bin/python
#
# Recode Events
#

NewEventCode = 0
PriorCode = 0


INP = open("ObservationsE.dat", "r")
OT1 = open("ObservationsErcP.dat", "w")
OT2 = open("EventCodes.dat", "w")


for line in INP:

     Recno = int(line[ 0:10])
     SpCut = int(line[10:20])
     Event = int(line[20:30])
     DLinf = int(line[30:40])

     if PriorCode <> Event:
          NewEventCode = NewEventCode + 1
          OT2.write ( '%10d%10d\n' % (NewEventCode, Event) )
          PriorCode = Event
     Event = NewEventCode
     OT1.write ( '%10d%10d%10d%10d\n' % (Recno, SpCut, Event, DLinf) )
 

INP.close()
OT1.close()
OT2.close()

print "Done\n"

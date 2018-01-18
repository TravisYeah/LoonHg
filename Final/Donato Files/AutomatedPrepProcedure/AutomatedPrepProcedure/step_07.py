#! /usr/bin/python
#
# Analyze connectivity
#
# In the initial step, observations with below-detection-limit values
# are ignored.
#
# Marginal counts are stored in the array elements with index value "0".
#
# DRAFT -- D. Donato -- Jan. 29, 2014
#


IN1 = open("SpCCodes.dat", "r")
IN2 = open("EventCodes.dat", "r")
IN3 = open("ObservationsErc.dat", "r")
OT1 = open("ObservationsCmp.dat", "w")
OT2 = open("ConnMatrix.dat", "w")
OT3 = open("ObservationsNcp.dat", "w")

# Find the number of events and the number of SpC codes.

for line in IN1:
     MaxSpC = int(line[ 0:10])
IN1.close()

for line in IN2:
     MaxEvC = int(line[ 0:10])
IN2.close()

# Initialize the matrix of observation counts by Event and SpC.

Matrix = [[0 for i in range(0,MaxSpC+1)] for j in range(0,MaxEvC+1)]

# Make a pass through the observations in Event sequence and
# fill in the connectivity matrix.

for line in IN3:

     Recno = int(line[ 0:10])
     SpCut = int(line[10:20])
     Event = int(line[20:30])
     DLinf = int(line[30:40])

     if DLinf == 0:
          Matrix[Event][SpCut] = Matrix[Event][SpCut] + 1
#         print Event, SpCut, Matrix[Event][SpCut]
 
IN3.close()

# Partition the Events and SpCs into disjoint connected sets.

CurrentConnSet = 1
PartitionCompleteIndicator = 0

while PartitionCompleteIndicator == 0:

     # Find the first event that has not already been assigned to a disjoint
     # connected set and mark it as a member of the current connected set
     # using the value of the variable CurrentConnSet.
     for CEvent in range(1,MaxEvC+1):
          if Matrix[CEvent][0] == 0:
               Matrix[CEvent][0] = CurrentConnSet
               break

     # Mark the SpC or SpCs for the current event (CEvent).
     for i1 in range(1,MaxSpC+1):
          if Matrix[CEvent][i1] != 0 and Matrix[0][i1] == 0:
               Matrix[0][i1]=CurrentConnSet

     # Work through the SpCs for the current event, repeatedly
     # adding Events and SpCs until no further additions are
     # made during a pass through the Matrix.
     AdditionCount = 1
     while AdditionCount > 0:
          AdditionCount = 0
          for j1 in range(1,MaxSpC+1):
               if Matrix[0][j1] == CurrentConnSet:
                    for i1 in range(1,MaxEvC+1):
                         if Matrix[i1][0] == 0 and Matrix[i1][j1] > 0:
                              Matrix[i1][0] = CurrentConnSet
                              AdditionCount += 1
                              for k1 in range(1,MaxSpC+1):
                                   if Matrix[i1][k1] >0 and Matrix[0][k1] == 0:
                                        AdditionCount += 1
                                        Matrix[0][k1] = CurrentConnSet
     # Check whether partitioning is complete by scanning column 0 of the
     # Matrix. Partitioning is complete when there are no more zero values.
     PartitionCompleteIndicator = 1
     for i2 in range(1,MaxEvC+1):
          if Matrix[i2][0] == 0:
               PartitionCompleteIndicator = 0
               break;

     CurrentConnSet += 1


# Find the identifying number of the largest connected set.

ConnSetSizes = [0 for i3 in range(1,CurrentConnSet+2)]
for i3 in range(1,MaxEvC+1):
     ConnSetSizes[Matrix[i3][0]] +=1
max = 0
LargestConnSet = 0
for j3 in range(1,CurrentConnSet+1):
     if ConnSetSizes[j3] > max:
          max = ConnSetSizes[j3]
          LargestConnSet = j3

# Write the observations for the largest connected set to one file
# and the remaining observations to another file.

IN3 = open("ObservationsErc.dat", "r")
for line in IN3:
     Recno = int(line[ 0:10])
     SpCut = int(line[10:20])
     Event = int(line[20:30])
     DLinf = int(line[30:40])
     if Matrix[Event][0] == LargestConnSet:
          if DLinf == 0:
               OT1.write(line)
          else:
               if Matrix[0][SpCut] >0:
                    OT1.write(line)
               else:
                    OT3.write(line)
     else:
          OT3.write(line)
IN3.close()
OT1.close()
OT3.close()

# Write Matrix[][] to a file.

for i in range(0,MaxEvC+1):
     for j in range(0,MaxSpC+1):
          OT2.write('%5d' % Matrix[i][j])
     OT2.write('\n')

OT2.close()

print "Done\n"

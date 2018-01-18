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
OT1 = open("ConnMatrix.dat", "w")

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

# Count the number of Events in which each SpC occurs and store the count in
# Row 0 of the Matrix. At the same time, count the number of SpCs included
# in each Event and store the count in Column 0 of the Matrix.

for i in range(1,MaxEvC+1):
     for j in range(1,MaxSpC+1):
          if Matrix[i][j] !=0:
               Matrix[i][0] = Matrix[i][0] + 1
               Matrix[0][j] = Matrix[0][j] + 1

# Check the Matrix for the cases of events with 2 or fewer SpC's each of
# which occurs in 2 or fewer events.

for i in range(1,MaxEvC+1):
     for j in range(1,MaxSpC+1):
          if Matrix[i][j] > 0 and Matrix[i][0] <= 2 and Matrix[0][j] <= 2:
               print i,Matrix[i][0]," -- ",j,Matrix[0][j]

# Write Matrix[][] to the file.

for i in range(0,MaxEvC+1):
     for j in range(0,MaxSpC+1):
          OT1.write('%5d' % Matrix[i][j])
     OT1.write('\n')

OT1.close()

print "Done\n"

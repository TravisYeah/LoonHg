#! /usr/bin/python
#
# step_08.py
# ----------
#
# Select just the records of File_0.csv that are comparable (that is,
# that are also in ObservationsCmp.dat. Then write just these records to
# File_1.csv.
#
# Because the record numbers found in ObservationsCmp.dat are used to
# mark in an array the records to be included in the output file
# File_1.csv, it is not necessary to sort the records in ObservationsCmp.dat
# prior to running this program.
#
# Since the records written to File_1.csv contain the original Event and SpC
# codes, it is not necessary to recode events and SpC's prior to running
# this step.
#
# At the end, write out to File_2.csv a list of all record numbers with
# an indication for each whether it is for a comparable observation
# ("C") or a non-comparable observations ("N").


# STEP  1: Determine the maximum number of records.
#---------

maxnumrecs = 0;
INP = open("File_0.csv", "r")

INP.readline()
for line in INP:
     A = line.split(',')
     if int(A[0]) > maxnumrecs:
          maxnumrecs = int(A[0])

INP.close()


# STEP  2: Build an array of the record numbers for comparable observations.
#---------

CmpArray = []
CmpArray.append(0)
for i in range(1,maxnumrecs+1):
     CmpArray.append(0)

# STEP  3: Mark the comparable records in the array.
#---------

INP = open("ObservationsCmp.dat", "r")
for line in INP:
     cmprec = int(line[0:10])
     CmpArray[cmprec] = 1

INP.close()

# STEP  4: Read through the file of all observations and write each record
#          out to the CSV file of comparable observations if and only if
#          the record number is marked as included in the comparable records.
#---------

INP = open("File_0.csv", "r")
OTP = open("File_1.csv", "w")
OTP.write(INP.readline())

for line in INP:
     A = line.split(',')
     rec = int(A[0])
     if CmpArray[rec] ==1:
          OTP.write(line)

INP.close()
OTP.close()

# STEP 5: Write out a list of all record numbers along with an indicator for
#         each as to whether it is for a comparable ("C") or non-comparable
#         ("N") record.
#---------

OTP = open("File_2.csv", "w")

for i in range(1,maxnumrecs+1):
     if CmpArray[i]==1:
          char = "C"
     else:
          char = "N"
     OTP.write('%8d,%c\n' % (i,char))

OTP.close()


print "Done\n"

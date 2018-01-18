#! /usr/bin/python
#
# March 25, 2014 -- This program has been modified to drop any record
#    for an event with only one sample (i.e., SamInEve = 1).
#
#      D. Donato

INP = open("File_0.csv", "r")
OUT = open("Observations1.dat", "w")
INP.readline()

for line in INP:
          A = line.split(',')
          SamInEve = int(A[10]);
          if SamInEve > 1:
               OUT.write( '%10d%10d%10d%10d\n' % ( int(A[0]), int(A[2]), int(A[3]), int(A[7]) ) )

INP.close()
OUT.close()
print "Done\n"

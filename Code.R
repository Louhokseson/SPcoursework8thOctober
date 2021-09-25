#XXXXXXXXXXXXXXXXXXXXXXXX This PATH XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
setwd("D:/EDIN/Sem1/StatisticalProgramming/Coursework1/SPcoursework8thOctober")
# setwd("put/your/local/repo/location/here")
# setwd("put/your/local/repo/location/here")
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a <- a[-((n-2909):n)] ## strip license
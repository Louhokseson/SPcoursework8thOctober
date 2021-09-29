#XXXXXXXXXXXXXXXXXXXXXXXX This PATH XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
setwd("D:/EDIN/Sem1/StatisticalProgramming/Coursework1/SPcoursework8thOctober")
# setwd("put/your/local/repo/location/here")
# setwd("put/your/local/repo/location/here")
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

a <- scan("1581-0.txt",what="character",skip=156)
n <- length(a)
a <- a[-((n-2909):n)] ## strip license

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXX THE SPLIT-PUNCT XXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#x <- gsub(ls[i],"",x,fixed=TRUE) ## get rid of punctuation

split_punct <- function(x){
  #The function should search
  #for each word containing the punctuation mark, remove it from the word, and add the mark as a new entry
  #in the vector of words, after the word it came from. 
  ls <- c(",", ".", ";", "!", ":", "?", "’")
  lenl <- length(ls)
  lenA <- 0 
  lenx <- length(x)
  for(i in 1:lenl) {
    browser()
    #find the location of the words that contains punctuation(s).
    a <- grep(ls[i],x,fixed=TRUE)
    x[a] <- gsub(ls[i],"",x[a]) ## get rid of punctuation
    lenA <- lenA + length(a)
    Totallen <- lenA + lenx
    xs <- rep(0,Totallen) ## vector to store the single digits
    A <- a + 1:length(a)
    xs[A] <- ls[i]
    xs[-A] <- x
    x <- xs
  }
  return(x)
}
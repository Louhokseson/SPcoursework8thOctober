#XXXXXXXXXXXXXXXXXXXXXXXX The PATH XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
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

# test = c("An", "omnishambles,", "in", "a", "headless", "chicken", "factory")


is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}


split_punct <- function(x){
  #The function should search
  #for each word containing the punctuation mark, remove it from the word, 
  #and add the mark as a new entry in the vector of words, 
  #after the word it came from. 
  ls <- c(",", ".", ";", "!", ":", "?", "’")
  lenl <- length(ls)
  lenA <- 0 
  lenx <- length(x)
  for(i in 1:lenl) {
    #browser()
    #find the location of the words that contains punctuation(s).
    location <- grep(ls[i],x,fixed=TRUE)
    x[location] <- gsub(ls[i],"",x[location], fixed=TRUE) ## get rid of punctuation
    lenA <- lenA + length(location)
    Totallen <- lenA + lenx
    xs <- rep(0,Totallen) ## vector to store the single digits
    A <- location + 1:length(location)
    xs[A] <- ls[i]
    if(is.integer0(location)){
      xs <-x
    } else {
      xs[-A] <- x
    }
    x <- xs
  }
  return(x)
}

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXX SPLIT-PUNCT END XXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXX M COMMONLY OCCURRING WORDS XXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


splited_text <- split_punct(a)

len_splited <- length(splited_text)

lower_splited_text <- tolower(splited_text)

unique_words <- unique(lower_splited_text,incomparables = NULL)

vector_match <- match(lower_splited_text,unique_words,incomparables = NULL)

cat(length(vector_match) == length(splited_text))

count_unique_words <- tabulate(vector_match)


# The threshold function
#m_threshold <- function(x){
  
#  m <- 1000
#  epsilon <- 500
  
#  if((x<=m+epsilon) & (x>=m-epsilon)){
#    return(TRUE)
#  } else {
#    return(FALSE)
#  }
  
#}

#len_unique <- length(unique_words)
#location_threshold <- NULL
#for(i in 1:len_unique) {
#  if (m_threshold(count_unique_words[i])){
#    location_threshold <- append(location_threshold, i)
#  }
#}


#XXXXXXXXXXXXX From small to large XXXXXXXXXXXXXXXXXXX


Order <- order(count_unique_words)

most_common_word <- Order[(length(Order)-999):length(Order)]

b <- unique_words[most_common_word]






#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXX M COMMONLY OCCURRING WORDS END  XXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX




#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXX  Question 7  XXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
common_match <- match(lower_splited_text,b)

shifted_pair <- cbind(common_match[1:length(common_match)-1],common_match[2:length(common_match)])

sum_shifted_pair <- rowSums(shifted_pair)

#find the location of NA.
location_NA <- grep(NA,sum_shifted_pair,fixed=TRUE)

#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXX  Question 7  END XXXXXXXXXXXXXXXXX
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

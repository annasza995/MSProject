blueberriesData <- scan("blueberriesData.txt", double(), quote = "")
redberriesData <- scan("redberriesData.txt", double(), quote = "")

measuresOfAssociation <- function(data)
{
  Mode <- function(data) 
  {
    uniqueVector <- unique(data)
    countTab <- tabulate(match(data, uniqueVector))
    resultIndex <- which.max(countTab)
    resultCount <- countTab[resultIndex]
    
    checkTab <- tabulate(countTab);
    
    if(checkTab[resultCount] > 1)
      return(NA);
    
    return(uniqueVector[resultIndex]);
  }
  
  result <- vector(mode = "list", length = 7)

  names(result)[1] = "Mean"
  result[1] <- mean(data)
  
  names(result)[2] = "Mode"
  result[2] <- Mode(data)
  
  names(result)[3] ="Null quartile - minumum"
  result[3] <- quantile(data, 0.00)
  
  names(result)[4] ="First quartile"
  result[4] <- quantile(data, 0.25)
  
  names(result)[5] ="Second quartile - median"
  result[5] <- quantile(data, 0.50)
  
  names(result)[6] ="Third quartile"
  result[6] <- quantile(data, 0.75)
  
  names(result)[7] ="Fourth quartile - maximum"
  result[7] <- quantile(data, 1.00)
  
  return (result);
}

measuresOfAssociation(blueberriesData)
measuresOfAssociation(redberriesData)
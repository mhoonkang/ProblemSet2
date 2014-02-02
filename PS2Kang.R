## 1) calculating violtions
## option "b" for both Leemis' m statistic and Cho-Gains' d
## option "m" for Leemis' m statistic only
## option "d" for Cho-Gains' d only
## default is "b".
benford <- function(x, print="b"){
  firstdigit <- as.numeric(substr(x,1,1)) 
  # extracting the first digit from every numbers
  
  distribution <- c(firstdigit, c(1:9)) 
  # It is not guaranteed that every digit will be included
  # in this vector. So, we add each digit once to this vector.
  
  distribution <- (table(distribution)-1)
  # Making a table of the full digit distribution
  # By making a table, we know how many times does each digit shows as the first digit.
  # To do this, we need to subtract 1 from the total numbers shown as the first digit for
  # each digit because we added them to make columns for each digit.  
  
  Xi <- (distribution/sum(distribution)) 
  # Calculating Xi for i={1,2,...,9}
                                                           
  i <- c(1:9)  
  # setting as a numeric vector of numbers from 1 to 9.
  
  m <- max(Xi -log10(1+1/i))  # calculating Leemis' m
  
  d <- sqrt(sum((Xi-log10(1+1/i))^2))  # calculating Cho-Gains' d
  
  output <- list(Leemis.m=m, Cho.Gains.d=d, The.Full.Digit.Distribution=distribution)
  # making a list consist of Leemis'm, Cho-Gains' d, and the full digit distribution
  
  
  if(print=="b"){return(output)} # print the results according to the option
  else{if(print=="m"){
    return(output[-2])}
       else{if(print=="d"){
         return(output[-1])
       } else {cat("Please input a valid option.")}
       }
  }
}

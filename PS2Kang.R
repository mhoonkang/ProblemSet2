## 1) calculating violations
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
  
  m <- sqrt(length(firstdigit))*max(abs((Xi-log10(1+1/i))))  # calculating Leemis' m
  
  d <- sqrt(length(firstdigit))*sqrt(sum((Xi-log10(1+1/i))^2))  # calculating Cho-Gains' d
  
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


## 2) Critical Values
print.benfords <- function(x){
  firstdigit <- as.numeric(substr(x,1,1)) 
  distribution <- c(firstdigit, c(1:9)) 
  distribution <- (table(distribution)-1)
  Xi <- (distribution/sum(distribution)) 
  i <- c(1:9)  
  m <- sqrt(length(firstdigit))*max(abs((Xi-log10(1+1/i))))
  d <- sqrt(length(firstdigit))*sqrt(sum((Xi-log10(1+1/i))^2))
  # the above code is the same as the previous function code except the code regarding printing
  
  crit.m <- c(m <= 0.851, m>0.851 & m<=0.967, m> 0.967 & m<=1.212, m>1.212 )
  crit.d <- c(d<= 1.121, d>1.121 & d<=1.330, d> 1.330 & d<=1.569, d>1.569 )
  # making boolean code for identifying the critical values for m and d
  
  asterisk <- (c(" ", "*", "**", "***"))
  # making a vector of asterisks showing significance level
  
  
  output <- matrix(c(m, d, asterisk[crit.m==TRUE],  asterisk[crit.d==TRUE]),2,2)  
  rownames(output) <- c("Leemis' m","Cho-Gains' d")
  colnames(output) <- c("Test Statistics","Signif. Level")
  # making a matrix which shows the name of each statistic, statistic as it was calculated,
  # and the relevant number of asterisk's. 
  # If the blooean value of crit.m(crit.d) is TRUE, then the corresponding element of asterisk
  # will be shown in the matrix. 
  
  cat("Benford's law test to reject the null hypothesis of no fraud")
  cat("\n")
  cat("\n")
  print(output, quote=FALSE)
  cat("\n")
  cat("Significant Level: *** alpha<0.01, ** alpha<0.05, * alpha<0.10")
  # print the output without quotation mark, and print a legend explaining the asterisk's.
  return.value<-t(output[,1])
  invisible(matrix(as.numeric(return.value),2,1))
  # invisibly returning the statistics for further calculation of them
  }
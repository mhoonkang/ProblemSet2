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
  
  m <- sqrt(length(firstdigit))*max(abs((Xi-log10(1+1/i))))  
  # calculating Leemis' m
  
  d <- sqrt(length(firstdigit))*sqrt(sum((Xi-log10(1+1/i))^2))  
  # calculating Cho-Gains' d
  
  total <- c(sum(distribution), sum(Xi))
  dist.print <- rbind(distribution, Xi)
  dist.print <- cbind(dist.print, total)
  rownames(dist.print) <- c("Count", "Probability") 
  # making a matrix which shows the full digit distribution by count and by probability
  
  output <- list("Leemis' m"=m, "Cho-Gains' d"=d, "The Full Digit Distribution"=dist.print)
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
  # making boolean vector for identifying the critical values for m and d
  
  asterisk <- (c(" ", "*", "**", "***"))
  # making a vector of asterisks showing significance level
  
  output <- matrix(c(m, d, asterisk[crit.m==TRUE],  asterisk[crit.d==TRUE]),2,2)  
  rownames(output) <- c("Leemis' m","Cho-Gains' d")
  colnames(output) <- c("Test Statistics","Signif. Level")
  # making a matrix which shows the name of each statistic, statistic as it was calculated,
  # and the relevant number of asterisk's. 
  # If the boolean value of 'crit.m(crit.d)' is TRUE, then the corresponding element of 'asterisk'
  # will be shown in the matrix. 
  
  cat("Benford's law test to reject the null hypothesis of no fraud")
  cat("\n")
  cat("\n")
  print(output, quote=FALSE)
  cat("\n")
  cat("Significant Level: *** alpha<0.01, ** alpha<0.05, * alpha<0.10")
  # print the output without quotation mark, and print a legend explaining the asterisk's.
  statistics <- matrix(as.numeric(output[,1]),2,1)
  rownames(statistics) <- c("Leemis' m", "Cho-Gains' d")
  colnames(statistics) <- c("Test Statistics")
  invisible(statistics)
  # invisibly returning the matrix of the each statistics for further calculation.
}

## 3) Testing
test.benfords <- function(){
  benford <- function(x, print="b"){
    firstdigit <- as.numeric(substr(x,1,1)) 
    distribution <- c(firstdigit, c(1:9)) 
    distribution <- (table(distribution)-1)
    Xi <- (distribution/sum(distribution)) 
    i <- c(1:9)  
    m <- sqrt(length(firstdigit))*max(abs((Xi-log10(1+1/i))))   
    d <- sqrt(length(firstdigit))*sqrt(sum((Xi-log10(1+1/i))^2))    
    total <- c(sum(distribution), sum(Xi))
    dist.print <- rbind(distribution, Xi)
    dist.print <- cbind(dist.print, total)
    rownames(dist.print) <- c("Count", "Probability") 
    output <- list("Leemis' m"=m, "Cho-Gains' d"=d, "The Full Digit Distribution"=dist.print)
    if(print=="b"){return(output)} 
    else{if(print=="m"){
      return(output[-2])}
         else{if(print=="d"){
           return(output[-1])
         } else {cat("Please input a valid option.")}
         }
    }
  }
  # sub-funtion('benford') from problem 1.
  # It is not necessary to insert a 'print.benfords' function here because the function
  # has the same codes for calculating statistics and distribution as 'benford' function.
  
  distribution.1 <- round(log10(1+1/c(1:9))*1000,0) 
  # making a distribution according to Benford's law
  dataset.1 <- rep(seq(10,90,by=10),distribution.1) 
  # making a dataset 1 where Benford's law is met
  Xi.1 <- distribution.1/sum(distribution.1)
  # the full digits probability distribution 
  dist.1 <- rbind(distribution.1, Xi.1)  
  # making distribution(count and probability) matrix 
  m.1 <- sqrt(sum(distribution.1))*max(abs((Xi.1-log10(1+1/c(1:9)))))
  # Calculating Leemis' m for dataset 1
  d.1 <- 1.2345
  # putting wrong value to fail unit test for wrong d for dataset 1.
  
  distribution.2 <- rep(c(112,111), c(1,8)) 
  # making a distribution where Benford's law is not met
  dataset.2 <- rep(seq(10,90,by=10),distribution.2) 
  # making a dataset 2 where Benford's law is not met
  Xi.2 <- distribution.2/sum(distribution.2)
  # the full digits probability distribution 
  dist.2 <- rbind(distribution.2, Xi.2)  
  # making distribution(count and probability) matrix   
  m.2 <- sqrt(sum(distribution.2))*max(abs((Xi.2-log10(1+1/c(1:9)))))
  # Calculating Leemis' m for dataset 2
  d.2 <- sqrt(sum(distribution.2))*sqrt(sum((Xi.2-log10(1+1/c(1:9)))^2))
  # Calculating Cho-Gains' d for dataset 2
  
  result.1 <- benford(dataset.1)    
  test.1 <- c(m=result.1[[1]]==m.1, d=result.1[[2]]==d.1, distribution=sum(result.1[[3]][1:2,1:9]==dist.1)==18)
  # comparing the truth for the digit distributions and two test statistics to the results 
  # from 'benford' fucntion for dataset 1.
  result.2 <- benford(dataset.2)
  test.2 <- c(m=result.2[[1]]==m.2, d=result.2[[2]]==d.2, distribution=sum(result.2[[3]][1:2,1:9]==dist.2)==18)
  # comparing the truth for the digit distributions and two test statistics to the results 
  # from 'benford' fucntion for dataset 2.
  test <- c(test.1, test.2)
  # merging two boolean vectors.
  
  which.statistic <- rep(c("m","d","distribution"),2)
  which.data <- rep(c("dataset 1","dataset 2"), c(3,3))
  # making vectors of the message which will be printed according to the value of
  # the boolean vector 'test'. 
  
  if(FALSE %in% test){
    paste("FALSE: The function calculates the wrong", which.statistic[test==FALSE],"for", which.data[test==FALSE])
  }
  else{cat("TRUE")}
  # printing TRUE if all unit tests are passed, and printing FALSE if all unit test are not passed.
  # In case of FALSE, it will show where the function is broken.
}
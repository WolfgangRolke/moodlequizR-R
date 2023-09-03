quiz23.outliers <- function(type) {
    if(missing(type))
      type <- sample( 1:4, 1, prob = c(3,1,1,1))
    category <- paste("Probability / Outliers / How many ? / type = ", type)
    quizname <- " problem -" 
    n <- 100
    tst=rep(TRUE,4)
    repeat {
      x <- rnorm(n, 10, 3)
      y <- x + rnorm(n, 0, 1)
      tst[1]=ifelse(quantile(x,0.25)-1.5*IQR(x)<min(x), TRUE,FALSE)
      tst[2]=ifelse(quantile(x,0.75)+1.5*IQR(x)>max(x), TRUE,FALSE)
      tst[3]=ifelse(quantile(y,0.25)-1.5*IQR(y)<min(y), TRUE,FALSE)
      tst[4]=ifelse(quantile(y,0.75)+1.5*IQR(y)>max(y), TRUE,FALSE)    
      if(all(tst)) break
    }
    if(type == 1) {
        qtxt <- "{2:MC:~=None~One~Two~Three~Four~Five}"
        atxt <- "No outlier"  
    }    
    if(type == 2) { 
        x <- c(x, 15)
        y <- c(y, 3)
        qtxt <- "{2:MC:~None~=One~Two~Three~Four~Five}"
        atxt <- "One outlier in lower right corner of scatterplot. It does not appear in either boxplot"
    }    
    if(type == 3) { 
        x <- c(x, runif(1, 40, 45))
        y <- c(y, runif(1, 40, 45))
        qtxt <- "{2:MC:~None~=One~Two~Three~Four~Five}"
        atxt <- "One outlier in upper right corner. This outlier also appears in both boxplots."
    }    
    if(type == 4) { 
        x <- c(x, runif(2, 40, 45))
        y <- c(y, runif(2, 40, 45))
        qtxt <- "{2:MC:~None~One~=Two~Three~Four~Five}"
        atxt <- "Two outliers in upper right corner. They both also appear in both boxplots."
    }    
    if(type == 5) { 
        x <- c(x, runif(2, 40, 45), runif(1, 2, 6))
        y <- c(y, runif(2, 40, 45), runif(1, 20, 25))
        qtxt <- "{2:MC:~None~One~Two~=Three~Four~Five}"
        atxt <- "Two outliers in upper right corner and another at around x=4, y=22"
    }    
    plt <- mplot(y, x, return.graph = TRUE)
    plt64 <- png64(plt)
    qtxt <- paste("How many outliers do you see?<p>", qtxt) 
    htxt <- "Remember that there are  three graphs here, so each observation is shown three times"            

    list(qtxt = paste("<h5>", qtxt, "</h5>", plt64),
         htxt = paste("<h5>", htxt, "</h5>"),
         atxt = paste("<h5>", atxt, "</h5>"),
         category = category, quizname = quizname)    
}    

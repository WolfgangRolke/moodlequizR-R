regression.simple <- function(whichstory, doAll=FALSE, doAssumptions = FALSE, 
    doEquation=FALSE, doR2 = FALSE, doGraphs=FALSE,
    doPredictEstimate=FALSE, doPredictInterval=FALSE, doTest=FALSE, whichtest="both") {
    require(mvtnorm)
    if(missing(whichstory)) whichstory <- sample( 1:4, 1)
    if(doAll) {doEquation <- TRUE; doPredictInterval <- TRUE; doR2 <- TRUE; doAssumptions <- TRUE}
    category <- paste("Simple Regression / Story = ", whichstory)  
    if(doAssumptions) category <- paste0(category, "  - Assumptions ")    
    if(doEquation) category <- paste0(category, " - Equation ")
    if(doR2) category <- paste0(category, " - R2 ")
    if(doPredictInterval) category <- paste0(category, " - Intervals:Prediction ")
    if(doPredictEstimate) category <- paste0(category, " - Estimate:Prediction ")
    if(doTest) category <- paste0(category, " - Hypothesis Tests  - ", whichtest)
    quizname <- "problem - " 
    g <- function() {
      plt64a <- ""; plt64b <- ""
      if(doGraphs) {
        plts <- slr(y, x, return.graph=TRUE)
        plt64a <- png64(plts[[1]])
        plt64b <- png64(plts[[2]])
      }  
      out <- slr(y, x, show.tests=TRUE, return.result=TRUE)
      
      qtxt <- ""
      atxt <- ""
      if(doAssumptions) {
          qtxt <- paste0(qtxt, "<p>The assumption of a linear model is {:MC:~=OK~Not OK~Can't tell}
            <p>The assumption of normal residuals is {:MC:~=OK~Not OK~Can't tell}
            <p>The assumption of equal variance is {:MC:~=OK~Not OK~Can't tell}")
          atxt <- paste0(atxt, "<p>The residual vs fits plot shows no problems with either the 
            linear model or the equal variance, and the normal plot looks fine, so the assumptions
            of least squares regression are all OK")
      }
      if(doEquation) {
          qtxt <- paste0(qtxt, "<p>The least squares regression line is: <p>", 
              varnames[2]," =  {:NM:%100%", out[1], ":", out[1]/100, "}&nbsp;&nbsp;", 
              ifelse( out[2]<0, "{:MC:~=-~+}", "{:MC:~-~=+}"), "&nbsp;&nbsp;", 
              "{:NM:%100%", abs(out[2]), ":", out[2]/100, "}", varnames[1])                         
          atxt <- paste0(atxt, "<p>The least squares regression line is: <p>", 
              varnames[2], "&nbsp;&nbsp;= &nbsp;&nbsp;", out[1], "&nbsp;&nbsp;", out[2], "&nbsp;&nbsp;", varnames[1])                                       
      }
      if(doR2) {
          qtxt <- paste0(qtxt, "<p>R<sup>2</sup> = &nbsp;&nbsp;{:NM:%100%", out[3], ":0.1}%") 
          atxt <- paste0(atxt, "<p>R<sup>2</sup> = &nbsp;&nbsp;", out[3], "%") 
      }    
      if(doPredictEstimate) {
          yval <- out[1]+out[2]*xval
          qtxt <- paste0(qtxt, "The estimate of ", varnames[2], " if ", varnames[1], "=", xval, 
              "is {:NM:%100%", yval, ":", yval/100, "}")
          atxt <- paste0(atxt, "The estimate of ", varnames[2], " if ", varnames[1], "=", xval, 
              " is ", yval)
      }
      if(doPredictInterval) {
        pout <- slr.predict(y, x, newx=xval, interval=whichinterval, 
        conf.level=conf.level, return.result=TRUE)[3:4]
        pout1 <- slr.predict(y, x, newx=xval, interval=whichinterval, return.result=TRUE)[3:4]  
        if(whichinterval=="PI") {
          pout2 <- slr.predict(y, x, newx=xval, interval="CI", conf.level=conf.level, return.result=TRUE)[3:4]
          pout3 <- slr.predict(y, x, newx=xval, interval="CI", return.result=TRUE)[3:4]      
        }
        if(whichinterval=="CI") {
          pout2 <- slr.predict(y, x, newx=xval, interval="PI", conf.level=conf.level, return.result=TRUE)[3:4]
          pout3 <- slr.predict(y, x, newx=xval, interval="PI",  return.result=TRUE)[3:4]      
        }
        if( min(x) < xval & xval < max(x) ) {
          qptxt <- "<p>this is a {:MC:~=Prediction~Extrapolation}"
          aptxt <- paste0("<p>this is a prediction : ", min(x), " < ", xval, " < ", max(x))
        }  
        else {
          qptxt <- "<p>this is a {:MC:~Prediction~=Extrapolation}"
          if( xval < min(x) )
              aptxt <- paste0("<p>this is an extrapolation : ",  xval, " < ", min(x))  
          else
              aptxt <- paste0("<p>this is an extrapolation : ",  xval, " > ", max(x))      
        }
        qtxt <- paste0(qtxt, "<p>A ", conf.level, "% confidence interval for ",
            ifelse(whichinterval=="PI", 
                paste("the value of ", varnames[2], " when the value of ", varnames[1], " was ", xval),     
                paste("the mean value of ", varnames[2], " when the value of ", varnames[1], " was ", xval)), 
                " is given by 
                ({:NM:%100%", pout[1], ":", pout[1]/100,
                  "~%75%", pout1[1], ":", pout1[1]/100,  
                  "~%60%", pout2[1], ":", pout2[1]/100, 
                  "~%50%", pout3[1], ":", pout3[1]/100, "} , 
                {:NM:%100%", pout[2], ":", pout[2]/100, 
                  "~%75%", pout1[2], ":", pout1[2]/100,  
                  "~%60%", pout2[2], ":", pout2[2]/100, 
                  "~%50%", pout3[2], ":", pout3[2]/100, "})", qptxt)
                   
         atxt <- paste0(atxt, "<p>A ", conf.level, "% confidence interval is given by (",
               pout[1], ", ", pout[2], ")", aptxt)
      }  
      if( doTest ) {
          if(whichtest == "Intercept" | whichtest == "both") {
              if(out[4]<0.05) is.not <- "{:MC:~=is~is not}"
              else is.not <- "{:MC:~is~=is not}"
              qtxt <- paste0(qtxt, "<p>The intercept of the least squares regression line", is.not, 
                " statistically significantly different from 0")
          }
          if(whichtest == "Slope" | whichtest == "both") {
              if(out[5]<0.05) is.not <- "{:MC:~=is~is not}"
              else is.not <- "{:MC:~is~=is not}"
              qtxt <- paste0(qtxt, "<p>The slope of the least squares regression line ", 
                  is.not, " statistically significantly different from 0")
          }      
      }    
      if(doTest) command <- paste0("slr(", varnames[2], ", ", varnames[1], ", show.tests=TRUE)") 
      else command <- paste0("slr(", varnames[2], ", ", varnames[1], ")")
      command1 <- ""
      if(doPredictInterval) 
          command1 <- paste0("slr.predict(", varnames[2], ", ", varnames[1], ", newx=", xval,
          ", interval=\"", whichinterval, "\"", 
          ifelse(conf.level==95, "", paste0(", conf.level=", conf.level)), ")")
      if(doPredictEstimate) 
          command1 <- paste0("slr.predict(", varnames[2], ", ", varnames[1], ", newx=", xval, ")")
                                                         
      atxt <- paste0( atxt, "<p>R commands: ", command,"<br>", command1)                    
      list(qtxt = qtxt, atxt = atxt, plt64a = plt64a, plt64b = plt64b)  
    } 
    repeat {
      n <- sample( 20:50, 1)
      if(whichstory == 1)  {
        x <- 1:n
        y <- round(sample(8:12,1) + runif(1, 0.8, 1.2)*sample(c(-1,1), 1)*x/10 + rnorm(n,0,1))
        y[y<0] <- 0
      }
      if(whichstory == 2)  {
        rho <- runif(1, -0.8, 0.8)
        z <- 20000+1000*rmvnorm(n, sigma=matrix( c(1, rho, rho, 1),2,2))
        x <- round(z[,1], 2)
        y <- round(z[,2], 2)
      }
      if(whichstory == 3)  {
        n <- sample(40:50, 1)
        x <- 20:n
        y <- round(1000+5*x+rnorm(length(x),0,100))
        y[y<0] <- 0
      }
      if(whichstory == 4)  {
        n <- sample(20:50, 1)
        x <- 2000+c(1:n)*100
        y <- round( x/1000*runif(1, 0,0.5)+rnorm(n,0,1), 1)
        y <- 10*(y+min(y)+1)
      }
      R2 <- slr(y, x, noGraph=TRUE, return.result=TRUE)[3]
      if(R2>10) break
    }  
    delta <- max(x)-min(x)
    xval <- runif(1, max(0, min(x)-2*delta), min(x)+2*delta)
    if( whichstory == 2) xval <- round(xval, 2)
    else xval <- round(xval)
    conf.level = sample(c(90, 95, 99), 1)
    whichinterval <- sample(c("PI", "CI"), 1, prob=c(3,1))    
    if(whichstory == 1) {
         varnames <- c("Day", "Faults")
         data <- data.frame(Day=x, Faults=y)
         qtxt <- paste0("A company has a machine that makes widgets. For ", n, " consecutive days
            they count the number of faulty widgets.")
    }  
    if(whichstory == 2) { 
         varnames <- c("Day_1", "Day_2")
         data <- data.frame(Day_1=x, Day_2=y)
         qtxt <- paste0("An economist wants to study the relationship of the 
            values of the Dow Jones industrial index on consecutive days. He
            randomly selects ", n, " days and records the value on this and the 
            following day. ") 
   }
   if(whichstory == 3) { 
         varnames <- c("Sugar","Microbes")
         data <- data.frame(Sugar=x, Microbes=y)
         qtxt <- paste0("A biologist wants to study the relationship between
            the number of microbes that grow in a petry dish and the amount of sugar 
            (in grams). ")
   }
   if(whichstory == 4) { 
         varnames <- c("RPM", "Oil")
         data <- data.frame(RPM=x, Oil=y)
         qtxt <- paste0("An industrial engineer wants to study the relationship between
            the RPM (revolutions per minute) of an engine and the amount of 
            oil used (in milliliters).")
   }
   
   g1 <- g() 
   list(qtxt = paste0("<h5>", qtxt, g1$qtxt,"</h5><hr>", moodle.table(data)), 
        atxt = paste0("<h5>", g1$atxt,"</h5>", g1$plt64a, g1$plt64b), 
        category = category, quizname = quizname)     
}   

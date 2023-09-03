regression.multiple <- function(doAll=FALSE, doEquation=FALSE, doR2 = FALSE, 
    doPredictEstimate=FALSE, doPredictInterval=FALSE, doBest=FALSE) {
    require(mvtnorm)
    whichstory <- sample( 1:3, 1)
    if(doAll) {doEquation <- TRUE; doPredictInterval <- TRUE; doR2 <- TRUE; doBest<-TRUE}
    category <- "Multiple Regression/"  
    if(doEquation) category <- paste0(category, "Equation ")
    if(doR2) category <- paste0(category, "R2 ")
    if(doPredictInterval) category <- paste0(category, "Intervals - Prediction ")
    if(doPredictEstimate) category <- paste0(category, "Estimate - Prediction ")
    if(doBest) category <- paste0(category, "Mallow's Cp ")
    quizname <- "problem - " 
    if(whichstory == 1) {
      npred <- 2
      n <- sample(5:10, 1)
      x <- matrix(0, 5*n, npred)
      colnames(x) <- 1:npred
      x[, 1] <- rep(2*(1:n), 5)
      x[, 2] <- rep(0:4, each=n)
      y <- 1 + x[,1]/3 + x[,2]/2 + rnorm(5*n)
      y[y<0] <- 0
      yname <- "Growth"
      xnames <- c("Day", "Water")
      newx <- c(sample(6:10, 1), round(runif(1, 0, 4), 1))
      data <- data.frame(Growth=y, Day=x[, 1], Water = x[, 2])
      qtxt <- paste0("An agricultural scientist wants to find out how
            the growth of a certain plant depends on the time and the 
            amount of water (in liters per day) used.")
    }
    if(whichstory == 2) {
      npred <- 4
      n <- sample(30:50, 1)
      x <- matrix(0, n, npred)
      colnames(x) <- 1:npred
      x[, 1] <- round(runif(n, 10, 20), 1)
      x[, 2] <- round(runif(n, 0, 2), 2)
      x[, 3] <- round(runif(n, 50, 100), 1)
      x[, 4] <- round(runif(n, 0.1, 0.5), 3)
      y <- 100 + 
          sample(0:1, 1, prob=c(1,3))*2*x[, 1] +
          sample(0:1, 1, prob=c(1,3))*10*x[, 2] +
          sample(0:1, 1, prob=c(1,3))*0.1*x[, 3] +
          sample(0:1, 1, prob=c(1,3))*100*x[, 4] +
          rnorm(n)
      y[y<0] <- 0
      yname <- "Strength"
      xnames <- c("A", "B", "C", "D")
      newx <- rep(0, 4)
      for(i in 1:4) newx[i] <- round(runif(1, min(x[, i]), max(x[, i])), c(1, 2, 1, 3)[i])
      data <- data.frame(Strength=y, A=x[, 1], B = x[, 2], C=x[, 3], D = x[, 4])
      qtxt <- paste0("A chemist wants to find a model to predict the effects of four
          ingredients labeled A, B, C and D on the strenght of a chemical. He repeats the experiment with ",
           n," combinations of the ingredients.")
    }
    if(whichstory == 3) {
      npred <- 6
      n <- sample(50:70, 1)
      x <- matrix(0, n, npred)
      colnames(x) <- 1:npred
      for(i in 1:6)
          x[, i] <- round(runif(n, 10, 100), 1)
      y <- 10+rnorm(n)
      for(i in 1:6) y <- y + sample(-1:1, 1, prob=c(1,1,2))*x[, i]
      y[y<0] <- 0
      yname <- "Strength"
      xnames <- paste0("X", as.character(1:6))
      newx <- rep(0, 6)
      for(i in 1:6) newx[i] <- round(runif(1, min(x[, i]), max(x[, i])), 1)
      data <- data.frame(Strength=y, x)
      qtxt <- paste0("An industrial engineer wants to find a model to predict 
          the effects of six factors labeled X1 to X6 on the time it takes to produce
          a product. He repeats the experiment with ",
           n," combinations of the factors.")
    }
    out <- mlr(y, x, return.result=TRUE)
    const <- out[1]
    coef <- out[2:(npred+1)]
    R2 <- out[npred+2] 
    atxt <- ""
    if(doEquation) {
        qtxt <- paste0(qtxt, "<p>The least squares regression line is 
        (include +/- in your solution): <p>", 
            yname, " =  {:NM:%100%", const, ":", abs(const/100), "}&nbsp;&nbsp;") 
        atxt <- paste0(atxt, "<p>",
            yname, " =  ", const, "&nbsp;&nbsp;") 
        for(i in 1:npred) {      
           qtxt <- paste0(qtxt, "&nbsp;&nbsp;", "{:NM:%100%", coef[i], ":", 
              abs(coef[i]/100), "}", xnames[i])
           atxt <- paste0(atxt, "&nbsp;&nbsp;", ifelse(coef[i]>0, " + ", " - "), 
                  coef[i], xnames[i])                  
           if(i %% 2==0)  {qtxt <- paste0(qtxt, "<br>");atxt <- paste0(atxt, "<br>")}
        }       
    }
    if(doR2) {
       qtxt <- paste0(qtxt, "<p>R<sup>2</sup> = &nbsp;&nbsp;{:NM:%100%", R2, ":0.1}%") 
       atxt <- paste0(atxt, "<p>R<sup>2</sup> = &nbsp;&nbsp;", R2, "%") 
    }    
    if(doPredictEstimate) {
       yval <- const + sum(coef*xval)
       qtxt <- paste0(qtxt, "The estimate of ", yname, " if ", paste0(xnames, "=", newx, ", ", collapse=""),
           "is {:NM:%100%", yval, ":", yval/100, "}")
       atxt <- paste0(atxt, "The estimate of ", yname, " if ", paste0(xnames, "=", newx, ", ", collapse=""),
           "is ", yval)
    }
    if(doPredictInterval) {
        pout <- mlr.predict(y, x, newx=newx, interval="PI")[npred+2:3]
        qtxt <- paste0(qtxt, "<p>A 95% confidence interval for ", yname, " if ",
             paste0(xnames, "=", newx, ", ", collapse=""),
             "is ({:NM:%100%", pout[1], ":", pout[1]/100, "}, 
             {:NM:%100%", pout[2], ":", pout[2]/100, "})")                  
        atxt <- paste0(atxt, "<p>A 95% confidence interval is given by (",
                pout[1], ", ", pout[2], ")")
    } 
    if(doBest) {
       mout <- mallows(y, x, return.result=T)
       best.n <- 1
       best.Cp <- as.numeric(mout[1, "Cp"])
       for(i in 2:4) {  
        xx <- as.numeric(mout[i, "Cp"])
           if(xx<best.Cp) {
            best.n <- i
            best.Cp <- xx
          }
      } 
       
       qtxt <- paste0(qtxt, "<p> Which of the following predictors 
          are part of the best linear model?")
          for(i in 1:npred) {
              qtxt <- paste0(qtxt, "<p>", xnames[i], "&nbsp; {:MC:~", 
                ifelse(mout[best.n, 2+i]==1, "=", ""), "Yes~", 
                ifelse(mout[best.n, 2+i]==0, "=", ""), "No}")
          }
       atxt <- paste0(atxt, "<p> The best linear model (Mallow's 
          C<sub>p</sub>=", best.Cp, ") includes the predictors ")
       for(i in 1:npred) {
          if(mout[best.n, i+2]==1) atxt <- paste0(atxt, xnames[i], "&nbsp")
       }      
    }                    
    list(qtxt = paste0("<h5>", qtxt, "</h5><hr>", moodle.table(data)), 
        atxt = paste0("<h5>", atxt,"</h5>"), 
        category = category, quizname = quizname)     
}   

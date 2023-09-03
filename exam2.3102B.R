exam2.3102A <- function(problem = 1) {
   category <- paste0("Exams/Exam 2/Problem ", problem) 
   quizname <- paste0("problem ", problem)  
   
   if(problem == 1) {   
      n <- 15
      z <- c("Morning", "Afternoon", "Evening")
      x <- rep(z , each=n)
      y <- round(rnorm(45, 50, 10), 2)
      repeat {
        y[16:30] <- y[16:30] + 1
        pval <- oneway(y, x, return.result=TRUE)[1]
        if(pval<0.025) break
      }  
      command1 <- "oneway(Sales, Time)"
      data <- data.frame(Sales = y, Time = x)    
      qtxt <- paste0("A store wants to see whether there are any differences in the sales
        depending on the time of day. They randomly select ", n, " sales receipts from sales 
        that were done in the morning, in the afternoon and in the evening.
        <p>They do a test at the 5% level to see whether there are any 
        differences in the sales . 
        They find a p value of {1:NM:%100%", pval, ":0.001}. Therefore they conclude 
        that there {1:MC:~=are~are no} statistically significant differences 
        between the sales. <hr>", moodle.table(data))
      atxt <- paste0("p value=", pval, "<p>R commands:<p>", command1)
   }    
   if(problem == 2) {   
      n <- sample(30:40, 1)
      x <- 1:n
      y <- round(10*x+rnorm(n, 0, 50))
      y<-sort(y+abs(min(y)))
      out <- slr(y, x, return.result=TRUE)
      xval <- sample(10:20, 1)
      pout <- slr.predict(y, x, newx=xval, interval="PI", conf.level=90, return.result=TRUE)[3:4]
      pout1 <- slr.predict(y, x, newx=xval, interval="PI", return.result=TRUE)[3:4]  
      pout2 <- slr.predict(y, x, newx=xval, interval="CI", conf.level=90, return.result=TRUE)[3:4]
      pout3 <- slr.predict(y, x, newx=xval, interval="CI", return.result=TRUE)[3:4]      
                           
      command1 <- "slr(Amount, Days)"      
      command2 <- paste0("slr.predict(Amount, Days, newx=", xval, ", interval=\"PI\",
            conf.level=90)")      
      qtxt <- paste0("An agronomist wants to study the growth of wheat over time. For ", n, 
        " consecutive days he observes the number of wheat stalks.")
      qtxt <- paste0(qtxt, "<p>The least squares regression line is: 
          <p> Amount =  {:NM:%100%", out[1], ":", out[1]/100, 
              "}&nbsp;&nbsp;+&nbsp;&nbsp;{:NM:%100%", 
              abs(out[2]), ":", out[2]/100, "} &nbsp;&nbsp;Days")
      qtxt <- paste0(qtxt, "<p>R<sup>2</sup> = &nbsp;&nbsp;{:NM:%100%", out[3], ":0.1}%") 
      qtxt <- paste0(qtxt, "<p>A 90% interval estimate for the 
          number of wheat stalks on day ", xval, " is given by
                ({:NM:%100%", pout[1], ":", pout[1]/100,
                  "~%75%", pout1[1], ":", pout1[1]/100,  
                  "~%60%", pout2[1], ":", pout2[1]/100, 
                  "~%50%", pout3[1], ":", pout3[1]/100, "} , 
                {:NM:%100%", pout[2], ":", pout[2]/100, 
                  "~%75%", pout1[2], ":", pout1[2]/100,  
                  "~%60%", pout2[2], ":", pout2[2]/100, 
                  "~%50%", pout3[2], ":", pout3[2]/100, "})
          <p>this is a {:MC:~=Prediction~Extrapolation}")
     
      atxt <- paste0("<p>The least squares regression line is: 
          <p>Amount &nbsp;&nbsp; = &nbsp;&nbsp;", out[1], 
             "&nbsp;&nbsp;+&nbsp;&nbsp;", out[2], "&nbsp;&nbsp;Days")
      atxt <- paste0(atxt, "<p>R<sup>2</sup> = &nbsp;&nbsp;", out[3], "%")                                                     
      atxt <- paste0(atxt, "<p>A 90% prediction interval is given by (",
               pout[1], ", ", pout[2], ")
               <p> this is prediction
               <p>R commands:<p>", command1,"<p>",command2)
      qtxt <- paste0(qtxt, "<hr>", moodle.table(data.frame(Number=y, Day=x))) 
  } 
  if(problem==3) {
      n <- sample(10:15, 1)
      x <- rep(c("Before", "After"), each=n)
      y <- round(c(rnorm(n, 15, 4), rnorm(n, 10, 4)))
      y[y<0] <- 0
      data <- data.frame(Incidents=y, Time=x)
      ci1 <- oneway(y, x, conf.level=90, return.result=TRUE)[2:3]
      ci2 <- oneway(y, x, return.result=TRUE)[2:3]
      command <- "oneway(Incidents, Time, conf.level=90)"
      qtxt <- paste0("A while ago a store implemented a new security system
       to prefent shoplifting. They want to estimate its effectiveness. They randomly
       select ", n," months before and after the new system went into effect.
       <p>A 90% confidence interval for the mean difference in the number of shoplifting
       incidents (Before-After) is given by <p>(
       {:NM:%100%", ci1[1], ":0.1~%75%", ci2[1], ":0.1}, &nbsp;
       {:NM:%100%", ci1[2], ":0.1~%75%", ci2[2], ":0.1})
       <hr>", moodle.table(data))
      atxt <- paste0("A 90% confidence interval for the mean difference in the number of shoplifting
       incidents (Before-After) is given by <p>(", ci1[1], ", &nbsp;", ci1[2],")
       <p>R command: ", command)
  }
  if(problem==4) {
      n <- sample(40:50, size=2)
      repeat {
        y1 <- round(c(rnorm(n[1]-1, 20, 3), 40.3), 1)
        y2 <- round(rnorm(n[2], 23, 3), 1)
        x <- c(rep("With_Water", n[2]), rep("Without_Water", n[1]))
        y <- c(y2, y1)
        pval <- kruskalwallis(y, x, return.result=TRUE)[1]
        if(pval>0.001) break
      }  
      mc <- ifelse(pval<0.05, "{:MC:~=is a~is no}", "{:MC:~is a~=is no}")
      qtxt <- paste0("In an experiment in agriculture the researcher wants
       to see whether there is a difference in the growth depending on whether
       water was given or not. She randomly selects ", n[2], " plants which 
       she waters in the morning. She also selects ", n[1], " plants which she does
       not water. After a while she measures the growth of the plants. 
       Then she tests at the 5% level whether there is a difference.
       She finds a p value of {:NM:%100%", pval,":0.01} and so she concludes that 
       there ", mc, " statistically singificant difference.
       <hr>Growth with water:<p>", paste(y2, collapse=" "), 
       "<p>Growth without water:<p>", paste(y1, collapse=" "))
        
      atxt <- paste0("The data is not in the proper format, so we need to 
        do the following first:
        <p>copy data with water, x1 <- getx()
        <p>copy data without water, x2 <- getx()
        <p>x <- c(rep(\"With_Water\", length(x1)), rep(\"Without_Water\", length(x2)))
        <p>y <- c(x1, x2) 
        <p>Now the boxplot shows an outlier. The log transform does not work, so
        we need to use the nonparametric method
        <p>kruskalwallis(y, x)
        <p> gives a p value of ", pval)
   }
   if(problem==5) {
      qtxt <- "The data set <b>church</b> in Resma3 has the perimeter (in 100 meters) 
        and the area (in 100 square meters) of 25 medieval churches in England. We want
        to find a model to predict the area from the perimeter.
        <p>The linear model is 
            Area = {:NM:%100%-6.745:0.1}&nbsp;+&nbsp; {:NM:%100%12.088:0.1}&nbsp;Perimeter.&nbsp;&nbsp;
            <br>This is a {2:MC:~good~=bad} model          
        <p>The exponential model is 
            log(Area) = {:NM:%100%0.386:0.1}&nbsp;+&nbsp; {:NM:%100%0.939:0.1}&nbsp;Perimeter.&nbsp;&nbsp;
            This is a {2:MC:~good~=bad} model
        <p>The y vs log(x) model is 
            Area = {:NM:%100%8.679:0.1}&nbsp;+&nbsp; {:NM:%100%19.127:0.1}&nbsp;log(Perimeter).&nbsp;&nbsp;
            This is a {2:MC:~good~=bad} model
        <p>The power model is 
            log(Area) = {:NM:%100%1.499:0.1}&nbsp;+&nbsp; {:NM:%100%1.683:0.1}&nbsp;log(Perimeter).&nbsp;&nbsp;
            This is a {2:MC:~=good~bad} model    
        <p>The quadratic model is 
            Area = {:NM:%100%-5.036:0.1}&nbsp;+&nbsp; {:NM:%100%9.913:0.1}&nbsp;Perimeter
            &nbsp;+&nbsp; {:NM:%100%0.475:0.1}&nbsp;Perimeter^2.&nbsp;&nbsp;
            This is a {2:MC:~=good~bad} model    
        <p>Among the models above the best one is the 
        {5:MC:~linear~exponential~y vs log(x)~=power~quadratic}."                      
        
      atxt <- "Linear model: Area  = -6.745 + 12.088 Perimeter.
             <br>This is a bad model  (bad residual vs fits plot)
             <p>Exponential model: log(Area)  = 0.386 + 0.939 Perimeter
             <br>This is a bad model  (bad residual vs fits plot)
             <p>y vs log(x): Area  = 8.679 + 19.127 log(Perimeter)
             <br>This is a bad model  (bad residual vs fits plot)
             <p>power model: log(Area)  = 1.499 + 1.683 log(Perimeter) 
             <br>This is a good model  (good residual vs fits plot), R^2=99%
             <p>quadratic model:Area  = -5.036 + 9.913 Perimeter +0.475Perimeter^2 
             <br>This is a bad model  (bad residual vs fits plot)
             <p> the power model is the only good one, so it is also the best one"
   }      
#print(qtxt)
#print(atxt) 
  list(qtxt = paste("<h5>", qtxt, "</h5>"),
     atxt = paste("<h5>", atxt, "</h5>"), 
     category = category, quizname = quizname) 
} 

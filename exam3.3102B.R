exam3.3102B <- function(problem = 1) {
   category <- paste0("Exams / Exam 3 /Problem ", problem) 
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
      out <- tukey(y, x, show.all=TRUE)
      out1 <- tukey(y, x)
      pairs <- strsplit(out[,1],"-")
      tpvals <- c(0, 0, 0)
      for(i in 1:3) {
        if("Morning" %in% pairs[[i]] & "Afternoon" %in%  pairs[[i]])
           tpvals[1] <- out[i, 2]
        if("Morning" %in% pairs[[i]] & "Evening" %in%  pairs[[i]])
           tpvals[2] <- out[i, 2]
        if("Evening" %in% pairs[[i]] & "Afternoon" %in%  pairs[[i]])
           tpvals[3] <- out[i, 2]
      }
      command1 <- "oneway(Sales, Time)"
      command2 <- "tukey(Sales, Time)"
      m <- nrow(out)
      pairs <- strsplit(rownames(out),"-")
      data <- data.frame(Sales = y, Time = x)    
      qtxt <- paste0("A store wants to see whether there are any differences in the sales
        depending on the time of day. They randomly select ", n, " sales receipts from sales 
        that were done in the morning, in the afternoon and in the evening.
        <p>They do a test at the 5% level to see whether there are any differences in the sales . 
        They find a p value of {1:NM:%100%", pval, ":0.001}. Therefore they conclude that 
        there {1:MC:~=are~are no} statistically significant difference between the sales.
        <p>Specifically they find that the difference between
        <p>Morning and Afternoon ", ifelse(tpvals[1]<0.05, "{1:MC:~=is~is not}",  
            "{1:MC:~is~=is not}"), " statistically significant 
        <p>Morning and Evening ", ifelse(tpvals[2]<0.05, "{1:MC:~=is~is not}",  
            "{1:MC:~is~=is not}"), " statistically significant 
        <p>Evening and Afternoon ", ifelse(tpvals[3]<0.05, "{1:MC:~=is~is not}",  
            "{1:MC:~is~=is not}"), " statistically significant 
        <hr>", moodle.table(data))
      htxt <- " "  
      atxt <- paste0("p value=", pval, "<p>Result of tukey command:<p>",
        moodle.table(out1), "<p>R commands:<p>", command1,"<p>",command2)                               
   }    
   if(problem == 2) {   
      n <- sample(35:40, 1)
      x <- 10:n
      y <- round(10*x+rnorm(length(x), 0, 25))
      y <- sort(y+abs(min(y)))
      out <- slr(y, x, return.result=TRUE)
      xval <- sample(10:20, 1)
      pout <- slr.predict(y, x, newx=xval, interval="PI", conf.level=90)[3:4]
      pout1 <- slr.predict(y, x, newx=xval, interval="PI")[3:4]  
      pout2 <- slr.predict(y, x, newx=xval, interval="CI", conf.level=90)[3:4]
      pout3 <- slr.predict(y, x, newx=xval, interval="CI")[3:4]      
                           
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
      htxt <- " "
      atxt <- paste0("<p>The least squares regression line is: 
          <p>Amount &nbsp;&nbsp; = &nbsp;&nbsp;", out[1], 
             "&nbsp;&nbsp;+&nbsp;&nbsp;", out[2], "&nbsp;&nbsp;Days")
      atxt <- paste0(atxt, "<p>R<sup>2</sup> = &nbsp;&nbsp;", out[3], "%")                                                     
      atxt <- paste0(atxt, "<p>A 90% prediction interval is given by (",
               pout[1], ", ", pout[2], ")
               <p> this is prediction
               <p>R commands:<p>", command1,"<p>",command2)
      qtxt <- paste0(qtxt, "<hr>", moodle.table(data.frame(Amount=y, Days=x))) 
  } 
  if(problem==3) {
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
      htxt <- " "  
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
   if(problem==4) {
      qtxt <- paste("The data set crop in Resma3 has the yield (amount) of a crop 
        grown using one of eight different watering schedules.
        <p>&nbsp;<p>The p value of the test to see whether there are statistically 
        significant differences between the watering schedules is", 
        nm(c(0, 0.0473), c(100, 50), eps=0.01),
        "<p>&nbsp;<p>Further analysis shows that the difference between
         SCHEDULE 1 and SCHEDULE 2 {1:MC:%0%is~%100%is not} statistically significant")
      htxt <- ""
      atxt <- paste("The box plot of YIELD by SCHEDULE shows several outliers, a 
         log transform fixes that problem. oneway(log(YIELD), SCHEDULE) gives a 
         p value of 0.000.
         <p>&nbsp;<p>tukey(log(YIELD), SCHEDULE) does not include 1-2, so 
         this difference is not statistically significant. ")   
   
   } 
   if(problem==5) {
       qtxt <- paste("The effect of oxygen level on fermentation end products 
         was examined in the article \"Effects of Oxygen on Pyruvate FormateLyase in 
         Situ and Sugar Metabolism of Streptococcusmutans and Streptococcus 
         samguis\"
         (Infection and Immunity 1985, p129-134) Four oxygen concentrations
         (0, 46,92, 138 microM) and two sugar types (galactose and glucose)
         were used. The amount of ethanol was measured for each oxygen-sugar 
         combination and each measurement was repeated twice. The data is in 
         <b>fermentation</b> in Resma3.
         <p>In this analysis  treat Oxygen as a categorical variable.
         <p>&nbsp;<p>There ", mc(5, c(0, 100)), " evidence of interaction.
         <p>&nbsp;<p>Sugar ", mc(5, c(100, 0)), " statistically significant 
         (p value =", nm(c(0.002, 0.006), c(100, 50), ndigit=3), "),
         <p>&nbsp;<p>Oxygen ", mc(5, c(0, 100)), " statistically significant 
         (p value=", nm(c(0.064, 0.112), c(100, 50), ndigit=2), ")")
       htxt <- ""
       atxt <- "twoway(Ethanol,Sugar, Oxygen) gives a p-value of 0.728 for
          the interaction, so it is not statistically significant.
          <p>twoway(Ethanol,Sugar, Oxygen, with.interaction = FALSE) shows 
          that Sugar is stat. significant (p=0.006) but Oxygen is not (p=0.064)"

   }   

   list(qtxt = paste("<h5>", qtxt, "</h5>"),
       htxt = paste("<h5>", htxt, "</h5>"),
       atxt = paste("<h5>", atxt, "</h5>"),
       category = category, quizname = quizname) 
} 

exam2.3102A <- function(problem, Show=FALSE) {
   if(missing(problem)) problem <- sample(1:5, 1)
   category <- paste0("Exams/Exam 2/Problem ", problem) 
   quizname <- paste0("problem ", problem)  
   
   if(problem == 1) {   
#---------------
#ANOVA
#---------------   
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
        They find a p value of {3:NM:%100%", pval, ":0.001}. Therefore they conclude 
        that there {1:MC:~=are~are no} statistically significant differences 
        between the sales. <hr>", moodle.table(data))
      atxt <- paste0("p value=", pval, "<p>R commands:<p>", command1)
   }    
   if(problem == 2) {   
#------------------
#Wilcoxon Rank Sum
#------------------   
      n <- sample(30:40, 1)
      repeat {
        x <- c(sample(10:20, 1), sort(round(rnorm(40, 100, 15))), sample(220:230, 1))
        pval <- one.sample.wilcoxon(x, med.null = 110, return.result = TRUE)                      
        if(pval>0) break
      }  
      command <- "one.sample.wilcoxon(Minutes, med.null = 110)"
      pval1 <- one.sample.t(x, mu.null = 110, return.result = TRUE)                      
      qtxt <- paste0("A chemist has developed a method for changing the time a certain 
          chemical reaction takes. In the past it was 110 minutes. He does an experiment 
          with his new method. The times are below. He tests at the 5% level whether 
          the average reaction time is now different and finds a p value of ", 
          nm(c(pval, pval1), c(100, 50), eps=0.001), "<hr>", moodle.table(x))     
      atxt <- paste0("The boxplot of the minutes shows several outliers, so we need to 
          use the Wilcoxon test. The pvalue is ", pval, 
          "<p>R command:<p>", command)
       
  } 
  if(problem==3) {
#----------------------
#Two- Sample, ANOVA
#----------------------
      n <- sample(10:15, 1)
      x <- rep(c("Before", "After"), each=n)
      y <- round(c(rnorm(n, 15, 4), rnorm(n, 10, 4)))
      y[y<0] <- 0
      data <- data.frame(Incidents=y, Time=x)
      ci1 <- oneway(y, x, conf.level=90, return.result=TRUE)
      ci2 <- oneway(y, x, return.result=TRUE)
      command <- "oneway(Incidents, Time, conf.level=90)"
      qtxt <- paste0("A while ago a store implemented a new security system
       to prevent shoplifting. They want to estimate its effectiveness. They randomly
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
#----------------------
#Correlation test
#----------------------
      n <- sample(30:31, 1)
      x <- 1:n
      repeat {
        y <- abs(100+1:n+round(rnorm(n, 0, 50)))
        pval <- pearson.cor(x, y, rho.null=0, return.result=TRUE)[2]
        if(pval>0.01 & pval<0.05) break
      }  
      data <- data.frame(Day=x, Sales=y)
      qtxt <- paste("A store manager want to know whether there is a relationship between 
          the day of the month and the number of sales. He tests at the 5% level and finds 
          a p value of ", nm(pval, eps=0.001, pts=3), ". He therefore concludes that there ",
          mc(c("is a statistically significant", "is no statistically significant"), c(100, 0)),
          " relationship<hr>", moodle.table(data))
      atxt <- paste("p value = ", pval, 
          "<p>R command: pearson.cor(Day, Sales, rho.null = 0)")
  }
  if(problem==5) {
#------------------------
#Kruskall-Wallis
#------------------------  
      n <- sample(40:50, size=2)
      repeat {
        y1 <- round(c(rnorm(n[1]-1, 20, 3), 40.3), 1)
        y2 <- round(rnorm(n[2], 23, 3), 1)
        x <- c(rep("With_Water", n[2]), rep("Without_Water", n[1]))
        y <- c(y2, y1)
        pval <- kruskalwallis(y, x, return.result=TRUE)[1]
        if(pval>0.001) break
      }  
      pval1 <- oneway(y, x, return.result=TRUE)[1]
      mc <- ifelse(pval<0.05, "{:MC:~=is a~is no}", "{:MC:~is a~=is no}")
      qtxt <- paste0("In an experiment in agriculture the researcher wants
       to see whether there is a difference in the growth depending on whether
       water was given or not. She randomly selects ", n[2], " plants which 
       she waters in the morning. She also selects ", n[1], " plants which she does
       not water. After a while she measures the growth of the plants. 
       Then she tests at the 5% level whether there is a difference.
       She finds a p value of ", nm(c(pval, pval1), c(100, 75), eps=0.001, pts=3), " and so she concludes that 
       there ", mc, " statistically significant difference.
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
   if(Show) {     
      print(qtxt)
      print(atxt) 
  }    
  list(qtxt = paste("<h5>", qtxt, "</h5>"),
     atxt = paste("<h5>", atxt, "</h5>"), 
     category = category, quizname = quizname) 
} 

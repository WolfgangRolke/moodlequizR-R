exam1.3101 <- function(problem = 1) {
   category <- paste("Exams / Exam 1 / Problem ", problem) 
   quizname <- "problem - " 
    
   if(problem == 1) {   
      n <- sample(101:149, 1)
      x <- round(rnorm(n, runif(1, 50, 70), runif(1, 5, 15)), 1)
      mu <- round(mean(x), 2)
      sig <- round(sd(x), 2)
      per <- sample(10*1:9, 1)
      qnt <- round(quantile(x, per/100), 2)
      qtxt <- paste0("<b>Problem 1<b>: For the data below find the following:
          <p>The sample size is ", nm(n), 
          "<p>The mean is ", nm(c(mu, mu), c(100, 80), eps = c(0, 0.1)),
          "<p>The standard deviation is ", nm(c(sig, sig), c(100, 80), eps = c(0, 0.1)),
          "<p>The ", per, "<sup>th</sup> <nolink>percentile</nolink> is ", 
                nm(c(qnt, qnt), c(100, 80), eps = c(0, 0.1)),
          "<p><hr>", moodle.table(x))
          
       atxt <- paste0("The sample size is ", n, 
          "<p>The mean is ", mu ,
          "<p>The standard deviation is ", sig,      
          "<p>The ", per, "<sup>th</sup> percentile is ", qnt)
          
  }
  if(problem == 2) {
      k <- sample(300:350, 1)
      n1 <- nrow(subset(upr, IGS > k))
      n2 <- nrow(subset( upr , IGS > k & Gender == "M" ))
      n3 <- nrow(subset(upr, IGS >= k))
      n4 <- nrow(subset( upr , IGS >= k & Gender == "M" ))
      qtxt <- paste0("How many applicants in the upr data set had an IGS higher than ", k, 
          "? ", nm(c(n1, n3), c(100, 75)),
          "<p>&nbsp<p>How many male applicants in the upr data set had an IGS higher than ", k, 
          "? ", nm(c(n2, n4), c(100, 75)))
      atxt <- paste0("Use isubset command.<p>
            Applicants with IGS higher than ", k, ": ", n1,
            "<p>&nbsp<p>Male applicants with IGS higher than ", k, ": ", n2)
  }
  if(problem == 3) {
      source("C:\\Users\\Wolfgang\\Dropbox\\teaching\\moodle\\contingency.table.R")
      out <- contingency.table(c(2, 3), with.marginals=FALSE)
      qtxt <- out$qtxt
      atxt <- out$atxt
      remove(list="contingency.table", pos=.GlobalEnv)
  }
  if(problem == 4) {
      source("C:\\Users\\Wolfgang\\Dropbox\\teaching\\moodle\\zscore.R")
      out <- zscore(4)
      qtxt <- out$qtxt
      atxt <- out$atxt
      remove(list="zscore", pos=.GlobalEnv)
  }
  if(problem==5) {
      n <- 10*sample(50:100, 1)
      xbar <- round(runif(1, 40, 60), 1)
      sig <- round(runif(1, 10, 20), 1)
      qtxt <- paste0("In a survey ", n, " people they were asked how many minutes
         they spent yesterday using their smart phone. The mean was ", xbar, " minutes
         with a standard deviation of ", sig, ".
         <p>&nbsp<p>According to the <nolink>empirical rule</nolink> ", nm(0.95*n, eps=0.999), " of the
         observations should be in the interval (", nm(xbar-2*sig, eps=0.1), ", ", 
         nm(xbar+2*sig, eps=0.1), ").
         <p>&nbsp<p>Most likely in this case the <nolink>empirical rule</nolink> ", 
         mc(c("would", "would not"), w=c(0, 100)), " work .")
     atxt <- paste("X&#772;-2 = ", round(xbar-2*sig, 1),
          "<p>X&#772+2s = ", round(xbar+2*sig, 1),
          "<p>95% of ", n, " is ", round(0.95*n))
         
  } 
  if(problem==6) {
      qtxt <- paste("For the Freshmen GPAs of the <b>upr</b> data find
      <p><table><tr><th>Minimum</th><th>Q<sub>1</sub></th><th>Median</th>
        <th>Q<sub>3</sub></th><th>Maximum</th></tr>
      <tr><td>", nm(0), "</td><td>", nm(2.32,eps=0.1), "</td><td>",
      nm(2.83, eps=0.1), "</td><td>", nm(3.28, eps=0.1), "</td><td>",  
      nm(4), "</td></tr></table>
      <p>&nbsp;<p>one of these numbers is quite strange, namely the ",
      mc(c("Minimum","Q1","Median","Q3","Maximum"), w=c(100,0,0,0,0)))
      
      atxt <- paste("attach(upr)<p>fivenumber(Freshmen.GPA, ndigit=2)<p> shows <p>
      <table><tr><th>Minimum</th><th>Q<sub>1</sub></th><th>Median</th>
        <th>Q<sub>3</sub></th><th>Maximum</th></tr>
      <tr><td>", 0, "</td><td>", 2.32, "</td><td>",
      2.83, "</td><td>", 3.28, "</td><td>",  
      4, "</td></tr></table>")
      
  } 
  if(problem==7) {
      n <- sample(50:70, 1)
      x <- sort(round(runif(n, 10, 20)*10))
      y <- round(30 + x/3 + rnorm(n, 0, 5))
      y[y>100] <- 100
      cr <- round(cor(x, y), 3)
      df <- data.frame(Time=x, Score=y)
      
      qtxt <- paste0("A company is interested in the relationship between the time (in minutes) an employee
               has spent in training courses and the score on an assessment test. The data
               is below.<p> The correlation between time and score is 
               <p>", nm(cr, c(100, 75), eps=c(0, 0.001)),                
               "<p><hr>", moodle.table(df))
       atxt <- paste0("round(cor(Time, Score), 3) = ", cr)        
  
  }
  qtxt <- paste0("<h5>", qtxt,   "</h5>")
  atxt <-paste("<h5>", atxt, "</h5>")   
  list(qtxt = qtxt, 
     atxt = atxt, 
     category = category, quizname = quizname) 
} 

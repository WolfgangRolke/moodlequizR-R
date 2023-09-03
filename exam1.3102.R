exam1.3102 <- function(problem = 1) {
   category <- paste("Exams / Exam 1 / Problem ", problem) 
   quizname <- "problem - " 
   source("C:/Users/Wolfgang/Dropbox/teaching/moodler/qatxts.R")
   if(problem == 1) {   
      n <- sample(100:150, 1)
      x <- rbinom(1, n, 0.4)
      int <- 100*one.sample.prop(x = x, n = n, conf.level = 90, return.result = TRUE)
      int1 <- 100*one.sample.prop(x = x, n = n, return.result = TRUE)
      command <- paste0("one.sample.prop(x = ", x, ", n = ", n, ", conf.level = 90)")      
      atxt <- paste0("<p>R command: ", command, collapse = "")       
      ci <- one.sample.prop(x, n, conf.level=90, return.result=TRUE)
      qtxt <- paste("<b>Problem 1<b>: An economist randomly selects", n, "days from 
            a certain year. He find that on", x, " of them the stock market went up.
            A 90% confidence interval for the percentage of days the market goes up is 
            given by (", paste0("{:NM:%100%",int[1],"~%80%",int1[1],"~%60%",int[1]/100,"~%50%",int1[1]/100, "}"), " , "
              , paste0("{:NM:%100%",int[2],"~%80%",int1[2],"~%60%",int[2]/100,"~%50%",int1[2]/100), "} )")
       atxt <- paste0("A 90% confidence interval for the percentage of days the market goes up is given by (",
          int[1], "%, ", int[2], "%)", atxt) 
                
  }
  if(problem == 2) {
      n <- sample(40:60, 1)
      repeat {
        x <- round(rnorm(n, 4, 2), 1)
        x[x<1] <- 1
        if(mean(x)<5) break
      }  
      pval <- one.sample.t(x, mu.null = 5.0, alternative = "greater", return.result = TRUE)
      pval1 <- one.sample.t(x, mu.null = 5.0, return.result = TRUE)
      command <- paste0("one.sample.t(x, muNull = 5.0, alternative=\"greater\")")      
      txts <- qatxts( par = "mean", assumption = "ok", alpha = 0.05, h0num = 5.0,
                  alt = "less", pval = c(pval, pval1))
      
      atxt <- paste0( txts$a[1:6] , collapse = "")       
      atxt <- paste0( atxt, "<p>", "<p>R command: ", command, collapse = "")    
      
      qtxt <- paste("<b>Problem 2<b>: A researcher has developed a new treatment for 
      an upset stomach. Using the old treatment it used to take 5 hours to feel better. The
      researcher randomly chose ", n, " subjects and gave them the new treatment. The subjects
      reported the following times until they felt better:<br>", paste(x, collapse= " "), "<br>Test at the 5% level
      whether this new treatment is better than the old one.")
      qtxt <- paste(qtxt, paste0(txts$q[1:6] , collapse = ""))
  }
#  print(qtxt)
#  print(atxt)
  qtxt <- paste0("<h5>", qtxt,   "</h5>")
  atxt <-paste("<h5>", atxt, "</h5>")  
  list(qtxt = qtxt, 
     atxt = atxt, 
     category = category, quizname = quizname) 
} 

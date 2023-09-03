exam3.3015 <- function(problem) {
   category <- paste0(" Exams / Exam 3   / Problem = ", problem)
   quizname <- "problem - " 
   
   with.data <- FALSE
   if(problem==1) {
      xbar <- round(runif(1, 10.5, 11.5), 1)
      sigma <- round(runif(1, 1.5, 2.5), 1)
      sgns <- c("=", "&ne;", ">", "<")
      mc.par <- mc( c("&mu;", "&pi;", "&rho;", "&lambda;"), c(100, 0, 0, 0))       
      qtxt <- paste0("A psychologist has given a test designed to measure their happiness
          to a group of ", sample(30:50, 1), " people. This test is designed to give scores
          from 0 to 20, with a mean of 10. The subjects in her group got a mean score of ", 
          xbar, " with a standard deviation of ", sigma, ".
          <p>The psychologist wants to know whether the mean happiness in her group is 
          statistically significantly higher than the overall one, so she tests
          <p>&nbsp;<p>H<sub>0</sub>: ", mc.par, "&nbsp;", mc(sgns, c(100, 0, 0, 0)), 
          "&nbsp;", nm(10), "&nbsp;vs H<sub>a</sub>: ", mc.par, "&nbsp;" , 
          mc(sgns, c(0, 0, 100, 0)), "&nbsp;", nm(10))
      atxt <- "H<sub>0</sub>: &mu; = 10 vs H<sub>a</sub>: &mu; > 10"          
   } 
   if(problem==2) {
        n <- sample(10:20,  1)
        repeat {
          xbar <- round(runif(1, 60, 70), 1)
          sig <- round(runif(1, 3, 5), 1)
          if(all(c(xbar&&1>0, sig%%1>0))) break
        }  
        ci <- one.sample.t(y=xbar, shat=sig, n=n, conf.level=90, return.result=TRUE)
        qtxt <- paste0("An economist wants to estimate the mean price of gasoline.
        He records the prices of gasoline at ", n, " gas stations and finds a mean 
        price of ", xbar, "cents with a standard deviation of ", sig, "cents. So a 90% 
        confidence interval for the mean price is 
        <p>&nbsp;<p>(", nm(ci[1], eps=0.1), ", ",  nm(ci[2], eps=0.1), ")")
        
        atxt <- paste0("R command: one.sample.t(y=", xbar, ", shat=", sig, ", n=", n, 
         ", conf.level=90) yields interval (", ci[1], ", ", ci[2], ")") 
   }
   if(problem==3) {
        n <- sample(50:70, 1)
        x <- round(rnorm(n, 19, 3), 1)
        with.data <- TRUE
        pval <- one.sample.t(x, mu.null=21, alternative="less", return.result=TRUE)
        pval1 <- one.sample.t(x, mu.null=21, return.result=TRUE)
        if(pval<0.001) pval <- 0
        qtxt <- paste0("The employees of a certain company have recently undergone
          some training to shorten the time it takes them to do a certain job. In the past
          it took them on average 21 minutes. Now the company wants to see whether
          this training has been effective. So they randomly select ", n, " employees
          performing this task. The minutes it took each employee are below. Test 
          at the 5% level whether the training was successful.<p>&nbsp;")
        qtxt <- paste(qtxt, "<p>&alpha; = ", nm(0.05))  
        sgns <- c("=", "&ne;", ">", "<")
        mc.par <- mc( c("&mu;", "&pi;", "&rho;", "&lambda;"), c(100, 0, 0, 0)) 
        qtxt <- paste(qtxt, "<p>H<sub>0</sub>: ", mc.par, "&nbsp;", 
          mc(sgns, c(100, 0, 0, 0)), "&nbsp;", nm(21), "&nbsp;vs H<sub>a</sub>: ", 
          mc.par, "&nbsp;" , mc(sgns, c(0, 0, 0, 100)), "&nbsp;", nm(21))
        qtxt <- paste(qtxt, "<p><nolink>p value</nolink> = ", nm(c(pval, pval1), c(100,75), eps=0.01))  
        if(pval<0.05) mcc <- mc(c("worked", "didn't work"), c(100, 0))
        else mcc <- mc(c("worked", "didn't work"), c(0, 100))
        qtxt <- paste(qtxt, "<p>so the company concludes that the training", mcc)
        
        atxt <- paste("alpha=0.05
        <p>H<sub>0</sub>: &mu; = 21 vs H<sub>a</sub>: &mu; < 21
        <p><nolink>p value</nolink> = ", pval, 
       "<p>p ", ifelse(pval<0.05, "<", ">"), " 0.05, so we ",
        ifelse(pval<0.05, "reject", "fail to reject"), " the null hypothesis, the
        training ", ifelse(pval<0.05, "worked", "didn't work")) 
   }
   if(problem==4) {
      n <- sample(20:40, 1)
      mu1 <- round(runif(1, 3, 5), 1) 
      sig <- round(runif(1, 3, 5), 1) 
      mu2 <- mu1 + sample(c(-1, 1), 1)*sig*round(runif(1, 2.5, 3.5)/sqrt(n), 1) 
      pow <- t.ps(n=n, diff=mu2-mu1, sigma=sig, return.result=TRUE)
      qtxt <- paste0("A marine biologist is planning a survey of a certain area. He
        will be counting the manatees per square mile. In a similar survey some years 
        ago the mean number of manatees per square mile was ", mu1, ". If he surveys ",
        n, " square miles and if the true number of manatees now is ", mu2, ", what 
        would be the power of the test? Assume he tests at the 5% level and that 
        the standard deviation is ", sig, "<p>&nbsp;<p>Power = ",  nm(pow))
      atxt <- paste0("R command: t.ps(n=", n, ", diff=", mu2, "-", mu1, ", sigma=", sig, ") 
          yields a power of ", pow)    
   } 
   if(problem==5) {
      std = round(runif(1, 20,40))
      err = round(runif(1, 3, 5), 1)   
      n1 = t.ps(sigma=std, E=err,  conf.level=90, return.result = TRUE)
      n2 = t.ps(sigma=std, E=err,  return.result = TRUE)
      qtxt <- paste0("A shop is planning on doing a survey of their customers. They will collect sales receipts
          and record the dollar amount. Then they will find a 90% confidence interval for the true
          mean sales amount. If the standard deviation is $", std, " and they want the interval to have an 
          error of $", err, ", they will need to include ", nm(c(n1,n2), c(100, 75)), " customers in their survey.")
      atxt <- paste0("R command : t.ps(sigma=", std, ", E=", err, "conf.level=90) yields n= ", n1)
   }
   if(problem==6) {
       ok=rep("good", 10)
       k=sample(1:10, 2)
       ok[k]="bad"
       ci=matrix(0, 10, 2)
       for(i in 1:10) { 
            ci[i, ]= 10+c(-1,1)*round(runif(1, 1,2), 2)
            if(i==k[1]) ci[i, ]=sort(10-c(1,3)*round(runif(1, 1.01,1.99), 2))
            if(i==k[2]) ci[i, ]=10+c(1,3)*round(runif(1, 1.01,1.99), 2)
            
       }
       qtxt=paste0("Each day a chemist is doing an experiment to test whether a certain chemical increases 
         the amount of some substance. If in reality the the amount increases by 10.3 gram, decide for each 
         day whether or not he found a good interval")
         for(i in 1:10) {
           qtxt=paste0(qtxt, "<p>Day ",i,":&nbsp;&nbsp; (",ci[i,1],",&nbsp;", ci[i,2],")&nbsp;&nbsp;", 
             mc(c("good","bad"), c(ifelse(ok[i]=="good", 100, 0),ifelse(ok[i]=="good", 0, 100))), " interval")
         }
       atxt= ""
       for(i in 1:10) {
           atxt=paste0(atxt, "<p>Day ",i,": Interval ", ifelse(ok[i]=="good", "includes", "does not include"),
           " 10, so it is a ", ok[i], " interval")
         }
   }
   if(problem==7) {
      err=c("committed no error", "comitted the type I error", "comitted the type II error")
      cases=c(paste0("If she finds a p value of ", round(runif(1,0.05,0.09), 4), " and if the therapy really works, she ",
              mc(err, c(100,  0, 0))$qmc),
              paste0("If she finds a p value of ", round(runif(1,0.11,0.2), 4), " and if the therapy really works, she ",
              mc(err, c(0,  0, 100))$qmc),
              paste0("If she finds a p value of ", round(runif(1,0.05,0.09), 4), " and if the therapy really does not work, she ",
              mc(err, c(0,  100, 0))$qmc),
              paste0("If she finds a p value of ", round(runif(1,0.11,0.2), 4), " and if the therapy really does not work, she ",
              mc(err, c(100,  0, 0))$qmc))
      I=sample(1:4)
      qtxt=paste0("A psychologist wants to test whether a certain type of therapy makes her patients less anxious. 
          She randomly selects 25 patients and gives them the therapy. She then tests at the 10% level the 
          null hypothesis to see whether the therapy worked.<p><p>",
          paste("Case ", 1:4, ": ", cases[I],collapse="<p>"))
      atxt <- c("$$p\\lt\\alpha$$, so she rejects the null hypothesis. The null hypothesis is false, so she made the correct decision and did not commit an error.",    
                "$$p>\\alpha$$, so she fails to reject the null hypothesis. The null hypothesis is false, so she made the wrong decision and committed the type II error.",
                "$$p\\lt\\alpha$$, so she rejects the null hypothesis. The null hypothesis is true, so she made the wrong decision and committed the type I error.",
                "$$p>\\alpha$$, so she fails to reject the null hypothesis. The null hypothesis is true, so she made the correct decision and did not commit an error")               
      atxt= paste0(rep("Case ",4), 1:4, ": ", atxt[I],collapse="<p>")             

   }

   htxt <- ""     
   list(qtxt = paste0("<h5>", qtxt, "</h5>", ifelse(with.data, moodle.table(x), "")),
        htxt = paste0("<h5>", htxt, "</h5>"), 
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 

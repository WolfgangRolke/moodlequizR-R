exam3.3101 <- function(problem) {
   category <- paste0(" Exams / Exam 3   / Problem = ", problem)
   quizname <- "problem - " 
   
   with.data <- FALSE
   if(problem==1) {
   txt <- c("Yes", "No", "We can't say")
      old <- sample(10:20, 1)
      pract <- sample(3:5, 1)
      ci <- c(old-pract-sample(10:20/10, 1), old-pract+sample(10:20/10, 1))
      mca1 <- mc(c("rejected", "failed to reject"), c(100, 0))$qmc
      mca2 <- mc(txt, c(0, 0, 100))$qmc
      qtxt <- paste0("In the past ", old,"% of the parts made by a machine were faulty. The
        company has tried to improve the machine. After the redesign they test a number
        of parts and find a 95% confidence interval for the true percentage of 
        faulty parts to be (", ci[1], "%, ", ci[2],"%). 
        <p>If they had done a hypothesis test at the 5% level to see whether the changes
        lowered the rate of faulty parts they would have ", mca1, " the null.
        <p>Before the redesign they had decided that to be useful the fault rate would
        have to be no more than ", old-pract, "%. Were the changes in 
        fact such a success? ", mca2)
        htxt <- ""
     atxt <- paste0("The null hypothesis would have been rejected, the interval is below the 
          old failure rate.
          <p>We can not be sure that the change because ", old-pract, "% is in the interval") 
   }
   if(problem==2) {
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
   if(problem==3){
#choose from Hypothesis Testing / Power      
   } 
   if(problem==4) {
        n <- sample(10:20, 1)
        xbar <- round(runif(1, 60, 70), 1)
        sig <- round(runif(1, 3, 5), 1)
        ci <- one.sample.t(y=xbar, shat=sig, n=n, conf.level=90, return.result=TRUE)
        qtxt <- paste0("An economist wants to estimate the mean price of gasoline.
        He records the prices of gasoline at ", n, " gas stations and finds a mean 
        price of ", xbar, "cents with a standard deviation of ", sig, "cents. So a 90% 
        confidence interval for the mean price is 
        <p>&nbsp;<p>(", nm(ci[1], ndigit=1), ", ",  nm(ci[2], ndigit=1), ")")
        
        atxt <- paste0("one.sample.t(y=", xbar, ", shat=", sig, ", n=", n, 
         ", conf.level=90)") 
   }
   if(problem==5) {
        n <- sample(20:30, 1)
        x <- round(rnorm(n, 19, 3), 1)
        with.data <- TRUE
        pval <- one.sample.t(x, mu.null=21, alternative="less", return.result=TRUE)
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
        qtxt <- paste(qtxt, "<p><nolink>p value</nolink> = ", nm(pval, eps=0.01))  
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
   if(problem==6) {
      n <- sample(20:40, 1)
      mu1 <- round(runif(1, 3, 5), 1) 
      sig <- round(runif(1, 3, 5), 1) 
      mu2 <- mu1 + sample(c(-1, 1), 1)*sig*round(runif(1, 2.5, 3.5)/sqrt(n), 1) 
      pow <- t.ps(n=n, diff=mu2-mu1, sigma=sig, return.result=TRUE)
      qtxt <- paste("A marine biologist is planning a survey of a certain area. He
        will be counting the manatees per square mile. In a similar survey some years 
        ago the mean number of manatees per square mile was ", mu1, ". If he surveys ",
        n, " square miles and if the true number of manatees now is ", mu2, ", what 
        would be the power of the test? Assume he tests at the 5% level and that 
        the standard deviation is ", sig,
        "<p>&nbsp;<p>Power = ",  nm(pow))
      atxt <- paste("t.ps(n=", n, ", diff=", mu2, "-", mu2, ", sigma=", sig, ")")    
   } 

   
   htxt <- ""     
   list(qtxt = paste0("<h5>", qtxt, "</h5>", ifelse(with.data, moodle.table(x), "")),
        htxt = paste0("<h5>", htxt, "</h5>"), 
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 

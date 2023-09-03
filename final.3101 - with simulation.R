final.3101 <- function(problem) {
   category <- paste0(" Final Exam / Problem = ", problem)
   quizname <- "problem - " 
   htxt <- paste("  ")   
   parlist <- c("&mu;", "&pi;", "&sigma;", "&lambda;", "&rho;", "other")
   sgnlist <- c("=", "<", ">")
   df <- ""
   if(problem==1) {
        n <- sample(101:121, 1); mu <- sample(20:50, 1); sig <- sample(2:5, 1)
        x <- round(rnorm(n, mu, sig), 1)
        df <- x
        xbar <- round(mean(x), 2)
        med <- round(median(x), 2)
        shat <- round(sd(x), 2)
        p90 <- round(quantile(x, 0.9), 2)
        q3 <- round(quantile(x, 0.75), 2)
        iqr <- round(IQR(x), 2)
        qtxt <- paste("for the data below find:
            <p>&nbsp;<p>Mean = ", nm(xbar, w=c(100, 75, 50),eps=c(0,0.05, 0.1)),
           "<p>&nbsp;<p>Median = ", nm(med, w=c(100, 75, 50),eps=c(0,0.05, 0.1)),
           "<p>&nbsp;<p>Standard Deviation = ", nm(shat, w=c(100, 75, 50),eps=c(0,0.05, 0.1)),
           "<p>&nbsp;<p>Inter quartile range = ", nm(iqr, w=c(100, 75, 50),eps=c(0,0.05, 0.1)),
           "<p>&nbsp;<p>90<sup>th</sup> Percentile = ", nm(p90, w=c(100, 75, 50),eps=c(0,0.05, 0.1)),
           "<p>&nbsp;<p>3<sup>rd</sup> Quartile = ", nm(q3, w=c(100, 75, 50),eps=c(0,0.05, 0.1)))
        htxt <- ""   
        atxt <- "Data has one digit behind the decimal, so answers should have two
                  <br>stat.table(x, ndigit=2)
                  <br>fivenumber(x, ndigit=2)
                  <br>round(quantile(x, 0.9), 2)"
   }
   
   if(problem==2) {
        df <- ""
        n <- 100*sample(10:20, 1)
        p <- runif(1, 0.2, 0.4)
        x <- rbinom(1, n, p)
        ci1 <- one.sample.prop(x, n, conf.level=90, return.result=TRUE)
        ci2 <- one.sample.prop(x, n, conf.level=95, return.result=TRUE)
        E <- round(diff(ci1)*0.3, 3)
        new.n <- prop.ps(E=E, phat=x/n, conf.level=90, return.result=TRUE)
        new.n[2] <- prop.ps(E=E, phat=x/n, return.result=TRUE)
        new.n[3] <- prop.ps(E=E, conf.level=90, return.result=TRUE)        
        new.n[4] <- prop.ps(E=E, return.result=TRUE)     
        qtxt <- paste0("In a survey of ", n, " people ", x, " said they were planning a
            vacation during the next six month.
            <p>&nbsp;<p>A 90% confidence interval for the percentage of people
            who are planning a vacation next month is (", 
              nm(c(100*ci1[1], 100*ci2[1], ci1[1], ci2[1]), w=c(100, 80, 70, 50), ndigits=1), ", ",
              nm(c(100*ci1[2], 100*ci2[2], ci1[2], ci2[2]), w=c(100, 80, 70, 50), ndigits=1), ")
            <p>&nbsp;<p>If they want to find this  interval with an error of ", E, 
            ", the sample size need is ", nm(new.n, w=c(100, 80, 70, 50)))
        atxt <- paste("one.sample.prop(", x , ", ", n, ", conf.level=90)
              <p>&nbsp;<p>prop.ps(E=", E, ", phat=", round(x/n, 4), ", conf.level=90)")
   }
   
   if(problem==3) {
       df <- ""
       n <- sample(30:50, 1)
       mu0 <- sample(100:150, 1) 
       sigma <- round(runif(1, 5, 10), 1)
       xbar <- round(mu0 - runif(1, 2, 3)*sigma/sqrt(n), 1) 
       pval <- one.sample.t(xbar, shat=sigma, n=n, muNull=mu0, alternative="less", return.result=TRUE)
       pval[2] <- one.sample.t(xbar, shat=sigma, n=n, muNull=mu0, return.result=TRUE)
       if(pval[1]<0.1) {
         qmc1 <- mc(c("reject", "fail to reject"), w=c(100, 0)) 
         qmc2 <- mc(c("did", "did not"), w=c(100, 0)) 
         amc1 <- "reject"
         amc2 <- "did"
       }  
       else {
         qmc1 <- mc(c("reject", "fail to reject"), w=c(0, 100)) 
         qmc2 <- mc(c("did", "did not"), w=c(0, 100)) 
         amc1 <- "fail to reject"
         amc2 <- "did not"
       }  
       
       qtxt <- paste0("In the past the mean number of phone calls received by a tech support  
         center was ", mu0, ". The company has made some improvements to their products and 
         hopes that this will have lowered the number of phone calls. They randomly select ",
         n, " days and find a mean number of calls of ", xbar, " with a standard deviation of ",
         sigma, ". They then test at the 10% level whether the improvements have been succesfull.
         <p>Fill in the details of the test:
         <p>&alpha; =  ", nm(0.1), 
        "<p>H<sub>0</sub>: ", mc(parlist, w=c(100, rep(0, 5))),  " ", mc(sgnlist, c(100, 0, 0)), 
                             " ", nm(mu0), 
        "<p>H<sub>a</sub>: ", mc(parlist, w=c(100, rep(0, 5))),  " ", mc(sgnlist, c(0, 100, 0)), 
                             " ", nm(mu0),                    
        "<p>p value = ", nm(pval, w=c(100, 50), pts=5),
        "<p>Decision: ",  qmc1, " the null hypothesis
         <p>Conclusion: the improvements ", qmc2, " work.")
      atxt <- paste0("
          &alpha; = 0.1 
         <p>H<sub>0</sub>: &mu; = ", mu0, 
        "<p>H<sub>a</sub>: &mu; < ", mu0, 
        "<p>p value = ", pval,
        "<p>Decision: ",  amc1, " the null hypothesis
         <p>Conclusion: the improvements ", amc2, " work.")                                 
         
   }
   
   if(problem==4) {
      mu <- round(runif(1, 20, 40))
      sig <- round(runif(1, 5, 10), 1) 
      x <- round(mu + runif(1, 0.5, 0.8)*sig, 1) 
      p1 <- round(1-pnorm(x, mu, sig), 4)
      p2 <- round(1-pnorm(x, mu, sig/sqrt(10)), 4)
      qtxt <- paste("Use <nolink>simulation</nolink> find
          <p>&nbsp;<p>a) the probability that an observation from a normal distribution 
            with mean ", mu, " and standard deviation ", sig, " is larger than ", x, 
         "<p>p = ", nm(p1, eps=3*sqrt(p1)/100),  
         "<p>&nbsp;<p>b) the probability that the mean of 10 observations from a normal distribution 
          with mean ", mu, " and standard deviation ", sig, " is larger than ", x, 
         "<p>p = ", nm(p2, eps=3*sqrt(p2)/100))
          
      atxt <- paste("a) 
          <p>B <- 10000
          <p>x <- rnorm(B, ", mu, ", ", sig, ")    
          <p>sum(x >", x, ")/B
          <p>&nbsp;<p>b)
          <p>y <- rep(0, B)
          <p>for(i in 1:B) y[i] <- mean(rnorm(10, ", mu, ", ", sig, "))    
          <p>sum(y >", x, ")/B")
          
   }
   
   if(problem==5) {
         library(mvtnorm)
         n <- sample(40:60, 1)
         x <- rmvnorm(n, sigma=matrix(c(1, 0.7, 0.7, 1),2,2))
         df <- data.frame(Height=round(170+5*x[, 1], 1) , Weight=round(80+7*x[,2]))
         r <- round(cor(df$Height, df$Weight), 3)
         fit <- slr(df$Weight, df$Height, return.result=TRUE, do.graph=FALSE)[1:2]
         est <- round(fit[1] + fit[2]*175, 1)
         qtxt <- paste("Below are the height (in cm) and weight (in kg) of ", n, 
            " randomly selected men. 
           <p>&nbsp;<p>The correlations coefficient of height and weight is ", nm(r, ndigits=3),
          "<p>&nbsp;<p>The least squares regression equation is 
           <p>Weight = ", nm(fit[1], eps=0.9), " + ",  nm(fit[2], eps=0.1), " Height
           <p>&nbsp;<p>use the model to find the weight of a man who is 175 cm tall. 
           <p>weight = ", nm(est, eps=0.9), "</h5><hr>", moodle.table(df))
           df<-""
         atxt <- paste("r = ", r, 
           "<p>Weight = ", round(fit[1], 2), " + ",  round(fit[2], 2), " Height  
            <p>", round(fit[1], 2), " + ",  round(fit[2], 2), "*175 = ", 
               round(fit[1], 2)+round(fit[2], 2)*175)     
   }
    
   if(problem==6) {
        old.p <- sample(15:20, 1)
        new.p <- old.p-sample(5:10, 1)
        n <- prop.ps(power=90, phat=new.p/100, pi.null=old.p/100, alternative="less", return.result=TRUE)
        n[2] <- prop.ps(power=90, phat=new.p/100, pi.null=old.p/100, return.result=TRUE)
        qtxt <- paste0("Over many years ", old.p, "% percent of the parts made in a factory where
            defective. They have redesigned their manifactoring processes and hope that the
            failure rate is now at most ", new.p, "%. How many parts do they have to check so 
            that the hypothesis test at the 5% level has a power of 90%?
            <p>&nbsp;<p>n = ", nm(n, w=c(100, 70), eps=rep(2, 2)))
        atxt <- paste0("prop.ps(power=90, phat=", new.p/100, ", pi.null=", old.p/100, ",
                alternative=\"less\") <p>n=", n[1])  
   } 
    
   if(problem==7) {
        df <- sort(round(rnorm(20, 100, 30)))
        n <- t.ps(E=3, sigma=sd(df), conf.level=90, return.result=TRUE)
        n[2] <- t.ps(E=6, sigma=sd(df), conf.level=90, return.result=TRUE)
        n[3] <- t.ps(E=6, sigma=sd(df), return.result=TRUE)
        n[3] <- t.ps(E=3, sigma=sd(df), return.result=TRUE)
        
        qtxt <- paste0("A researcher wants to find out how much time people spend every day on
           social media. He wants to find a 90% confidence interval for the mean. He does
           a small pilot study and the replies are below (in minutes). How many people does
           he have to include in his study if he wants the interval to have a length of 6
           minutes?
           <p>&nbsp;<p>n = ", nm(n, w=c(100, 75, 75, 50), eps=rep(2, 4)))
        atxt <- paste0("E = L/2 = 3
           <p>t.ps(E=3, sigma=sd(minutes), conf.level=90)
           <p>n = ", n[1])   
    } 
  list(qtxt = paste0("<h5>", qtxt, "</h5>", 
        ifelse(df[1]=="", "", "<hr>"),
        ifelse(df[1]=="", "", moodle.table(df))),
        htxt = paste0("<h5>", htxt, "</h5>"), 
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 

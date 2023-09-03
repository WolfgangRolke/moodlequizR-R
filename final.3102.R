final.3102 <- function(problem) {
   category <- paste0(" Final Random / Problem = ", problem)
   quizname <- "problem - " 
   htxt <- paste("  ")   
   if(problem==1) {
        ngroup <- 4
        n <- sample(10:15, size = ngroup, replace = T)         
        x <- rep(paste0("Species-", 1:ngroup), n)
        repeat {
          y <- rnorm(sum(n), 25, 3)
          y[x=="Species-3"] <- y[x=="Species-3"] + runif(n[3], 2, 5)
          y <- round(y, 1)   
          pval <- oneway(y, x, return.result=TRUE, noGraph=TRUE)
          a <- tukey(y, x, show.all = T) 
          Done <- c(pval<0.05,  pval>0.005,
             ifelse("Species-3-Species-1" %in% a[, 1],
                a[a[, 1]=="Species-3-Species-1", 2]<0.05,
                a[a[, 1]=="Species-1-Species-3", 2]<0.05),
             ifelse("Species-2-Species-1" %in% a[, 1],
                a[a[, 1]=="Species-2-Species-1", 2]>0.05,
                a[a[, 1]=="Species-1-Species-2", 2]>0.05))
          if(all(Done)) break
        } 
        df <- data.frame(Length=y, Species=x) 
        qtxt <- paste0("A biologist is studying 4 different species of rats. 
          She wants to find out whether the rats in different species have 
          different lengths (in cm). She randomly collects rats from each pecies.
          The data is below.
          <p>&nbsp;<p>The test whether the mean lengths are the same has a p value of ",
          nm(pval, ndigit=3), " and therefore we ", mc(c("reject", "fail to reject"), c(100, 0)), 
          " the null hypothesis.
          <p>&nbsp;<p>Further investigation shows that
          <p>&nbsp;<p>Species 1 and Species 2 ", mc(c("are", "are not"), c(0, 100)), 
              " statistically significantly different.
          <p>&nbsp;<p>Species 1 and Species 3 ", mc(c("are", "are not"), c(100, 0)), 
              " statistically significantly different. ")             
      atxt <- paste0("Rcommands: <p>&nbsp;<p>oneway(Length, Species)
         <p>&nbsp;<p>tukey(Length, Species)")
   }
   
   if(problem==2) {
      x <- sort(round(runif(30, 0, 30)))
      y <- round(20000 + 1000*x + rnorm(30, 0, 5000), -2)
      df <- data.frame(Years=x, Salary=y)
      fit <- slr(y, x, return.result=TRUE)
      est1 <- slr.predict(y, x, newx=10, interval="PI", conf.level=90)
      est2 <- slr.predict(y, x, newx=10, interval="PI")
      est3 <- slr.predict(y, x, newx=10, interval="CI", conf.level=90)
      est4 <- slr.predict(y, x, newx=10, interval="CI")
      est <- round(rbind(est1, est2, est3, est4)[, 3:4], -2)
      qtxt <- paste("A company has done a survey of its employees. The data for the number of
         years an employee has been with the company and his/her salary are below.
         <p>&nbsp;<p>The least squares regression equation is
         <p>Salary = ", nm(fit[1], eps=100), " + ", nm(fit[2], eps=10), " Years
          <p>&nbsp;<p>R<sup>2</sup> = ", nm(fit[3], eps=0.1), "%
          <p>&nbsp;<p>A 90% confidence interval for the salary of an employee with 10 years
          of service is (", nm(est[, 1], w=c(100, 80, 70, 50), eps=100), 
          ", ", nm(est[, 2], w=c(100, 80, 70, 50), , eps=100), ")
          <p>&nbsp;<p>This is a ", mc(c("Prediction", "Extrapolation"), c(100, 0)))
      atxt <- "slr(Salary, Years)    
          <p>&nbsp;<p>slr.predict(Salary, Years, newx=10, interval=\"PI\", conf.level=90)"
   }
   
   if(problem==3) {
      n <- sample(40:60, 1)
      x <- gen.cont.table.data(n, c("Male", "Female"), 
          c("A", "B", "C", "Fail"), rho=0.7)
      df <- data.frame(Gender=x[, 1], Grade=x[, 2])    
      pval <- round(chi.ind.test(table(x[, 1], x[, 2]), return.result=TRUE), 4)
      if(pval<0.05) qmc <- mc(3, c(100, 0))     
      else qmc <- mc(3, c(0, 100))        
      qtxt <- paste("A teacher wants to see whether there is a difference between
        the grades depending on the gender of the student. 
        <p>&nbsp;<p>She finds that the difference ", qmc, 
          ". ( p value = ", nm(pval, eps=0.01), ")")
      atxt <- "chi.ind.test(table(Gender, Grade))"    
   }
   
   if(problem==4) {
      n <- sample(40:60, 1)
      x <- gen.cont.table.data(n, 1:2, 1:2, rho=0)
      repeat {
          y <- round(5+x[,1]+x[,2]+x[,1]*x[,2]+rnorm(n))
          df <- data.frame(Score=y, Gender=ifelse(x[,1]==1, "Male", "Female"),
            Education=ifelse(x[,2]==1, "Low", "High"))
          pvals <- twoway(y, df$Gender, df$Education, return.result=TRUE)   
          if(max(pvals)<0.05) break
      }    
      a <- tukey(y, df$Gender, df$Education, which="interaction", show.all = T)
      if("Female:High-Male:High" %in% a[, 1])
          pval2 <- a[a[,1]=="Female:High-Male:High", 2]
      else    
          pval2 <- a[a[,1]=="Male:High-Female:High", 2]
      if(pval2<0.05) qmc <- mc(c("is", "is not"), c(100, 0))
      else qmc <- mc(c("is", "is not"), c(0, 100))
      qtxt <- paste("An economist wants to study the effects of gender and educational level    
         on the scores in a happiness test.  
         <p>&nbsp;<p>there ", mc(c("is", "is no"), c(100, 0)), " interaction. 
           ( p value = ", nm(pvals[3], ndigits=2), ")
         <p>&nbsp;<p>Gender ", mc(c("is", "is not"), c(100, 0)), " statistically significant. 
           ( p value = ", nm(pvals[1], ndigits=2), ")
         <p>&nbsp;<p>Education ", mc(c("is", "is not"), c(100, 0)), " statistically significant. 
           ( p value = ", nm(pvals[2], ndigits=2), ")
         <p>&nbsp;<p>The combination of Female:High ", qmc, 
            " statistically significantly better than the combination Male:High. 
             ( p value = ", nm(pval2, ndigits=2), ")")
      atxt <- "twoway(Score, Gender, Education)
           <p>&nbsp;<p>tukey(Score, Gender, Education, which=\"interaction\")"        
   }
   
   if(problem==5) {
      n <- sample(100:120, 1)
      Age <- round(runif(n, 20, 60))
      Education <- round(rnorm(n, 12, 2), 1)
      JobType <- sample(1:3, size=n, replace=TRUE)
      Engagement <- sample(1:3, size=n, replace=TRUE)
      Income <- round(30000 + 100*Age + 1000*Education + rnorm(n, 0, 3000), -2)
      df <- data.frame(Income, Age, Education, JobType, Engagement)
      fit <- mlr(Income, df[,-1],return.result=TRUE)
      mout <- mallows(Income, df[, -1], return.result=T)
      best.n <- 1
      best.Cp <- as.numeric(mout[1, "Cp"])
      for(i in 2:4) {  
        xx <- as.numeric(mout[i, "Cp"])
          if(xx<best.Cp) {
            best.n <- i
            best.Cp <- xx
          }
      } 
      w <- cbind(rep(100, 4), rep(0, 4))
      for(i in 1:4) 
        if(mout[best.n, 2+i]=="") w[i, ] <- c(0, 100)
      qtxt <- paste("An economist is studying the relationship between income, age,
         educational level, job type (1=low, 2=medium, 3=high), and level of 
         engagement (0=low, 1=medium, 3=high). 
         <p>&nbsp;<p>The least squares regression line is (include - sign if necessary, but not + sign)
         <p>Income = ", nm(fit[1], eps=100), " &nbsp;&nbsp;" ,          
                        nm(fit[2], eps=1), " Age &nbsp;&nbsp;" ,
                        nm(fit[3], eps=100), " Education&nbsp;&nbsp;" ,
                        nm(fit[4], eps=10), " JobLevel&nbsp;&nbsp; " ,
                        nm(fit[5], eps=10), " Engagement
         <p>&nbsp;<p>The best <b>linear</b> model 
         <p>", mc(c("does", "does not"), w[1, ]), " include Age               
         <p>", mc(c("does", "does not"), w[2, ]), " include Education               
         <p>", mc(c("does", "does not"), w[3, ]), " include JobType         
         <p>", mc(c("does", "does not"), w[4, ]), " include Engagement")
     atxt <- "mlr(Income, x[,-1]); mallows(Income, x[,-1])"    
   }
    
   if(problem==6) {
        x <- 1:100
        repeat {
          y <- round(x + (x-50)^2/30 +rnorm(100,0,30), 1)
          y <- y + abs(min(y)) + 50
          quad.fit <- slr(y, x, polydeg=2, return.model=TRUE)
          cube.fit <- slr(y, x, polydeg=3, return.model=TRUE)
          pval <- nested.models.test(cube.fit, quad.fit, return.result = TRUE)
          if(pval>0.05) break
        }
        df <- data.frame(Amount=x, Strength=y)
        fit <-  slr.predict(y, x, polydeg=2, newx=50, interval="PI")
        qtxt <- paste("A chemist is measuring the strength of a chemical reaction
          depending on the amount of a reagent. Find the best polynomial model and use
          it to find a 95% interval estimate if the amount is 50.
          <p>&nbsp;<p>The best polynomial model is the ",
          mc(c("Linear", "Quadratic", "Cubic", "Power 4"), c(0, 100, 0, 0)),
          "<p>&nbsp;<p>The 95% interval estimate is (", nm(fit[3], eps=0.01),
          ", ", nm(fit[4], eps=0.01), ")")
       atxt <- "Linear model has bad residual vs fits plot
               <p>quad.fit <- slr(Strength, Amount, polydeg=2, return.model=TRUE)
               <p>cube.fit <- slr(Strength, Amount, polydeg=3, return.model=TRUE)
               <p>nested.models.test(cube.fit, quad.fit)
               <p>shows that cubic model is not better than quadratic one.
               <p>slr.predict(Strength, Amount, polydeg=2, newx=50, interval=\"PI\")"
          
   } 
    
        
   list(qtxt = paste0("<h5>", qtxt, "</h5><hr>", moodle.table(df)),
        htxt = paste0("<h5>", htxt, "</h5>"), 
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 

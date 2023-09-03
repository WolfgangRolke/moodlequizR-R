quiz31.ci.test.practical <- function(story) {
   if(missing(story)) story <- sample(1:3, size=1) 
   category <- paste0("Test Interval Practical / Story = ", story)
   quizname <- "problem - " 
   x <- NULL
   
   if(story==1) {
      x <- sort(sample(100:300/10, 4))
    
      worked <- sample(c(TRUE, FALSE), 1)
      if(worked) reject <- TRUE
      else reject <- sample(c(TRUE, FALSE), 1)
      txt <- c("Yes", "No", "We can't say")
      if(worked) {
          old <- round(x[4]); pract <- round(x[3]); ci <- x[1:2]
          mca1 <- mc(c("rejected", "failed to reject"), c(100, 0))$qmc
          mca2 <- mc(txt, c(100, 0, 0))$qmc
      }
      if(!worked & reject) {
          old <- round(x[4]); pract <- round(x[2]); ci <- x[c(1, 3)]
          mca1 <- mc(c("rejected", "failed to reject"), c(100, 0))$qmc
          mca2 <- mc(txt, c(0, 0, 100))$qmc
      }
      if(!worked & !reject) {
          old <- round(x[3]); pract <- round(x[1]); ci <- x[c(2, 4)]
          mca1 <- mc(c("rejected", "failed to reject"), c(0, 100))$qmc
          mca2 <- mc(txt, c(0, 100, 0))$qmc
      }
      if(old==pract) pract=old-1
      qtxt <- paste0("A call center made some changes to shorten the time their customers have to 
         wait in line. In the past the average waiting time was ", old,"  minutes. They find that 
         now a 95% confidence interval for the waiting time is (", ci[1], ", ", ci[2],") minutes.
         <p>If they had done a hypothesis test at the 5% level to see whether the changes improved the 
         waiting time they would have ", mca1, " the null hypothesis.
         <p>They also decided before the changes that to have been a succcess they would have needed to changes 
         to lower the waiting time by at least ", old-pract, " minutes. Were the changes in fact such a success? ",
         mca2)

      htxt <- "Does the interval contain the previous average waiting time?
          <p> Does this indicate a practically useful improvement?"
   
      if(ci[2]<old)
        atxt <- "The null hypothesis would have been rejected, the interval is below the 
          old waiting time."
      else
        atxt <- "The null hypothesis would have not have been rejected, the interval includes the
          old waiting time."       
        
      if(worked)    
         atxt <- paste0(atxt, "<p>The change decreased the waiting time by more than ",
         abs(pract-old), " minutes")
      if(!worked & reject)
         atxt <- paste0(atxt, "<p>We can not be sure that the change decreased the 
         waiting time by more 
         than ", abs(pract-old), " minutes because ", pract, " is in the interval")
      if(!worked & !reject)
         atxt <- paste0(atxt, "<p>The change decreased the waiting time by less than ", 
         abs(pract-old), " minutes")
   }

   if(story==2) {
      x <- sort(sample(50:200*10, 4))
    
      worked <- sample(c(TRUE, FALSE), 1)
      if(worked) reject <- TRUE
      else reject <- sample(c(TRUE, FALSE), 1)
      txt <- c("Yes", "No", "We can't say")
      if(worked) {
          old <- round(x[1]); pract <- round(x[2]); ci <- x[3:4]
          mca1 <- mc(c("rejected", "failed to reject"), c(100, 0))$qmc
          mca2 <- mc(txt, c(100, 0, 0))$qmc
      }
      if(!worked & reject) {
          old <- round(x[1]); pract <- round(x[3]); ci <- x[c(2, 4)]
          mca1 <- mc(c("rejected", "failed to reject"), c(100, 0))$qmc
          mca2 <- mc(txt, c(0, 0, 100))$qmc
      }
      if(!worked & !reject) {
          old <- round(x[2]); pract <- round(x[3]); ci <- x[c(2, 4)]
          mca1 <- mc(c("rejected", "failed to reject"), c(0, 100))$qmc
          mca2 <- mc(txt, c(0, 100, 0))$qmc
      }
      if(old==pract) pract=old+2
      qtxt <- paste0("A law was passed some time ago to increase the number of fish in a lake. In
         the past the number was ", old,"  fish per cubic kilometer. In a survey now they find that 
         a 95% confidence interval for the number of fish do be (", ci[1], ", ", ci[2],").
         
         <p>If they had done a hypothesis test at the 5% level to see whether the number of fish has 
         gone up they would have ", mca1, " the null hypothesis.
         
         <p>It was also decided at the beginning that to be a success the number of fish had to 
         increase to at least ", pract, " . Was the new law in fact such a success? ",
         mca2)

      htxt <- "Does the interval contain the previous average number of fish?
          <p> Does this indicate a practically useful increase?"
   
      if(ci[2]<old)
        atxt <- "The null hypothesis would have been rejected, the interval is above the
          old number of fish."
      else
        atxt <- "The null hypothesis would have not have been rejected, the interval includes the
          old number of fish."       

      if(worked) 
         atxt <- paste0(atxt, "<p>The law increased the number of fish by more than ", abs(pract-old), ".")
      if(!worked & reject)
         atxt <- paste0(atxt, "<p>We can not be sure that the law increased the number of fish by more 
         than ", abs(pract-old), " because ", pract, " is in the interval")
      if(!worked & !reject)
         atxt <- paste0(atxt, "<p>The law increased the number of fish by less than ", abs(pract-old), ".")
   }
   
   if(story==3) {
      x <- sort(sample(10:50*10, 4))
    
      worked <- sample(c(TRUE, FALSE), 1)
      if(worked) reject <- TRUE
      else reject <- sample(c(TRUE, FALSE), 1)
      txt <- c("Yes", "No", "We can't say")
      if(worked) {
          old <- round(x[1]); pract <- round(x[2]); ci <- x[3:4]
          mca1 <- mc(c("rejected", "failed to reject"), c(100, 0))$qmc
          mca2 <- mc(txt, c(100, 0, 0))$qmc
      }
      if(!worked & reject) {
          old <- round(x[1]); pract <- round(x[3]); ci <- x[c(2, 4)]
          mca1 <- mc(c("rejected", "failed to reject"), c(100, 0))$qmc
          mca2 <- mc(txt, c(0, 0, 100))$qmc
      }
      if(!worked & !reject) {
          old <- round(x[2]); pract <- round(x[3]); ci <- x[c(2, 4)]
          mca1 <- mc(c("rejected", "failed to reject"), c(0, 100))$qmc
          mca2 <- mc(txt, c(0, 100, 0))$qmc
      }
      if(old==pract) pract=old+2
      qtxt <- paste0("A company has delevopped a new fertilizer. It is supposed to increase the weight 
         of a fruit. In the past the average weight was ", old,"  gram. In a small study they find that 
         a 95% confidence interval for the weight is (", ci[1], ", ", ci[2],") gram.
         
         <p>If they had done a hypothesis test at the 5% level to see whether the weight has 
         gone up they would have ", mca1, " the null hypothesis.
         
         <p>It was also decided at the beginning that to be a success the weight had to 
         increase to at least ", pract, " . Is the new fertilizer in fact such a success? ",
         mca2)

      htxt <- "Does the interval contain the previous average weight of the fruit?
          <p> Does this indicate a practically useful increase?"
   
      if(ci[2]<old)
        atxt <- "The null hypothesis would have been rejected, the interval is above the
          old weight."
      else
        atxt <- "The null hypothesis would have not have been rejected, the interval includes the
          old number of fish."       

      if(worked) 
         atxt <- paste0(atxt, "<p>The fertilize increased the weight by more than ", abs(pract-old), ".")
      if(!worked & reject)
         atxt <- paste0(atxt, "<p>We can not be sure that the fertilize increased the weight by more 
         than ", abs(pract-old), " because ", pract, " is in the interval")
      if(!worked & !reject)
         atxt <- paste0(atxt, "<p>The fertilize increased the weight by less than ", abs(pract-old), ".")
   }
      
   dta.tbl <- ""     
   if(!is.null(x)) 
      dta.tbl <-  paste("<hr>", moodle.table(x), collapse="")    
        
   list(qtxt = paste0("<h5>", qtxt, "</h5>"),
        dta.tbl,
        htxt = paste0("<h5>", htxt, "</h5>"), 
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 

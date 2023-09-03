quiz1.basic.R.1 <- function(which) {
   if(missing(which)) which=sample(1:2,1)
   problemname=c("sum 1", "sum 2", "upr 1", "upr 2")[which]
   category <- paste0("Introduction to R / Basic R Part 1 / ", problemname)
   quizname <- "problem - " 
   x <- NULL  
   htxt <- ""   
   if(which==1) {
      n=500+50*sample(1:10, 1)
      s=sum(1:n)
      qtxt <- paste0("Use R to do answer the following questions.<p>
           The sum of the numbers from 1 to ", n, " (1+2+3+..+",n,")=",nm(s))
      htxt <- "use sum command"
      atxt <- paste0("1+2+3+..+",n," = ",s,
      "<p>R command: sum(1:", n,")")
   }

   if(which==2) {
      n=50+5*sample(1:10, 1)
      s=sum((1:n)^2)
      qtxt <- paste0("The sum of the squares of the numbers from 1 to ", n, " 
                     $$(1^2+2^2+3^2+..+",n,"^2)$$=",nm(s))
      atxt <- paste0("$$(1^2+2^2+3^2+..+",n,"^2)$$ = ",s,
      "<p>R command: sum((1:", n,")^2)")
   }   
   if(which==3) {
      qtxt=paste0("The data set upr (part of Resma3.Rdata) has the application information for UPRM students from 2003-2013.
      <p>The mean of the Highschool GPAs is ", nm(3.66, eps=0.1))
      atxt=paste0("The mean of the Highschool GPAs is 3.66
      <p>attach(upr)
      <p>mean(Highschool.GPA)")
   
   }
   if(which==4) {
      qtxt=paste0("The percentage of the students that was female is ", 
             nm(0.4853799*c(100, 1), w=c(100, 75),eps=c(0.1, 0.01)),"%")
      atxt=paste0("The percentage of the students that was female is 48.5%
      <p>round(sum(Gender == \"F\")/length(Gender) * 100, 1)
      <p>or use the isubset command to get female students only.")
   
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

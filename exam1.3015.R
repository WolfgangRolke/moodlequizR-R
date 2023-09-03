exam1.3015 <- function(problem ) {
   if(missing(problem)) problem <- sample(c(1:7), 1)
   category <- paste("Exams / Exam 1 / Problem ", problem) 
   quizname <- "problem - " 
    
   if(problem == 1) {   
      n <- sample(101:149, 1)
      x <- round(rnorm(n, runif(1, 50, 70), runif(1, 5, 15)), 1)
      mu <- round(mean(x), 2)
      sig <- round(sd(x), 2)
      med <- round(median(x), 2)
      qtxt <- paste0("For the data below find the following:
          <p>The sample size is ", nm(n), 
          "<p>The mean is ", nm(mu, c(100, 80, 50), eps = c(0, 0.1, 1)),
          "<p>The median is ", nm(med, c(100, 80, 50), eps = c(0, 0.1, 1)),
          "<p>The standard deviation is ", nm(sig, c(100, 80, 50), eps = c(0, 0.1, 1)),
          "<p><hr>", moodle.table(x))
          
       atxt <- paste0("The sample size is ", n, 
          "<p>The mean is ", mu ,
          "<p>The median is ", med ,
          "<p>The standard deviation is ", sig ,
          "<p>Data has one digit behind decimal, so answers should be rounded to two digits.
           <p>see quizzes 13 and 15")
          
  }
  if(problem == 2) {
      opt=c("Categorical", "Most likely categorical", "Quantitative", "Most likely quantitative",  "Unclear")
      qtxt <- paste0("For each of the following variables choose the most likely data type.
            <p><li>Age of people participating in a weight loss program", mc(opt, c(0, 0, 100, 50, 0)), "</li>
            <p><li>Time it takes to finish an exam ", mc(opt, c(0, 0, 100, 0, 0)), "</li>
            <p><li>In an experiment a researcher uses a treatment labeled 1, 2 or 3. The treatment is a ", mc(opt, c(100, 0, 0 ,0, 0)), " variable</li>            
            <p><li>In an experiment a researcher uses 1, 2 or 3 liters of water. The amount of water is a", mc(opt, c(0, 0, 100 ,0, 0)), " variable</li>
            <p><li>A company uses a system of id numbers for their employees. The id numbers are a", mc(opt, c(100, 0, 0, 0, 0)), " variable</li>
            <p><li>In a survey participants are asked how  happy they are. ", mc(opt, c(50, 50, 0, 50, 100)), "</li>"
      )

      atxt <- paste0("
           <li>", opt[3], "</li>
           <p><li>", opt[3], "</li>
           <p><li>", opt[3], "</li>
           <p><li>", opt[1], "</li>
           <p><li>", opt[3], "</li>
           <p><li>", opt[1], "</li>
           <p><li>", opt[5], "</li>
           <p>see quiz 7"
      )
  }
  if(problem == 3) {
      source("C:\\Users\\Wolfgang\\Dropbox\\teaching\\moodlequizR\\contingency.table.R")
      out <- contingency.table(c(3, 3), with.marginals=FALSE)
      qtxt <- out$qtxt
      atxt <- paste0(out$atxt,"<p>see quiz 11")
      remove(list="contingency.table", pos=.GlobalEnv)
  }
  if(problem == 4) {
       n=sample(50:70, 1)
       ID=sample(10000:20000, n)
       Gender=sample(c("Male", "Female"), n, replace=TRUE)
       Type=sample(c("Part-Time", "Full-Time"), n, replace=TRUE, prob=c(0.3, 0.7))
       Income=round(runif(n, 20000, 40000)+ifelse(Type=="Full-Time", 20000, 0), -2) 
       mu=round(mean(Income),-1)
       muf=round(mean(Income[Gender=="Female"]),-1)
       nf=sum(Gender=="Female")
       pfp=round(sum(Gender=="Female" & Type=="Part-Time")/n*100, 1)
       x=data.frame(ID, Gender, Type, Income)
       qtxt=paste0("Below is some information for the employees in a company.
          <p>The company has ", nm(n), " employees. 
          <p>The mean income is ", nm(mu, c(100, 80, 50), eps=c(0, 10, 1000)), ". 
          <p>There are ", nm(nf), " female employees.
          <p>The mean income of the female employees is  ", nm(muf, c(100, 80, 50), eps=c(0, 10, 1000)), ".
          <p>The percentage of female part time employees is ",  nm(pfp, c(100, 80), eps=c(0, 1)), ".
          <p><hr>", moodle.table(x)
       )   
       atxt=paste0("The company has ", n, " employees.
          <p>The mean income is ", mu, ".
          <p>There are ", nf, " female employees.
          <p>The mean income of the female employees is  ", muf, ".
          <p>The percentage of female part time employees is ",  pfp, ".
          <p>see quiz 3"
       )
   }   
   if(problem==5) {
       n=sample(50:100, 1)
       m=sample(c(17, 19, 23, 27, 31, 37, 43), 1)
       qtxt=paste0("Use R to find the following:
          <p>
          <p><li>$$\\sum_{i=1}^{",n,"}$$ 1/i = 1+1/2+1/3+...+1/",n,"=",nm(sum(1/c(1:n)), eps=0.1), ".</li>
          <p><li>$$\\sqrt{",m,"}=$$",nm(sqrt(m), eps=0.1), ".</li>
          <p><li>$$\\sqrt{1+2+...+",n,"}=$$",nm(sqrt(sum(1:n)), eps=0.1), ".</li>
          <p><li>$$\\sqrt{1}+\\sqrt{2}+...+\\sqrt{",n,"}=$$",nm(sum(sqrt(1:n)), eps=0.1), ".</li>
       ")    
       atxt=paste0("
          <li>sum(1/c(1:",n,") : ",sum(1/c(1:n)), "</li>.
          <p><li><p>sqrt(",m,") : ",sqrt(m) , "</li>.
          <p><li>sqrt(sum(1:",n,")) :", sqrt(sum(1:n)), ".</li>
          <p><li>sum(sqrt(1:",n,")) :", sum(sqrt(1:n)), ".</li>          
          <p>see quizzes on R
       ")
   }
   if(problem==6) {
       source("meanvsmedian.R")
       tmp=meanvsmedian(whichstory=3)
       qtxt=tmp$qtxt
       atxt=paste0(tmp$atxt,"<p>see quiz 14")
   }    
  if(problem == 7) {
      source("C:\\Users\\Wolfgang\\Dropbox\\teaching\\moodlequizR\\contingency.table.fill.R")
      out <- contingency.table.fill(num_row_col=c(3,3))
      qtxt <- out$qtxt
      atxt <- paste0(out$atxt,"<p>see quiz 9")
      remove(list="contingency.table.fill", pos=.GlobalEnv)
  }  
  qtxt <- paste0("<h5>", qtxt,   "</h5>")
  atxt <-paste("<h5>", atxt, "</h5>")   
  htxt <- ""
  list(qtxt = qtxt, 
     atxt = atxt, 
     htxt = htxt, 
     category = category, quizname = quizname) 
} 

exam2.3015 <- function(problem) {
   category <- paste("Exams / Exam 2 / Problem ", problem) 
   quizname <- "problem - " 
   if(problem==1) {
       x=round(rnorm(50, 20, 5), 2)
       qtxt=paste0("For the data set below find
         <p>Minimum=", nm(min(x)), "
         <p>First Quartile=", nm(round(quantile(x, 0.25), 3), w=c(100,75), eps=c(0, 0.1)), "
         <p>Mean=", nm(round(mean(x), 3), w=c(100,75), eps=c(0, 0.1)), "
         <p>Median=", nm(round(median(x), 3), w=c(100,75), eps=c(0, 0.1)), "
         <p>Third Quartile=", nm(round(quantile(x, 0.75), 3), w=c(100,75), eps=c(0, 0.1)), "   
         <p>Maximum=", nm(max(x)), "      
         <p>Standard deviation=", nm(round(sd(x), 3), w=c(100,75), eps=c(0, 0.1)), "
         <p>Interquartile Range=", nm(round(IQR(x), 3), w=c(100,75), eps=c(0, 0.1)), "
         <p>67th Percentile=", nm(round(quantile(x, 0.67), 3), w=c(100,75), eps=c(0, 0.1))
       )
       qtxt = paste(qtxt, moodle.table(x), collapse="")
       atxt=paste0("For the data set below find
         <p>Minimum=", min(x), "
         <p>First Quartile=", round(quantile(x, 0.25), 3), "
         <p>Mean=", round(mean(x), 3), "
         <p>Median=", round(median(x), 3), "
         <p>Third Quartile=", round(quantile(x, 0.75), 3),"      
         <p>Maximum=", max(x), "         
         <p>Standard deviation=", round(sd(x), 3), "
         <p>Interquartile Range=", round(IQR(x), 3),"
         <p>67th Percentile=", round(quantile(x, 0.67), 3)
       )  
   }
   if(problem==2){
        x=round(runif(30, 50, 100))
        y=round(runif(1, 60, 80))
        z=round((y-mean(x))/sd(x), 2)
        qtxt=paste0("Below are the scores of students in an exam. Your score was ", y, ".
        <p>your z score in this exam is ", nm(z, c(100,75), c(0.01, 1)), ", and so you did ",
        mc(c("better","worse"), c(ifelse(z<0, 0, 100), ifelse(z>0, 0, 100)))$qmc, " than average compared to the other students.")
        qtxt = paste(qtxt, moodle.table(x), collapse="")
        atxt=paste("z score = ", z, ifelse(z<0, "<", ">"), " 0, so you did ", 
        mc(c("better","worse"), c(ifelse(z<0, 0, 100), ifelse(z>0, 0, 100)))$amc)
           
   } 
   if(problem==3){
        n=sample(1000:2000, 1)  
        x=rnorm(n, 100, 30)
        mn=mean(x)
        s=sd(x)
        Int=round(mn+c(-2,2)*s, 2) 
        qtxt=paste0("A sample was collected from a normal distribution. It turned out that the interval
        from the empirical rule was (", Int[1],", ", Int[2], "). ", n, " observations were inside the interval.
        <p>The total number of observations in the sample was about ", nm(round(n/0.95), eps=1), "
        <p>The sample mean was ", nm( (Int[2]+Int[1])/2, eps=0.1), "
        <p>The sample standard deviation ", nm( (Int[2]-Int[1])/4, eps=0.1))
        atxt=paste(n, "/0.95 = ", n/0.95, "
        <p>Mean = (", Int[1],"+", Int[2], ")/", 2, " = ", (Int[1]+Int[2])/2, "
        <p>sd = (", Int[2],"-", Int[1], ")/", 4, " = ", (Int[2]-Int[1])/4)
           
   }   
   if(problem==4){
        n=40
        x=sort(round(runif(n, 10, 50), 1))
        y=round(20+3*x+rnorm(n, 0, 25))
        df=data.frame(Time=x,Amount=y)
        r=round(cor(x,y), 3)
        qtxt=paste0("A chemist carries out a study to see the effect of the length of time an experiment is run
        on how much of some chemical is produced. The data is below.
        <p>The correlation between time and amount is ", nm(r, w=c(100, 75), eps=c(0, 0.1)))
        qtxt = paste(qtxt, moodle.table(df), collapse="")
        atxt=paste("round(cor(Time, Amout), 3) = ", r)
           
   }   
   if(problem %in% c(5,6,7)) {
      source("C:\\Users\\Wolfgang\\Dropbox\\teaching\\moodler\\normal.check.R")
      if(problem==5) out <- normal.check(1)
      if(problem==6) out <- normal.check(2)
      if(problem==7) out <- normal.check(3)
      qtxt <- out$qtxt
      atxt <- out$atxt
      remove(list="normal.check", pos=.GlobalEnv)
   }     
   if(problem %in% c(8,9,10)) {
      source("C:\\Users\\Wolfgang\\Dropbox\\teaching\\moodler\\outliers.R")
      if(problem==8) out <- outliers(1)
      if(problem==9) out <- outliers(2)
      if(problem==10) out <- outliers(3)
      qtxt <- out$qtxt
      atxt <- out$atxt
      remove(list="outliers", pos=.GlobalEnv)
   }   
   list(qtxt=qtxt, htxt = "", atxt = atxt, 
     category = category, quizname = quizname) 
} 

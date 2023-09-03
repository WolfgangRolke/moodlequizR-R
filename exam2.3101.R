exam2.3101 <- function(problem, which) {
   if(missing(problem)) problem <- sample(1:3, 1) 
   category <- paste("Exams / Exam 2 / Problem ", problem) 
   quizname <- "problem - " 
   library(base64) 
   if(problem == 1) {   
      x <- rnorm(100, 100, 10)
      x <- c(x, quantile(x, 0.25)-3*IQR(x))
      plt <- png64(bplot(x, orientation = "Horizontal", 
        label_x="", return.graph=TRUE))      
      m <- round(min(x), 1)
      q1 <- round(quantile(x, 0.25), 1)
      med <- round(median(x), 1)
      q3 <- round(quantile(x, 0.75), 1)      
      M <- round(max(x), 1)
      iqr <- round(IQR(x), 1)
      opt <- c(sort(c(m, q1, med, q3, M, iqr)), "Can't tell")
      qtxt <- paste("Read the following info from the boxplot below (if possible)   
          <p>Minimum = ", mc(opt, c(0, 100, 0, 0, 0, 0)),  
         "<p>Median = ", mc(opt, c(0, 0, 0, 100, 0, 0)),
         "<p>Mean = ", mc(opt, c(0, 0, 0, 0, 0, 0, 100)),
         "<p>IQR = ", mc(opt, c(100, 0, 0, 0, 0, 0)),
         "<p>Standard deviation = ", mc(opt, c(0, 0, 0, 0, 0, 0, 100)),
         "<p>", plt)
       atxt <- paste("Minimum = ", m,  
         "<p>Median = ", med,
         "<p>Mean =  can't tell
          <p>IQR = ", iqr, 
         "<p>Standard deviation = can't tell")
   } 
   if(problem==2) {
      n <- sample(40:50, 1)
      x <- 1:n
      y <- round(10+x+rnorm(n, 0, 10), 1)
      cr1 <- cor(x, y)
      cr <- round(cor(x, y), 3)
      qtxt <- paste("the data set below has a sample correlation of ",
        nm(c(cr, cr1), c(100, 75), eps=c(0.00001, 0.001)), moodle.table(data.frame(x=x, y=y)))
      atxt <- paste("correlation = ", cr, 
        "<p>R command: cor(x, y), rounded to three digits")    
   }
   if(problem==3) {
      n <- 71
      x <- round(runif(n, 0, 99))
      mn <- c(round(mean(x), 1), mean(x))
      q3 <- c(round(quantile(x, 0.75), 1), quantile(x, 0.75))      
      iqr <- c(round(IQR(x), 1), IQR(x))
      qtxt <- paste("For the data below we have 
        <p>Mean = ", nm(mn, c(100, 75), eps=c(0.001, 0.1)),
        "<p>Q<sub>3</sub> = ", nm(q3, c(100, 75), eps=c(0.001, 0.1)), 
        "<p>IQR = ", nm(iqr, c(100, 75), eps=c(0.001, 0.1)),
        moodle.table(x))
      atxt <- paste("Mean = ", mn, 
        "<p>Q<sub>3</sub> = ", q3,
        "<p>IQR = ", iqr,   
        "<p>round to 1 digit behind decimal") 
   }
   if(problem==4) {
     quizname <- paste("problem - ", which)
     set.seed(1)
     x <- 1:50
     y<- 10+x+rnorm(50, 0, 5)
     if(which==1) {
        ot<-0 
        mmc <- mc(0:5, c(100, 0, 0, 0, 0, 0))
     }   
     if(which==2) {
       x <- c(x, 80, 20) 
       y<- c(y, 90, 75)
       ot <- 2
       mmc <- mc(0:5, c(0, 0, 100,  0, 0, 0))
     } 
     if(which==3) {
       x <- c(x, 80) 
       y<- c(y, 90)
       ot <- 1
       mmc <- mc(0:5, c(0, 100, 0, 0, 0, 0))
     }     
     if(which==4) {
       x <- c(x, 20, 21, 22, 21) 
       y<- c(y, 75, 73, 77, 70)
       ot <- 4
       mmc <- mc(0:5, c(0, 0, 0, 100,  0, 0))
     }  
     plt <- png64(mplot(y, x, return.graph=T))
     qtxt <- paste("There are ", 
     mmc, 
     " outliers in this data set<p>", plt)
     atxt <- paste("There are ", ot, " outliers in this data set")
   }
   if(problem==5) {
      n <- sample(70:90, 1)
      x <- round(rnorm(n, 50, 20))
      x1 <- sample(c(10:30, 70:90), 1)
      mu <- round(mean(c(x, x1)), 1)
      y1 <- x1
      repeat {
        y1 <- y1-1
        if(round(mean(c(x, y1)), 1)!=mu) break
      } 
      y1 <- y1+1 
      y2 <- x1
      repeat {
        y2 <- y2+1
        if(round(mean(c(x, y2)), 1)!=mu) break
      }         
      y2 <- y2-1  
      if(mu==round(mu)) mu <- paste0(mu, ".0")
      qtxt <- paste0("Consider the data set below. Find a NEW number x 
        for the data set so that the mean is ", mu, ".<p>&nbsp;<p> x = ", 
        nm(y1:y2), 
        "<p>&nbsp;<p>Note: your choice of x has to be a whole
        number and lead to the required mean when using our rounding rules!",
        moodle.table(x))
     atxt  <- paste(" any number between ", y1, " and ", y2, " works")
   }
   if(problem==6) {
      mu <- sample(c(-1, 1), 1)*runif(1, 1, 2)
      if(mu<0) mcmu <- mc(c("<", "=", ">"), c(100, 0, 0))
      else mcmu <- mc(c("<", "=", ">"), c(0, 0, 100))
      sig <- ifelse(runif(1)<0.5, runif(1, 0.1, 0.7), runif(1, 1.3, 2))
      if(sig<1) mcsig <- mc(c("<", "=", ">"), c(100, 0, 0))
      else mcsig <- mc(c("<", "=", ">"), c(0, 0, 100))
      x <- seq(-4, 4, length=250)
      y <- c(dnorm(x), dnorm(x, mu, sig))
      df <- data.frame(x=c(x,x),y=y,z=rep(c("a","b"),each=250))  
      plt <- png64(ggplot(df, aes(x,y,color=z))+geom_line()+
          theme(legend.position="none"))
      qtxt <- paste("In the graph below the red normal curve has a &mu;=0 
        and &sigma;=1. So the blue normal curve has &mu;", mcmu, "0 and
        &sigma;", mcsig, "1.<p>", plt)     
      atxt <- paste("&mu;", ifelse(mu<0, "<", ">"), "0 and     
          &sigma;", ifelse(sig<1, "<", ">"), "1.")
   }
   qtxt <- paste0("<h5>", qtxt,   "</h5>")
   atxt <-paste("<h5>", atxt, "</h5>")   
   list(qtxt = qtxt, 
     atxt = atxt, 
     category = category, quizname = quizname) 
} 

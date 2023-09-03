quiz38.correlation <- function(which, whichstory, do.graph=FALSE) {
    require(mvtnorm)
    require(base64)
    if(missing(whichstory))
       whichstory <- sample( 1:4, 1)
    category <- paste0("Correlation /", which, " / Story = ", whichstory)
    quizname <- "problem - " 
    g <- function() {
      plt64 <- ""
      if(do.graph) plt64 <- png64(pearson.cor( x, y, return.graph = TRUE))
      if( which == "Estimation" ) {
          est <- cor(x, y) 
          qtxt <- paste0( "<p>Pearson's correlation coefficient is : 
             {:NM:%100%", round(est, 3), "~%75%", est, ":0.01}") 
          atxt <- paste0("<p>Pearson's correlation coefficient is : ", round(est, 3),
             "<br>(round to 3 digits)
             <br> R command: ", paste("cor(", colnames(data)[2], ", ", colnames(data)[1], ")")) 
      }    
      if( which == "Confidence Interval" ) {
          ci <- pearson.cor( x, y, conf.level = alpha, return.result = TRUE)[3:4]         
          cialt <- pearson.cor( x, y, return.result = TRUE)[3:4]                   
          qtxt <- paste0("<p>A ", alpha, "% confidence interval for the correlation is
             given by <p>( {:NM:%100%", ci[1], ":0.001~%75%", cialt[1], ":0.001} , 
                             {:NM:%100%", ci[2], ":0.001~%75%", cialt[2], ":0.001})")
          atxt <- paste0("A ", alpha, "% confidence interval for the correlation is
             given by <p>(", ci[1], " , ", ci[2], ")
             <p>R command: ", paste0("pearson.cor(", colnames(data)[2], ", ", 
                                             colnames(data)[1], ", ", al, ")"))    
      }      
      if( which == "Hypothesis Test" ) {           
          pval <- pearson.cor( x, y, rho.null = 0, return.result = TRUE)[2]
          if(pval < alpha) 
            qdecision <- "<p>Decision: {1:MC:~=Reject~Fail to reject} null hypothesis"
          else    
            qdecision <- "<p>Decision: {1:MC:~Reject~=Fail to reject} null hypothesis"  
          command <- paste0("pearson.cor(", colnames(data)[2], ", ", 
                                     colnames(data)[1], ", rho.null=0)")
          qtxt <- paste0("
              <hr>Parameter(s): {1:MC:~mean~median~percentage~proportion~probability
                      ~standard deviation~=correlation~association~other}
              <p>Assumptions: {1:MC:=ok~not ok~none~can't tell~don't know}
              <p>H<sub>0</sub>: &nbsp; &rho; = 0,
              <p>H<sub>a</sub>: &nbsp; &rho; &ne; 0
              <p><nolink>p-value</nolink> = {:NM:%100%", pval, ":0.01}
              <p>", qdecision)
          atxt <- paste0("<p><nolink>p-value</nolink>=", pval, "<p>R command: ", command)                    
      }      
      list(qtxt = qtxt, atxt = atxt, plt64 = plt64)  
    } 
    alpha <- sample( c(0.01, 0.05, 0.1), 1)
    if( which == "Confidence Interval") {
        alpha <- 100*(1-alpha)
        al <- ifelse( alpha == 95, "", paste0(", conf.level = ",alpha) )
    }    
    n <- sample( 20:50, 1)
    if(whichstory == 1)  {
        x <- 1:n
        y <- round(sample(8:12,1) + runif(1, 0.8, 1.2)*sample(c(-1,1), 1)*x/10 + rnorm(n,0,1))
    }
    if(whichstory == 2)  {
        rho <- runif(1, -0.8, 0.8)
        z <- 20000+1000*rmvnorm(n, sigma=matrix( c(1, rho, rho, 1),2,2))
        x <- round(z[,1], 2)
        y <- round(z[,2], 2)
    }
    if(whichstory == 3)  {
        n <- sample(40:50, 1)
        x <- 20:n
        y <- round(1000+5*x+rnorm(length(x),0,100))
    }
    if(whichstory == 4)  {
        n <- sample(20:50, 1)
        x <- 2000+c(1:n)*100
        y <- round( x/1000*runif(1, 0,0.5)+rnorm(n,0,1), 1)
    }
    if(whichstory == 1) {
         data <- data.frame(Day=x, Faulty=y)
         if( which == "Estimation" ) txt <- "<p>Find the correlation of the days and the
                                        number of faulty widgets."
         if( which == "Hypothesis Test" ) txt <- paste0("<p>Test at the ", 100*alpha, "% level
            whether there is a relationship between the days and the number of faults.")                                        
         if( which == "Confidence Interval" ) txt <- paste0("<p>Find a ", alpha, "% confidence 
            interval for the correlation between the days and the number of faults.")                                        
         qtxt <- paste0("A company has a machine that makes widgets. For ", n, " consecutive days
            they count the number of faulty widgets.", txt)
    }  
    if(whichstory == 2) { 
         data <- data.frame(Day_1=x, Day_2=y)
         if( which == "Estimation" ) txt <- "<p>Find the correlation of the value of the
            Dow Jones on consecutive days."
         if( which == "Hypothesis Test" ) txt <- paste0("<p>Test at the ", 100*alpha, "% level
            whether there is a relationship between the values of the Dow Jones on 
            consecutive days.")                                        
         if( which == "Confidence Interval" ) txt <- paste0("<p>Find a ", alpha, "% confidence 
            interval for the correlation between the values of the Dow Jones on 
            consecutive days.")                                        
         qtxt <- paste0("An economist wants to study the relationship of the 
            values of the Dow Jones industrial index on consecutive days. He
            randomly selects ", n, " days and records the value on this and the 
            following day.", txt)
   }
   if(whichstory == 3) { 
         data <- data.frame(Sugar=x, Microbes=y)
         if( which == "Estimation" ) txt <- "<p>Find the correlation of the number of microbes
            and the amount of sugar."
         if( which == "Hypothesis Test" ) txt <- paste0("<p>Test at the ", 100*alpha, "% level
            whether there is a relationship between the amount of sugar and the number 
            of microbes.")                                        
         if( which == "Confidence Interval" ) txt <- paste0("<p>Find a ", alpha, "% confidence 
            interval for the correlation between the amount of sugar and the 
            number of microbes.")                                        
         qtxt <- paste0("A biologist wants to study the relationship between
            the number of microbes that grow in a petry dish and the amount of sugar 
            (in grams)", txt)
   }
   if(whichstory == 4) { 
         data <- data.frame(RPM=x, Oil=y)
         if( which == "Estimation" ) txt <- "<p>Find the correlation of the RPM
            and the amount of oil used."
         if( which == "Hypothesis Test" ) txt <- paste0("<p>Test at the ", 100*alpha, "% level
            whether there is a relationship between the RPM and the amount 
            of oil used.")                                        
         if( which == "Confidence Interval" ) txt <- paste0("<p>Find a ", alpha, "% confidence 
            interval for the correlation between the RPM and the amount of 
            oil used.")                                        
         qtxt <- paste0("An industrial engineer wants to study the relationship between
            the RPM (revolutions per minute) of an engine and the amount of 
            oil used (in milliliters)", txt)
   }
   
   g1 <- g()  
   list(qtxt = paste0("<h5>", qtxt, g1$qtxt,"</h5><hr>", moodle.table(data)), 
        htxt = "",
        atxt = paste0("<h5>", g1$atxt,"</h5>", g1$plt64), 
        category = category, quizname = quizname)     
}   

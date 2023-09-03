twosample <- function( which = "CI" ) {
   category <- "ANOVA/two-sample"
   quizname <- "two-sample - "
   g <- function() {
    out <- oneway(y, x, conf.level = alpha, return.result = TRUE)
    int <- out [ 2:3 ]
    int1 <- oneway(y, x, return.result = TRUE)[ 2:3 ]      
    pval <- out[1]
    plt64 <- png64( oneway(y, x, conf.level = alpha, return.graph = TRUE) )
    if( which == "CI" ) command <- paste0("oneway(y, x", al, ")")
    else command <- "oneway(y, x)"
    if( 3*out[4] < out[4] ) {
       assmt <- "not ok"
       asstxt <- paste0( "<br>Unequal variances: smaller std =", out[4], ",  larger std=", out[5])
    }    
    else {
       assmt <- "ok"
       asstxt <- paste0( "<br>Normal plot ok, equal variances: smaller std =", out[4], ",  larger std=", out[5])
    }
    txts <- qatxts( par = "mean", assumption = assmt, pval = pval,
        alpha = ifelse( which == "CI", alpha , alpha/100 ), int = int, int1 = int1)
    if( which == "CI" ) {             
       qtxt <- paste0( txts$q[ c(1, 2, 7)], collapse ="" )
       atxt <- paste0( c(txts$a[ 1:2 ], asstxt, txts$a[ 7 ]), collapse = "" )
    }  
    else {
       Htxt <- "<p>H<sub>0</sub>: &nbsp; &mu;<sub>1</sub> = &mu;<sub>2</sub>
               <p>H<sub>a</sub>: &nbsp; &mu;<sub>1</sub> &nbsp; &ne; &nbsp; &mu;<sub>2</sub>"          
       qtxt <- paste0( c(txts$q[ 1:3 ], Htxt, txts$q [ 5:6 ]), collapse = "" )
       atxt <- paste0( c(txts$a[ 1:2 ], asstxt, txts$a[ 3 ],
              Htxt, txts$a[ 5:6 ]), collapse = "" )
    }   
    atxt <- paste0( atxt, "<p>R command: ", command, "<p>", plt64, collapse = "")       
    list( qtxt = qtxt, atxt = atxt )
   }  
   
   alpha <- sample( c(90, 95, 99), 1)
   al <- ifelse(alpha == 95, "", paste0(", conf.level = ", alpha) )
   
   whichstory <- sample( 1:6, 1) 
   ngroup <- 2
   if(whichstory == 1) {
        n <- sample( 10:20, size = ngroup, replace = T)
        z <- c("", paste0("Machine-", 1:ngroup))
        k <- sample( z , 1, prob = c(ngroup, rep(1, ngroup))) 
        x <- rep( z[-1] , n)
        y <- round(10 + ifelse(x==k, 1, 0) + rnorm(sum(n), 0, 0.5), 1)
   }   
   if(whichstory == 2) {
        n <- sample( 10:20, size = ngroup, replace = T)
        z <- c("", LETTERS[1:ngroup])         
        k <- sample( z , 1, prob = c(ngroup, rep(1, ngroup))) 
        x <- rep( z[-1] , n)        
        y <- round(20 + ifelse(x==k, 6, 0) + rnorm(sum(n), 0, 2), 1)
   }   
   if(whichstory == 3) {
        n <- sample( 10:20, size = ngroup, replace = T)      
        z <- c("", paste0("Mall-", 1:ngroup))
        k <- sample( z , 1, prob = c(ngroup, rep(1, ngroup)))
        x <- rep( z[-1],  n)
        y <- round(50 + ifelse(x==k, 10, 0) + rnorm(sum(n), 0, 10), 1)
   }   
   if(whichstory == 4) {
        n <- sample( 10:20, size = ngroup, replace = T)         
        z <- c("", paste0("City-", LETTERS[1:ngroup]))        
        k <- sample( z , size=3, prob = c(ngroup, rep(1, ngroup)))
        x <- rep( z[-1] , n)
        y <- 10000  + rnorm(sum(n), 0, 3000)
        for(j in k) y[x==j] <- y[x==j] + + sample(c(-1,1),1)*sample( 1000+50*1:40, 1)
        y <- round(y, -1)
    }   
    if(whichstory == 5) {
        z <- c("", "Male", "Female")
        n <- sample( 10:20, size = ngroup, replace = T)         
        k <- sample( z , size=2, prob = c(ngroup, rep(1, ngroup)))
        x <- rep( z[-1], n)
        y <- 2.7 + rnorm(sum(n), 0, 0.5)
        for(j in k) y[x==j] <- y[x==j] + sample(c(-1,1),1)*sample( c(0.1, 0.2, 0.3), 1)
        y <- round(y, 1)
    }
    if(whichstory == 6) {
        z <- c("Male", "Female")
        n <- sample( 10:15, size = ngroup, replace = T)         
        x <- rep( z, n)
        y <- 10 + ifelse(x=="Male", 5 ,0) + rnorm(sum(n), 0, 2)
        y <- round(y, 2)
    }
    
    alpha <- ifelse( which == "CI", alpha , 100-alpha )
    if(whichstory == 1) {
       data <- data.frame(Length = y, Machine = x)    
       storytxt <- paste0("A company has a 2 machines that make widgets. 
                They randomly select some widgets made by each machine and measure their 
          lengths (in inches).<p>",
          ifelse( which == "CI", 
              paste("Find a ", alpha,"% confidence interval for the true mean difference 
                    of the lengths of the widgets"),
              paste("Test at the ", alpha, "% level whether the widgets made by the two machines 
              have the same lengths.")
          ))                          
    }  
    if(whichstory == 2) {
       data <- data.frame(Length = y, Employee = x)
       storytxt <- paste0("A call center has 2 people answering the phones. 
          They time a number of their calls.<p>",
          ifelse( which == "CI", 
              paste0("Find a ", alpha,"% confidence interval for the true mean difference 
                    in the lengths of the calls"),
              paste0("Test at the ", alpha, "% level whether the length of the calls are the same.")
          )) 
   }
   if(whichstory == 3) {
         data <- data.frame(Amount = y, Outlet = x)
         storytxt <- paste0("A store has outlets in two different malls. 
            They want to know the difference in the sales at the two 
            outlets, so they randomly choose a number of sales receipts from each outlet.<p>",
            ifelse( which == "CI", 
              paste0("Find a ", alpha,"% confidence interval for the true mean difference 
                    in the sales"),
              paste0("Test at the ", alpha, "% level whether the sales at the
              two outlets are the same.")
            ))
   }
   if(whichstory == 4) {
        data <- data.frame(Amount = y, City = x)
        storytxt <- paste0(" An insurance company wants to know what the 
            difference between the sizes of the insurance claims in two 
            cities is. They randomly choose a number claims in each of the cities.<p>",
            ifelse( which == "CI", 
              paste0("Find a ", alpha,"% confidence interval for the true mean difference 
                    in the amounts of the claims"),
              paste0("Test at the ", alpha, "% level whether claim amounts are the same.")
          ))
   }                            
   if(whichstory == 5) {
        data <- data.frame(GPA = y, School = x)   
        storytxt <- paste0("The chancellor of some university wants to 
            know how (if at all) the GPAs of Male and Female students 
            differ. He randomly chooses a number of students.<p>",
            ifelse( which == "CI", 
              paste0("Find a ", alpha,"% confidence interval for the true mean difference 
                    in the GPAs"),
              paste0("Test at the ", alpha, "% level whether the GPAs are the same.")
          ))           
   }
   if(whichstory == 6) {
        data <- data.frame(Weight = y, Bird = x)   
        storytxt <- paste0("A biologist is studying a certain species of birds. 
          He wants to find study the sizes of the male and female birds.<p>",
          ifelse( which == "CI", 
              paste0("Find a ", alpha,"% confidence interval for the true mean difference 
                    in the sizes of the birds"),
              paste0("Test at the ", alpha, "% level whther the male and female birds are the same size.")
          ))          
   }
   list(qtxt = paste0("<h5>",storytxt, g()$qtxt, "<hr>", moodle.table( data ), "<hr></h5>"), 
        atxt = paste0("<h5>", g()$atxt,"</h5>"), 
        category = category, quizname = quizname)   
   
}   

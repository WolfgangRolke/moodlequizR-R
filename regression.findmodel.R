regression.findmodel <- function(whichstory=1) {
    require(mvtnorm)
          
    if(whichstory==1)
      category <- "Regression/Find Model/quadratic" 
    if(whichstory==2)
      category <- "Regression/Find Model/log(y) vs x"
    quizname <- "problem - " 
    g <- function() {
      if(whichstory==1) {
        out <- slr(y, x, polydeg=2, return.result=TRUE)
        qtxt <- paste0("<p>The quadratic model for predicting the ", varnames[2], 
        " is<p>", varnames[2]," =  {:NM:%100%", out[1], ":", out[1]/100, "}&nbsp;&nbsp;", 
              ifelse( out[2]<0, "{:MC:~=-~+}", "{:MC:~-~=+}"), "&nbsp;&nbsp;", 
              "{:NM:%100%", abs(out[2]), ":", out[2]/100, "}", varnames[1], "&nbsp;&nbsp;",  
              ifelse( out[3]<0, "{:MC:~=-~+}", "{:MC:~-~=+}"), "&nbsp;&nbsp;", 
              "{:NM:%100%", abs(out[3]), ":", out[3]/100, "}", varnames[1], "^2")
         atxt <- paste0("The quadratic model is <p>", varnames[2], "&nbsp;&nbsp;= &nbsp;&nbsp;",   
            out[1], ifelse(out[2]>0, "+", ""), out[2], "&nbsp;&nbsp;", varnames[1],
             ifelse(out[3]>0, "+", ""), out[3], "&nbsp;&nbsp;", varnames[1], "^2")
         command <- paste0("slr(", varnames[2], ", ", varnames[1], ", polydeg=2)")                                       
      }
      if(whichstory==2) {
        out <- slr(log(y), x, return.result=TRUE)
        qtxt <- paste0("<p>The exponential model for predicting the ", varnames[2], 
        " is<p>log(", varnames[2],") =  {:NM:%100%", out[1], ":", out[1]/100, "}&nbsp;&nbsp;", 
              ifelse( out[2]<0, "{:MC:~=-~+}", "{:MC:~-~=+}"), "&nbsp;&nbsp;", 
              "{:NM:%100%", abs(out[2]), ":", out[2]/100, "}", varnames[1])
        atxt <- paste0("The exponential is <p>log(", varnames[2], ")&nbsp;&nbsp;= &nbsp;&nbsp;", 
           out[1], "&nbsp;&nbsp;", ifelse(out[2]>0, "+", ""), out[2], "&nbsp;&nbsp;", varnames[1])
        command <- paste0("slr(log(", varnames[2], "), ", varnames[1], ")")
      }        
      list(qtxt=qtxt, atxt=atxt, command=command)                     
    }                    
    n <- sample( 50:80, 1)
    if(whichstory == 1)  {
        x <- 1:n
        y <- round((x + x^2/5+rnorm(n,0,10))/10)
        y[y<1] <- 1
    }
    if(whichstory == 2)  {
        n <- sample(40:50, 1)
        x <- 20:n
        y <- round(exp(x/5+rnorm(length(x))))
        y[y<1] <- 1
     }  
    if(whichstory == 1) {
         varnames <- c("Day", "Faults")
         data <- data.frame(Day=x, Faults=y)
         qtxt <- paste0("A company has a machine that makes widgets. For ", n, " consecutive days
            they count the number of faulty widgets.")
    }  
    if(whichstory == 2) { 
         varnames <- c("Sugar","Microbes")
         data <- data.frame(Time=x, Microbes=y)
         qtxt <- paste0("A biologist wants to study the relationship between
            the number of microbes that grow in a petry dish and the amount of time
            (in grams). ")
    }
   
   g1 <- g() 
   list(qtxt = paste0("<h5>", qtxt, g1$qtxt,"</h5><hr>", moodle.table(data)), 
        atxt = paste0("<h5>", g1$atxt, "<p>R command: ", g1$command, "</h5>"), 
        category = category, quizname = quizname)     
}   

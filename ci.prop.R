ci.prop <- function(whichstory) {
   if(missing(whichstory)) whichstory <- sample( 1:5, 1)    
   category <- paste0("Inference for Proportion / Confidence interval / 
      Story = ", whichstory)
   quizname <- "problem - " 
   
   alpha <- sample(c(90,95,99), 1)
   al <- ifelse(alpha == 95, "", paste0(", conf.level = ", alpha) )
   n <- sample(100:1000,1)
   if(whichstory == 1)  p <- sample(c(4:16)*0.05,1)
   if(whichstory == 2)  p <- sample(c(8:12)*0.05,1)
   if(whichstory == 3)  p <- sample(c(4:8)*0.05,1)                 
   if(whichstory == 4)  p <- sample(c(8:16)*0.025,1)                 
   if(whichstory == 5)  {n <- sample(200:400,1);p <- sample(c(25:35)*0.01,1)}                 
   x <- rbinom(1,n,p)
   int1 <- 100*one.sample.prop(x = x, n = n, conf.level = alpha, return.result = TRUE)
   int2 <- 100*one.sample.prop(x = x, n = n, return.result = TRUE)
   command <- paste0("<p>&nbsp;<p>R command: one.sample.prop(x = ", x, ", n = ", n, al, ")")   
  
   low.nm <- nm(c(int1[1], int2[1]), w=c(100, 75), eps=0.2)
   high.nm <- nm(c(int1[2], int2[2]), w=c(100, 75), eps=0.2)
         
   if(whichstory == 1) {
        varname <- "red widgets"
        qtxt <- paste0("A company has a machine that makes widgets. Each widget is 
          either red or blue. At the end of one day they find that the machine made ", n,
          " widgets, of which ",x, " where red.")
   }                   
   if(whichstory == 2) {
        varname <- "votes for Mr. Miller"
        qtxt <- paste0("Mr. Miller is running for major. He has just done a 
            phone survey. Of the ",n ," people interviewed ",x," said they would 
            vote for him.") 
   }                         
   if(whichstory == 3) {
        varname <- "people who will buy the ice cream"
        qtxt <- paste0("The Tasty Ice Cream company is considering to introduce a 
        new flavor of ice cream. They want to know what percentage of people might buy it, 
        so they do a taste test. Of the ",n , " people participating in the test ",x ,
        " said they would buy it.")
   }                         
   if(whichstory == 4) {
        varname <- " car policies"
        qtxt <- paste0("An insurance company wants to know what percentage of their 
        policies are for cars. They randomly select ",n ," policies and find that ",x ,
        " of them are car insurances. ")               
   }                         
   if(whichstory == 5) {
        varname <- "students who failed the course"
        qtxt <- paste0("A certain course at some University has a very high failure rate. 
              The University randomly selects ", n ," students at the beginnig of the 
              semester. When they check at the end of the semester they find that ", x, 
              " of students had failed the course")               
               
   }   
   qtxt <- paste0(qtxt, "<p>&nbsp;<p>A ", alpha, "% confidence interval for the true 
    percentage of ", varname, " is given by (", low.nm, "%, ", high.nm, "%)")
  
   htxt <- "Your answer should be a <b>percentage</b>, not a proportion"
  
   atxt <- paste0("<p>&nbsp;<p>A ", alpha, "% confidence interval for the true 
    percentage of ", varname, " is given by (", int1[1], "%, ", int1[2], "%)")               
  
   list(qtxt = paste0("<h5>", qtxt, "</h5>"), 
        htxt = paste0("<h5>", htxt, "</h5>"),
        atxt = paste0("<h5>", atxt, command, "</h5>"), 
        category = category, quizname = quizname) 
} 

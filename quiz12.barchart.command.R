quiz12.barchart.command <- function(whichstory) {
   require(base64) 
   if(missing(whichstory)) 
      whichstory <- sample(1:3, 1)
   category <- paste("Categorical Data / Barchart for Contingency Table / Story = ", whichstory) 
   quizname <- "problem - "
  
   n <- sample(50:80, 1)
   rho <- runif(1, -0.7, 0.7)
   whichpercentage <- sample( c("row", "col"), 1)
   ordr <- c(1:2) 
   if(runif(1)<0.5) ordr <- c(2,1)
   if(whichstory==1) {
      varnames <- c("Supplier", "Quality")
      data <- gen.cont.table.data(n, c("Supplier_1","Supplier_2"), 
          c("Low","Medium","High"), FALSE, rho=rho)
      qtxt <- paste0("A company has just received shipments of electronic parts from two suppliers. 
      They randomly select ", n, " parts and rate their quality as either low, 
      medium or high. The data is below.")
   }   
   if(whichstory==2) {
      varnames <- c("Treatment", "Speed")
      data <- gen.cont.table.data(n, c("Treatment","Control"), 
          c("1","2","3"), FALSE, rho=rho)
      qtxt <- paste0("A researcher wants to test the reflexes of people after they have
         participated in a treatment program as compared to those who have not in a control group. He
         randomly selects ", n, " people and measures their reflexes, coded as slow=1,
         average=2 and fast=3. The data is below.")
   }      
   if(whichstory==3) {
      varnames <- c("Happiness", "Gender")
      grps <- c("1","2","3")
      data <- gen.cont.table.data(n, c("Male","Female"), grps, FALSE, rho=rho)
      qtxt <- paste0("An sociologist is doing a study on the relationship between the gender of 
         a person and and their general happiness, coded as low=1, average=2 or high=3. She randomly selects ", 
         n, " people and determines their happiness. The data is below.")
   }
   colnames(data) <- varnames    
#   print(whichpercentage)  
   plt <- barchart(data[,ordr[1]], data[,ordr[2]], percentage=whichpercentage, return.graph=TRUE)
   plt64 <- png64(plt)
   if(ordr[1]==1) 
     command <- paste0("barchart(", varnames[1], ", ", varnames[2], 
          ", percentage = \"", whichpercentage, "\") ")
   else
     command <- paste0("barchart(", varnames[2], ", ", varnames[1], 
          ", percentage = \"", whichpercentage, "\") ")

   qtxt <- paste(qtxt, "<p>The R command that draws this graph is ",
           sa(command), "
      <p>Hint: read the data into R, work until you have a command that 
          creates EXACTLY this 
          graph, then copy-paste this command into the answer box")
   htxt <- ""
   atxt <- paste0("<h5>", command, "</h5>")
   
   list(qtxt = paste("<h5>", qtxt,"</h5>", plt64, moodle.table(data)),  
        htxt = htxt,
        atxt = atxt, 
        category = category, quizname = quizname) }

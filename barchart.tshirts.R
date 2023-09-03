barchart.tshirts <- function() {
    require(base64)
    category <- "Categorical Data / Tshirst - Barchart"
    quizname <- " problem -"
    
    n <- sample(100:150, 1)
    Tshirts <- sample(c("Small", "Medium", "Large", "X-Large"), size = n, replace = T, prob = c(1,2,4,3))
    datatbl <- moodle.table(Tshirts, 10)
    out <- length(Tshirts[Tshirts == "Medium"])
    out[2] <- round(out/n*100, 1)
    plt <- barchart(table(Tshirts), Percent="Grand", newOrder = c(3,2,1,4), return.graph = TRUE)
    plt64 <- png64(plt)
    qtxt <- paste0("<h5>A store was trying to see how many t shirts of different sizes they were selling. 
                The results were <hr>",
                datatbl, 
                "<hr>Answer the following questions:<p>
                a) the number of Medium size Tshirts is: {2:NM:=", out[1], ":0.1}</p><p>
                b) the percentage of Medium size Tshirts is 
                  {2:NM:=", out[2], ":0.1~%80%", out[2], ":0.0001~%50%", out[2]/100, ":0.0001}</p><p>
                c) give the R command that draws the following graph: (use variable name: Tshirts)<p>",
                plt64, "<p>
                {4:SHORTANSWER_C:%100%barchart(*table*(*Tshirts*)*,*Percent*=*\"*Grand*\"*,*newOrder*=*c(*3*,*2*,*1*,*4)*)
                ~%75%barchart(*table*(*Tshirts*)*,*Percent*=*\"*Grand*\"*)
                ~%75%barchart(*table*(*Tshirts*)*,*newOrder*=*c(*3*,*2*,*1*,*4)*)
                ~%50%barchart(*table*(*Tshirts*)*)}</h5>")
   atxt <- paste0("<h5>Correct answers:
                  <p><p>the number of Medium size Tshirts is: ", out[1], 
                      "  <br>R Command: length(Tshirts[Tshirts == \"Medium\"]) or table(Tshirts)
                  <p><p>the percentage of Medium size Tshirts is ", out[2], "%  
                        <br>R Command: round(length(Tshirts[Tshirts == \"Medium\"])/length(Tshirts)*100, 1)
                  <p><p>R Command for graph:<br> barchart(table(TShirts), Percent = \"Grand\", newOrder = c(3,2,1,4))                  
                 </h5>")
   list(qtxt = qtxt, atxt = atxt, category = category, quizname = quizname)              
}    

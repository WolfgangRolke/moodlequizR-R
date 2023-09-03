quiz13.mean.median <- function(which = c("mean", "median")) {
    category <- paste0("Summary Statistics/", paste0(which," - ", collapse = ""))
    quizname <- "problem -"
    n <- sample(15:30, 1)
    type <- sample(0:5, 1)
    if(type == 0) {x <- round(rnorm(n, 50000, 10000), -2); dg <- (-1) }   
    if(type == 1) {x <- round(rnorm(n, 10, 3), 1); dg <- 2 }
    if(type == 2) {x <- round(rnorm(n, 100, 20)); dg <- 1 }
    if(type == 3) {x <- round(runif(n), 2); dg <- 3 }
    if(type == 4) {x <- round(runif(n, 30, 70)); dg <- 1 }
    if(type == 5) {x <- round(runif(n, 0, 0.1), 3); dg <- 4 }
    out <- round( c(mean(x), median(x), sd(x)), dg)
    datatbl <- moodle.table(x)
    qMean <- ifelse( "mean" %in% which,
        paste0("<p>the mean is {2:NM:=", out[1], "~%80%", out[1], ":", out[1]/1000, "}<p>"), "")
    qMedian <- ifelse( "median" %in% which,
        paste0("<p>the median is {2:NM:=", out[2], "~%80%", out[2], ":", out[2]/1000, "}<p>"), "")
    aMean <- ifelse( "mean" %in% which,
        paste0("<p>mean = ", out[1], "<br>R Command: mean(x)<p>"), "")
    aMedian <- ifelse( "median" %in% which,
        paste0("<p>median = ", out[2], "<br>R Command: median(x)<p>"), "")
        
    qtxt <- paste0("<h5> Here is some data:<hr>", datatbl, "<hr>", 
            qMean, qMedian, "<h5>") 
    atxt <- paste0("<h5>Correct answers", aMean, aMedian, "</h5>")
    htxt <- "<h5>Don't forget to round correctly!</h5>"
    list(qtxt = qtxt, htxt = htxt, atxt = atxt, 
           category = category, quizname = quizname)
}    

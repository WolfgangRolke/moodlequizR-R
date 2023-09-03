quiz17.zscore.better <- function(whichstory) {
   if(missing(whichstory)) whichstory <- sample(1:3, 1) 
   category <- paste0("Summary Statistics / z score / Story = ", whichstory)
   quizname <- "problem - " 
   
   if(whichstory==1) {
      xbar <- round(runif(2, 70, 80), 1)
      sigbar <- round(runif(2, 10,20), 1)
      x <- round(runif(2, 70, 80), 1)
      z <- round((x-xbar)/sigbar, 2)
      if(z[1]<z[2]) w <- c(0, 100)
      else w <- c(100, 0)
      data <- matrix(0, 2, 3)
      dimnames(data) <- list(c("Exam 1", "Exam 2"), 
          c("Your Score", "Class Mean", "Class Std. Dev."))
      data[1,] <- c(x[1], xbar[1], sigbar[1])      
      data[2,] <- c(x[2], xbar[2], sigbar[2])
      qtxt <- paste("The table below shows your score as well as the class
      means and standard deviations in two exams. Relative to the class, you
      did better in ", mc(c("Exam 1", "Exam 2"), w))
      atxt <- paste0("z score of Exam 1 = (", x[1], "-", xbar[1],  
          ")/", sigbar[1], " = ", z[1], 
          "<p>z score of Exam 2 = (", x[2], "-", xbar[2],  
          ")/", sigbar[2], " = ", z[2],
          "<p>", z[1], ifelse(z[1]<z[2], " < ", " > "), z[2], " so ",
          ifelse(z[1]<z[2], " Exam 2 ", " Exam 1 "), " was better")  
   }   
   
   if(whichstory==2) {
      xbar <- round(c(runif(1, 120, 160), runif(1, 150, 200)))
      sigbar <- round(c(runif(1, 10, 20), runif(1, 15, 30)))
      x <- round(c(runif(1, 120, 160), runif(1, 150, 200)))
      z <- round((x-xbar)/sigbar, 2)
      if(z[1]<z[2]) w <- c(0, 100)
      else w <- c(100, 0)
      data <- matrix(0, 2, 3)
      dimnames(data) <- list(c("Mary", "John"), 
          c("Weight", "Mean", "Std. Dev."))
      data[1,] <- c(x[1], xbar[1], sigbar[1])      
      data[2,] <- c(x[2], xbar[2], sigbar[2])
      qtxt <- paste("The table below shows the weight of a man (John) and a 
        women (Mary) as well as the means and standard deviations of men and 
        women in general. Relative to their gender, who is heavier?", 
          mc(c("Mary", "John"), w))
      atxt <- paste0("z score of Mary = (", x[1], "-", xbar[1],  
          ")/", sigbar[1], " = ", z[1], 
          "<p>z score of John = (", x[2], "-", xbar[2],  
          ")/", sigbar[2], " = ", z[2],
          "<p>", z[1], ifelse(z[1]<z[2], " < ", " > "), z[2], " so ",
          ifelse(z[1]<z[2], " John ", " Mary "), " is heavier")  
   }             

   if(whichstory==3) {
      xbar <- round(c(runif(1, 50, 60), runif(1, 90, 100)))
      sigbar <- round(c(runif(1, 30, 50), runif(1, 30, 50)))
      x <- round(c(runif(1, 70, 80), runif(1, 110, 130)))
      z <- round((x-xbar)/sigbar, 2)
      if(z[1]<z[2]) w <- c(0, 100)
      else w <- c(100, 0)
      data <- matrix(0, 2, 3)
      dimnames(data) <- list(c("Race 1", "Race 2"), 
          c("Weight", "Mean", "Std. Dev."))
      data[1,] <- c(x[1], xbar[1], sigbar[1])      
      data[2,] <- c(x[2], xbar[2], sigbar[2])
      qtxt <- paste("The table below shows the times a runner needed in two
      races, as well as the means and standard deviations of all runners. 
        Relative to the other runners, in which race did he perform better?", 
          mc(c("Race 1", "Race 2"), w))
      atxt <- paste0("z score of Race 1 = (", x[1], "-", xbar[1],  
          ")/", sigbar[1], " = ", z[1], 
          "<p>z score of Race 2 = (", x[2], "-", xbar[2],  
          ")/", sigbar[2], " = ", z[2],
          "<p>", z[1], ifelse(z[1]<z[2], " < ", " > "), z[2], " so he did better in",
          ifelse(z[1]<z[2], " Race 2 ", " Race 1 "))  
   }             

   if(whichstory==4) {
#for exam 1   
      xbar <- round(c(runif(1, 280, 285),runif(1, 286, 291)), 1)
      sigbar <- round(runif(2, 2, 5))
      x <- round(xbar - runif(2, 1, 5))
      z <- round((x-xbar)/sigbar, 2)
      if(z[1]>z[2]) w <- c(0, 100)
      else w <- c(100, 0)
      data <- matrix(0, 2, 3)
      dimnames(data) <- list(c("Race 1", "Race 2"), 
          c("Score", "Mean", "Std. Dev."))
      data[1,] <- c(x[1], xbar[1], sigbar[1])      
      data[2,] <- c(x[2], xbar[2], sigbar[2])
      qtxt <- paste("A golf player has competed in two tournaments. The table below 
        shows his scores, as well as the means and standard deviations of all players. 
        In which tournament did he play better, relative to the other players?
        <p>&nbsp;<p>Note: in golf the lower the score the better!<p>&nbsp;<p>", 
          mc(c("Tournament 1", "Tournament 2"), w))
      atxt <- paste0("z score of Tournament 1 = (", x[1], "-", xbar[1],  
          ")/", sigbar[1], " = ", z[1], 
          "<p>z score of Tournament 2 = (", x[2], "-", xbar[2],  
          ")/", sigbar[2], " = ", z[2],
          "<p>", z[1], ifelse(z[1]<z[2], " < ", " > "), z[2], " so he did better in",
          ifelse(z[1]>z[2], " Tournament 2 ", " Tournament 1 "))  
   }

          
   list(qtxt = paste0("<h5>", qtxt, "</h5><hr>", moodle.table(data)),
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 

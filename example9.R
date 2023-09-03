example9=function() {
   category="MoodleR Examples / 9"
   quizname="problem -"
   E=round(runif(1, 0.02, 0.04), 3) # desired error of interval
   pnull=round(runif(1, 0.3, 0.4), 2) # true proportion
   cl=sample(c(90,99), 1) # desired confidence level
   zalpha=qnorm(1-(1-cl/100)/2)
   n=round(pnull*(1-pnull)*zalpha^2/E^2)
   res=as.list(1:4)
   res[[1]]= cl
   res[[2]]= 100*pnull
   res[[3]]= E
   res[[4]]= n
   qtxt =  paste0( "<p>We are planning a survey likely voters in an election. We will then find a  ", res[[1]]," % confidence interval for the true proportion of voters for  candidate ABC. If we expect the candidate to get around  ", res[[2]]," % of the votes and we want the confidence interval to have an error of ", res[[3]]," . The sample size required is  ",moodleR::nm(res[[4]], w = 100, eps = 1)," </p>" )
   atxt =  paste0( "<p> ","","  ","","  ",""," The sample size is  ", res[[4]]," </p>" )
   htxt = "  "
   list(qtxt = paste0("<h5>", qtxt, "</h5>"),
       htxt = paste0("<h5>", htxt, "</h5>"),
       atxt = paste0("<h5>", atxt, "</h5>"),
       category = category, quizname = quizname)
}

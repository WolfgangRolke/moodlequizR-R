example7=function() {
   category="MoodleR Examples / 7"
   quizname="problem -"
   n=700+1*sample(0:100, 1)
   truep=runif(1, 0.2, 0.4) # true proportion
   x=sample(0:1,size=n, replace=TRUE, prob=c(truep,1-truep))
   m=length(x[x==0]) # number of successes
   
   cl=sample(c(90,99), 1) # randomly chosen confidence level
   ci1=round(100*c(binom.test(m,n, conf.level=cl/100)$conf.int), 1)
   ci2=round(100*c(binom.test(m,n)$conf.int), 1)  # interval if wrong (95%) cl is used, for partial credit
   L=c(ci1[1], ci1[1]/100, ci2[1],ci2[1]/100) 
   U=c(ci1[2], ci1[2]/100, ci2[2],ci2[2]/100)
   res=as.list(1:5)
   res[[1]]= n
   res[[2]]= m
   res[[3]]= cl
   res[[4]]= L
   res[[5]]= U 
   qtxt =  paste0( "<p>In a survey of  ", res[[1]]," people  ", res[[2]]," said they would vote for candidate ABC in the next election. A  ", res[[3]]," % confidence interval for the percentage of voters for ABC is given by ( ",moodleR::nm(res[[4]], w = c(100, 90, 80, 70), eps = c(0,0.01,10,0.1))," %,  ",moodleR::nm(res[[5]], w = c(100,90,80,70), eps = c(0,0.001,10,0.1))," %)</p>" )
   atxt =  paste0( "<p>In a survey of  ", res[[1]]," people  ", res[[2]]," said they would vote for candidate ABC in the next election. A  ", res[[3]]," % confidence interval for the percentage of voters for ABC is given by ( ", res[[4]]," %,  ", res[[5]]," %)</p>" )
   htxt = "  "
   list(qtxt = paste0("<h5>", qtxt, "</h5>"),
       htxt = paste0("<h5>", htxt, "</h5>"),
       atxt = paste0("<h5>", atxt, "</h5>"),
       category = category, quizname = quizname)
}

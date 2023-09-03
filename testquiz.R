testquiz=function() {
   category="Test"
   quizname="problem -"
   x=1
   y=2
   res=as.list(1:2)
   res[[1]]= 
   res[[2]]= 2
   qtxt =  paste0( "<p>NA ",moodlequizR::nm(res[[1]], w = 100, eps = 0)," <p>y= ",moodlequizR::nm(res[[2]], w = 100, eps = 0)," </p>" )
   atxt =  paste0( "<p>NA ", res[[1]]," <p>y= ", res[[2]]," </p>" )
   htxt = "  "
   list(qtxt = paste0("<h5>", qtxt, "</h5>"),
       htxt = paste0("<h5>", htxt, "</h5>"),
       atxt = paste0("<h5>", atxt, "</h5>"),
       category = category, quizname = quizname)
}

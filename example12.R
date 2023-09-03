example12=function() {
   category="MoodleR Examples / 12"
   quizname="problem -"
   a=round(runif(1, 1, 3), 1) # random coefficient
   x=round(runif(1, 0, 1), 2) # random point for evaluation
   A=round(runif(1, 0, 1), 2) # random endpoints for integral
   B=round(runif(1, 1, 2), 2)
   whichfun=sample(1:2, 1) # sin or cosin?
   fun=ifelse(whichfun==1,"\\sin","\\cos")
   if(whichfun==1) {
     f=function(x) sin(a*x)
     F=function(x) -1/a*cos(a*x) # antiderivative
     df=function(x) a*cos(a*x)  # first derivative
     d2f=function(x) -a^2*sin(a*x) # second derivative
     I=round(integrate(f,A,B)$value) # integral
   }
   else {
     f=function(x) cos(a*x)
     F=function(x) 1/a*sin(a*x)
     df=function(x) -a*sin(a*x) 
     d2f=function(x) -a^2*cos(a*x)
     I=round(integrate(f,A,B)$value, 3)
   }
   res=as.list(1:11)
   res[[1]]= fun
   res[[2]]= a
   res[[3]]= x
   res[[4]]= round(f(x),3)
   res[[5]]= x
   res[[6]]= round(df(x),3)
   res[[7]]= x
   res[[8]]= round(d2f(x), 3)
   res[[9]]= A
   res[[10]]= B
   res[[11]]= round(F(B)-F(A), 3)
   qtxt =  paste0( "<p>Consider the function $$f(x)=\\ ", res[[1]]," <p>( ", res[[2]]," x)$$<p>$$f( ", res[[3]]," )=$$ ", res[[4]]," <p>$$f'( ", res[[5]]," )=$$ ",moodleR::nm(res[[6]], w = 100, eps = 0)," <p>$$f''( ", res[[7]]," )=$$ ",moodleR::nm(res[[8]], w = 100, eps = 0.01)," <p>$$\\int_{ ", res[[9]]," }^{ ", res[[10]]," } f(x)dx$$= ",moodleR::nm(res[[11]], w = 100, eps = 0.01)," </p>" )
   atxt =  paste0( "<p>Consider the function $$f(x)=\\ ", res[[1]]," <p>( ", res[[2]]," x)$$<p>$$f( ", res[[3]]," )=$$ ", res[[4]]," <p>$$f'( ", res[[5]]," )=$$ ", res[[6]]," <p>$$f''( ", res[[7]]," )=$$ ", res[[8]]," <p>$$\\int_{ ", res[[9]]," }^{ ", res[[10]]," } f(x)dx$$= ", res[[11]]," </p>" )
   htxt = "  "
   list(qtxt = paste0("<h5>", qtxt, "</h5>"),
       htxt = paste0("<h5>", htxt, "</h5>"),
       atxt = paste0("<h5>", atxt, "</h5>"),
       category = category, quizname = quizname)
}

quiz24.find.area <- function() {
   category <- paste0("Probability / Area")
   quizname <- "problem - " 
   
  g=function(x, v=0.3) {
    y=ifelse(x<v, x/v, 1)/(1-0.5*v)
    y[x<0]=0
    y[x>1]=0
    y
  }
  f <- function(v, a, b) {
     x=seq(0, 1, length=1000)
     df=data.frame(x=x, y=g(x, v))
     plt=ggplot(df, aes(x, y))+geom_line(size=1.5)
     if(missing(a))  return(plt)
     shade=subset(df, x>a&x<b)
     plt=plt + 
       geom_area(data=shade, aes(x,y), alpha=0.4, fill="lightblue")
       plt=plt+
         geom_segment(x = a, y = 0, xend = a, yend = g(a,v))+
         geom_segment(x = v, y = g(a,v), xend = v, yend = g(v,v)) +
         geom_segment(x = b, y = 0, xend = b, yend = g(b,v)) +
         geom_segment(x = a, y = g(a,v), xend = b, yend = g(a,v)) +
         annotate("text", x = 0.9*v, y=1.1*g(a,v), label="A",size=5)+
         annotate("text", x = (v+b)/2, y=(g(a,v)+g(b,v))/2, label="B",size=5)+
         annotate("text", x = (a+b)/2, y=g(a,v)/2, label="C",size=5) 
     p=c(0.5*(v-a)*(g(b,v)-g(a,v)), (b-v)*(g(b,v)-g(a,v)),
         (b-a)*g(a,v))
     p=round(c(p, sum(p)), 2)
     list(plt=plt, p=p)    
   }
   v=round(runif(1, 0.3, 0.7), 2)
   plt1.64=png64(f(v))
   a=round(runif(1, 0.1, v-0.1), 2)
   b=round(runif(1, v+0.1, 0.9), 2)
   z=f(v=v,a=a,b=b)
   plt2.64=png64(z$plt)
   p=z$p
   qtxt <- paste0("The graph below shows a probability density. 
           Find P(", a, "< X <", b, ")=", nm(p[4], 100, 0.05), 
           "<p>Hint: two points on the graph are (", a, ",", round(g(a,v),2), ") and (", v, ",", round(g(v,v),2), ") 
           <p>", plt1.64)
   htxt <- "Probability=Area under Curve"
   atxt <- paste("Area of triangle A = ", p[1], "
           <p>Area of rectangle B = ", p[2], "
           <p>Area of rectangle C = ", p[3], "
           <p>P(", a, "< X <", b, ") = total area = ", p[4], "<p>", plt2.64)
       
   list(qtxt = paste0("<h5>", qtxt, "</h5>"),
        htxt = paste0("<h5>", htxt, "</h5>"), 
        atxt = paste0("<h5>", atxt, "</h5>"), 
        category = category, quizname = quizname) 
} 

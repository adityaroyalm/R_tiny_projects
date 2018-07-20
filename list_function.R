powert<-function(x,p){
  modified=(x^p-1)/p
  return(modified)
}
curve(powert(islands$Area,-1))

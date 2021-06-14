# Semana 11

# Q1
RaizBissecao = function(a, b, f, erro){
  if(f(a)*f(b)>0) stop("f(a) precisa ter um sinal oposto a f(b)")
  repeat{    
    m = (a+b)/2
    if((b-a)<=erro*2) return(m)
    if(f(a)*f(m)<0){b = m} else{a=m}
  }
}

# testando com a funcao do exercicio
f = function(x){return(exp(x) -1/x)}
x = seq(0.1,2,l=100)
plot(x,f(x),type="l")

RaizBissecao(0.1, 2, f, 0.1)

# d
RaizBissecaoRec = function(a, b, f, erro){
    if(f(a)*f(b)>0) stop("f(a) precisa ter um sinal oposto a f(b)")
    repeat{
    m = (a+b)/2
    if((b-a)<=erro)  return(m)
    if(f(a)*f(m)<0) {return(RaizBissecaoRec(a,m,f,erro))} else 
                    {return(RaizBissecaoRec(m,b,f,erro))}
}}
RaizBissecaoRec(0.1, 2, f, 0.1)

# e
RaizBissecao2 = function(a, b, f, erro){
  if(f(a)*f(b)>0) stop("f(a) precisa ter um sinal oposto a f(b)")
  pms = NULL
  i = 1
  v = NULL
  repeat{    
    m = (a+b)/2
    pms = c(pms, m)
    if((b-a)<=erro) return(list(m,pms))
    if(f(a)*f(m)<0){
      b = m
      v[i] = b
      i = i + 1
    }else{
      a=m
      v[i] = a
      i = i + 1
        }
  }
  return(v)
}
a = 0.1
b = 2.0
u     = RaizBissecao2(a,b,f, 0.1)

# f
saida = u[[2]]
plot(0,xlab="x",ylab="f(x)",xlim=c(0,b),ylim=c(f(a),f(b)),type="n", 
     bty="n", cex.lab=1.4, cex.axis=1.4)
grid()
segments(x0=0,y0=0,x1=b,y1=0)
segments(x0=0,y0=f(a),x1=0,y1=f(b))
curve(exp(x)-1/x,col="blue",add=T, lwd=2)
for(i in 1:length(saida)){
  points(saida[i],0,col="red",pch=18,cex.lab=1)
  text(saida[i],-0.2,i,col="red")
}

# Q2
# f(x) = x^2-3
f2 = function(x){return(x^2-3)}
RaizBissecao(0,4,f2, 0.001)
sqrt(3)

# Q3
# A)
f = function(x){
  return(exp(x) - 2 + x^2)
}

curve(f(x))

# B)
Q3 = function(a,b,f,e){
     if(f(a)*f(b)>0) stop("f(a) precisa ter um sinal oposto a f(b)")
     repeat{
     m = (a+b)/2
     if(b-a<2*e)return(m)
     if(f(a)*f(m)<0){b = m
     }else{a = m}
     }
}

Q3(0,10,f,0.01)

# C)
points(x = 0.5371094, y = 0)
f(0.5371094)





#Semana 12

# Q1
# a
xa     = c(1.1,1.2,1.3,1.4)
fa     = c(9.025013,11.02318,13.46374,16.44465)
dfa    = NULL
dfa[1] = (fa[2] - fa[1])/(xa[2]-xa[1])
dfa[2] = (fa[3] - fa[1])/(xa[3]-xa[1])
dfa[3] = (fa[4] - fa[2])/(xa[4]-xa[2])
dfa[4] = (fa[4] - fa[3])/(xa[4]-xa[3])
dfa

# b
xb     = c(7.4,7.6,7.8,8.0)
fb     = c(-68.31929,-71.69824,-75.15762,-78.69741)
dfb    = NULL
dfb[1] = (fb[2] - fb[1])/(xb[2]-xb[1])
dfb[2] = (fb[3] - fb[1])/(xb[3]-xb[1])
dfb[3] = (fb[4] - fb[2])/(xb[4]-xb[2])
dfb[4] = (fb[4] - fb[3])/(xb[4]-xb[3])
dfb

# Q2
# a
funcao_dfa = function(x){return(2*exp(2*x))}
funcao_dfa(x)
dfa

# comparando qual limite chegou mais perto do valor verdadeiro
funcao_df(x)[2]
dfa[2]
(fa[3] - fa[2])/(xa[3]-xa[2])
(fa[2] - fa[1])/(xa[2]-xa[1])

funcao_df(x)[3]
dfa[3]
(fa[4] - fa[3])/(xa[4]-xa[3])
(fa[3] - fa[2])/(xa[3]-xa[2])

# b
funcao_dfb  = function(x){return(1/(x+2) - 2*(x+1))}
funcao_dfb(xb)
dfb

# Q3

# a
# conjunto dos reais

# b
DerivadaNumerica = function(x0, f, erro){
  h   = 1
  x1  = x0 + h
  x2  = x0 - h
  fx1 = f(x1)
  fx2 = f(x2)
  repeat{
    if( is.null(fx1) || is.null(fx2)) {
      h   = h/2
      x1  = x0 + h
      x2  = x0 - h
      fx1 = f(x1)
      fx2 = f(x2)      
    }
    break
  }
  d   = (fx1 - fx2) / (2*h)
  repeat{
    h   = h/2
    x1  = x0 + h
    x2  = x0 - h
    fx1 = f(x1)
    fx2 = f(x2)
    repeat{
      if( is.null(fx1) || is.null(fx2)) {
        h   = h/2
        x1  = x0 + h
        x2  = x0 - h
        fx1 = f(x1)
        fx2 = f(x2)      
      }
      break
    }
    dtil = (fx1 - fx2) / (2*h)
    if(abs(d-dtil)<erro) return(dtil)
    d   = dtil
  }  
}


# testando com outras funcoes
# f(x)   = x^2
# f'(x)  = 2x
f1      = function(x){return(x^2)}
df1     = function(x){return(2*x)}
x0      = 3
erro    = 0.1
DerivadaNumerica(x0, f1, erro)
df1(x0)  

# f(x)   = x^3+x^2
# f'(x)  = 3x^2+2x
f2      = function(x){return(x^3+x^2)}
df2     = function(x){return(3*x^2+2*x)}

x0    = 3
erro  = 0.1
DerivadaNumerica(x0, f2, erro)
df2(x0)  
###########################################

# c
f = function(x){return(1/(x^2+1))}
df = function(x){return(-2*x/((x^2+1)^2))}
erro = 0.001
DerivadaNumerica(0, f, erro)
df(0)
DerivadaNumerica(-1/5, f, erro)
df(-1/5)
DerivadaNumerica(1/3, f, erro)
df(1/3)

# d
DerivadaNumericaRec = function(x0, f, erro, h){
  x1  = x0 + h
  x2  = x0 - h
  fx1 = f(x1)
  fx2 = f(x2)
  repeat{
    if( is.null(fx1) || is.null(fx2)) {
      h   = h/2
      x1  = x0 + h
      x2  = x0 - h
      fx1 = f(x1)
      fx2 = f(x2)      
    }
    break
  }
  d   = (fx1 - fx2) / (2*h)
  h   = h/2
  x1  = x0 + h
  x2  = x0 - h
  fx1 = f(x1)
  fx2 = f(x2)
  repeat{
    if( is.null(fx1) || is.null(fx2)) {
      h   = h/2
      x1  = x0 + h
      x2  = x0 - h
      fx1 = f(x1)
      fx2 = f(x2)      
    }
    break
  }
  dtil = (fx1 - fx2) / (2*h)
  if(abs(d-dtil)<erro) return(dtil)
  return(DerivadaNumericaRec(x0, f, erro, h))
}

# e
h = 1
DerivadaNumericaRec(0, f, erro, h)
df(0)
DerivadaNumericaRec(-1/5, f, erro, h)
df(-1/5)
DerivadaNumericaRec(1/3, f, erro, h)
df(1/3)

# Q4

# a
# quando x = -2 e x=1, tem-se f(x) = 0
# a funcao ln esta definida apenas para valores positivos, entao x>1 ou x<-2

f = function(x){
  if((x<=1)&&(x>=(-2))) return(NULL)
  return(log(x^2+x-2))
}

df = function(x){return((2*x+1)/(x^2+x-2))}

# b
DerivadaNumericah = function(x0, f, erro, h){
  x1  = x0 + h
  x2  = x0 - h
  fx1 = f(x1)
  fx2 = f(x2)
  repeat{
    if( is.null(fx1) || is.null(fx2)) {
      h   = h/2
      x1  = x0 + h
      x2  = x0 - h
      fx1 = f(x1)
      fx2 = f(x2)      
    }
    break
  }
  d   = (fx1 - fx2) / (2*h)
  repeat{
    h   = h/2
    x1  = x0 + h
    x2  = x0 - h
    fx1 = f(x1)
    fx2 = f(x2)
    repeat
    {
      if( is.null(fx1) || is.null(fx2)) {
        h   = h/2
        x1  = x0 + h
        x2  = x0 - h
        fx1 = f(x1)
        fx2 = f(x2)      
      }
      break
    }
    dtil = (fx1 - fx2) / (2*h)
    if(abs(d-dtil)<erro) return(dtil)
    d   = dtil
  }  
}

# c
x0 = 3
h = 1
DerivadaNumericah(x0, f, erro, h)
DerivadaNumericaRec(x0, f, erro, h)
df(x0)

x0 = -5/2
h = 0.1
DerivadaNumericah(x0, f, erro, h)
DerivadaNumericaRec(x0, f, erro, h)
df(x0)

x0 = 4/3
h = 0.25
DerivadaNumericah(x0, f, erro, h)
DerivadaNumericaRec(x0, f, erro, h)
df(x0)

# d

# e

# Q5

# a) Conjunto dos números reais.

# b
f = function(x){return(exp(-x/3) * (1 + x/(x^2+1)) - 1)}

# c
erro=0.001
df = function(x){DerivadaNumerica(x, f, erro)}
df(0)

# a expressÃ£o analÃ?tica da derivada da f eh:
# du = derivada da funcao x * (xÂ²+1)^(-1)
dfexata = function(x){
  du = (x^2+1)^(-1) + x * (-1)*(x^2+1)^(-2) * 2 * x
  return(exp(-x/3)*(-1/3)*(1+x/(x^2+1)) + exp(-x/3)*(du))
}

df(0)
dfexata(0)

df(10)
dfexata(10)

# d
plot(f,xlim=c(-3,5))
abline(h=0)

RaizNewtonRaphson=function(x0, fx, dfx){
  erro = 0.001
  x = x0
  repeat{
    if( (fx(x-erro)*fx(x+erro)) <0) return(x)
    x = x - fx(x)/dfx(x)
  }
}

r1=RaizNewtonRaphson(-2, f, df)
r2=RaizNewtonRaphson(0, f, df)
r3=RaizNewtonRaphson(2, f, df)
abline(v=r1,lty=3,lwd=2)
abline(v=r2,lty=3,lwd=2)
abline(v=r3,lty=3,lwd=2)

plot(f,xlim=c(-3,5))
abline(h=0)
segments(x0=r1,y0=1,x1=r1,y1=-1,lty=2)
points(r1,0,pch=19,cex=1.2)
segments(x0=r2,y0=1,x1=r2,y1=-1,lty=2)
points(r2,0,pch=19,cex=1.2)
segments(x0=r3,y0=1,x1=r3,y1=-1,lty=2)
points(r3,0,pch=19,cex=1.2)


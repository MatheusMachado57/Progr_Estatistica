
# 10.1 A partir da Equação 10.1 encontre uma aproximações para e1 e e3 usando n = 4. 
# FaÇa as contas na mão.

#10.2

# a - Implemente uma função que recebe como entrada x ∈ R e n ∈ N e retorna o polinômio Pn i=0 x i i!
# avaliado em x. Vamos chamar essa função de pol_exp(x,n).

pol_exp <- function(x,n){
  soma <- 0
  fac <- 1
  for(i in 0:n){
    if(i>=2)fac <- i * fac
    soma <- x**i/fac + soma
  }
  return(soma)
}

# b - Digite o código a seguir para ver como esse polinômio se aproxima da função exponencial conforme seu grau cresce.

# > plot(exp,-4,4)
# > grid()
# > segments(x0=0,y0=0,x1=0,y1=150,lty=2)
# > curve(pol_exp(x,n=2),add=T,col="violet")
# > curve(pol_exp(x,n=3),add=T,col="red")
# > curve(pol_exp(x,n=4),add=T,col="blue")
# > curve(pol_exp(x,n=5),add=T,col="green")

# Veja que a aproximação melhora quanto mais perto de 0 for o ponto avaliado ou quando maior for o valor de n.

plot(exp,-4,4)
grid()
segments(x0=0,y0=0,x1=0,y1=150,lty=2)
curve(pol_exp(x,n=2),add=T,col="violet")
curve(pol_exp(x,n=3),add=T,col="red")
curve(pol_exp(x,n=4),add=T,col="blue")
curve(pol_exp(x,n=5),add=T,col="green")

#10.3

# a) Implemente o pseudocódigo visto em sala de aula que recebe como entrada x ∈ R e um erro e retorna uma aproximação para e^x.

f <- function(x,er){
  fa <- 1
  i <- 0
  soma <- 0
  repeat{
    if(i>1)fa <- i*fa 
    parcel <- x**i/fa
    if(abs(parcel) < er) break
    soma <- soma + parcel
    i <- i + 1
    
  }
  return(c(soma,i))
}

g = function(x,er){
    resp = f(-x^(2)/2,er)
    return(resp)
}

#10.5
pol_ln <- function(x,n){
  soma <- 0
  for(i in 1:n){
  fator <- (-1)**(i+1) *((x-1)**i)/i
  soma <- soma + fator
  }
return(soma)
}
#b

plot(log,0,4)
grid()
segments(x0=1,y0=-4,x1=1,y1=10,lty=2)
curve(pol_ln(x,n=2),add=T,col="violet")
curve(pol_ln(x,n=3),add=T,col="red")
curve(pol_ln(x,n=4),add=T,col="blue")
curve(pol_ln(x,n=5),add=T,col="green")

#10.6
#a
aprox_log <- function(x,er, p = 1){
  if(x > 2){
    x <- 1/x
    p <- -1
  }
  soma <- 0
  i <- 1
  repeat{
  fator <- (-1)**(i+1) *((x-1)**i)/i
  soma <- fator + soma
  if(abs(fator)<er)break
  i <- i +1
  }
  return(p*soma)
}

#c
any_base <- function(x,b,er){
  part1 <- aprox_log(x,er)
  part2 <- aprox_log(b,er)
  return(part1/part2)
}

#10.7

aprox_sen <- function(x,er){
  if(abs(x)>pi) return(aprox_sen(x-2*pi, er))
  aprox <- 0
  i <- 0
  repeat{
  fator <- (-1)**i * x**(2*i+1)/factorial((2*i +1))
  aprox <- aprox + fator
  if(fator<er)break
  i <- i +1
  }
  return(aprox)
}

############################################################################

ap = function(x,inc){
     if(x<=0){return("Erro")}
     if(x>=2){return(-ap(1/x,inc))}
     aprox = 0
     for(i in 1:10){
         a = 1
         if(i%%2==0){a = -a}
         aprox = aprox + a*((x-1)^i)/i
       
     }
     i = 11
     repeat{
        parcela = ((-1)^(i+1))*((x-1)^i)/i
        if(abs(parcela)<inc){return(aprox)}
        aprox = aprox + parcela
        i = i +1
     }
}
ap(3,0.00001)
log(3)































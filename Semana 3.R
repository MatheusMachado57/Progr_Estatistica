# Semana 3

# Q1
# OBS: Não use a função min pronta no R.
# a - Implemente uma função que recebe como argumento dois números reais e retorna o menor entre eles.
Menor = function(a,b) {
  if( (class(a)!="numeric") || (class(b)!="numeric") ){stop("As entradas precisam ser numéricas")}
  if(a<b){return(a)}else{return(b)}
}
Menor(2,"a")
Menor(2,5.6)
Menor(15.6,3)

# b - Implemente uma função que recebe como argumento três números reais e retorna o menor entre eles.
Menor = function(a,b,c) {
  if( (class(a)!="numeric") || (class(b)!="numeric") || (class(c)!="numeric") ){stop("As entradas precisam ser numéricas")}
  if( (a<b) && (a<c) ){ return(a) } else{
    if(b<c){return(b)} else{return(c)}
  }
}
Menor(2,"a",3)
Menor(2,5.6,0)

# Q2 - Implemente uma função que recebe como argumento o tamanho de cada lado de um triângulo e 
# retorna um texto informando se o triângulo é equilátero, isósceles ou escaleno. 
# Antes de fazer o exercício pense:

# Quantos argumentos a sua função vai receber?
# Quais são os valores aceitáveis para esses argumentos?
# Qual o tipo de objeto que a sua função deve retornar?

Tipo = function(a,b,c){
  if( (class(a)!="numeric") || (class(b)!="numeric") || (class(c)!="numeric") ){stop("As entradas precisam ser numéricas")}
  if( (a<=0) || (b<=0) || (c<=0) ){stop("As dimensões do triângulo precisam ser positivas e não nulas.")}
  if(a>=(b+c) || b>=(a+c) || c>=(a+b) || abs(b-c)>=a || abs(a-c)>=b || abs(a-b)>=a){stop("A medida de cada lado deve ser maior que o valor absoluto da diferença dos outros dois e menor que a soma destes lados")}
  if((a==b)&&(a==c)){ print("O triângulo é equilátero.")  }else{
    if( (a==b) || (a==c) || (b==c)){  print("O triângulo é isósceles.")    } else{  print("O triângulo é escaleno.") }
  }
}

Tipo("a",2,1)
Tipo(7,2,1)
Tipo(0,2,1)
Tipo(2,2,2)
Tipo(2,2,1)
Tipo(5,2,4)

# Q3 - Implemente uma função que recebe como argumento um vetor de números reais e
# retorna a quantidade de elementos positivos desse vetor. Não se esqueça de inciar
# todas as variáveis locais usadas em sua função. OBS: Depois que a sua função estiver
# pronta invente vetores para serem passados como argumento de forma a verificar se a função está
# funcionando como o esperado. Por exemplo, use a função para contar o número de elementos
# positivos em v = (1.0, 3.2, −2.1, 10.6, 0.0, −1.7, −0.5).

# item Implemente uma função que recebe como argumento um vetor de números reais
# v| e um número real α e retorna o número de elementos do vetor v menores que α.
Quant = function(v){
  if(class(v) !="numeric"){stop("O vetor precisa ser numérico.")}
  l    = length(v)
  cont = 0
  for(i in 1:l){
    if(v[i]>0){cont=cont+1}
  }
  return(cont)
}
Quant(c(1,-2,0,4))
Quant(c(1.0,3.2,-2.1,10.6,0.0,-1.7,-0.5))

# b
Quant = function(v,alfa){
  if(class(v) !="numeric" || class(alfa)!="numeric"){stop("As entradas precisam ser numéricas.")}
  if(length(alfa)>1){stop("O segundo argumento precisa ter apenas um número.")}
  l    = length(v)
  cont = 0
  for(i in 1:l)
  {
    if(v[i]<alfa){cont=cont+1}
  }
  return(cont)
}
Quant(c(1,-2,0,4),3)
Quant(c(1.0,3.2,-2.1,10.6,0.0,-1.7,-0.5),0)

# Q4
# a
Mult = function(n,m){
  if( (class(n)!="numeric") || (class(m)!="numeric") ){stop("As entradas precisam ser numéricas")}
  if(n<=0){stop("A primeira entrada precisa ser positiva.")}
  v = NULL
  for(i in 1:n){
    v[i] = (i-1)*m
  }
  return(v)
}
Mult(-9,9)
Mult(9,9)
# variaveis locais: i, v
# argumento da funcao: n e m

# b
Mult2 = function(m,K){
  if( (class(m)!="numeric") || (class(K)!="numeric") ){stop("As entradas precisam ser numéricas")}
  v = NULL
  i = 0
  repeat{
    u  = i*m
    i  = i+1
    if(u>=K){break}else{v=c(v,u)}
  }
  return(v)
}
Mult2(2,6)
# variaveis locais: i, v, u
# argumento da funcao: m e K

# b
QMult2 = function(m,K){
  if( (class(m)!="numeric") || (class(K)!="numeric") ){stop("As entradas precisam ser numéricas")}
  i = 0
  cont = 0
  repeat{
    u  = i*m
    i  = i+1
    if(u>=K){break}else{cont=cont+1}
  }
  return(cont)
}
QMult2(2,6)
# variaveis locais: i, cont, u
# argumento da funcao: m e K

# Q5
#a
Funcao1 = function(n){
  if( class(n)!="numeric" ){stop("A entrada precisa ser numérica")}
  if((n%%1!=0) || (n<0) ){stop("O número precisa ser natural.")}
  M = matrix(NA,n,n)
  i = 2
  while(i<=n)  {
    for(j in 1:n){
      M[i,j] = 2
    }
    i = i + 2
    print(i)
  }
  i = 1
  while(i<=n){
    for(j in 1:n){
      M[i,j] = 1
    }
    i = i + 2
  }
  return(M)
}
Funcao1(5)

# b
Funcao2 = function(n){
  if( class(n)!="numeric" ){stop("A entrada precisa ser numérica")}
  if((n%%1!=0) || (n<0) ){stop("O número precisa ser natural.")}
  M = matrix(NA,n,n)
  for(i in 1:n){
    for(j in 1:n){
      M[i,j] = j
    }
  }
  return(M)
}
Funcao2(5)

# c
Funcao3 = function(n){
  if( class(n)!="numeric" ){stop("A entrada precisa ser numérica")}
  if((n%%1!=0) || (n<0) ){stop("O número precisa ser natural.")}
  M = matrix(0,n,n)
  for(i in 1:n){
    M[i,i] = i
  }
  return(M)
}
Funcao3(5)

# Q6
# a
Funcao = function(v){
  if( class(v)!="numeric" ){stop("A entrada precisa ser numérica")}
  n = length(v)
  M = matrix(0,n,n)
  for(i in 1:n){
    M[i,i] = v[i]
  }
  return(M)
}
Funcao(c(5,9,60,10))

# b
Funcao = function(v){
  if( class(v)!="numeric" ){stop("A entrada precisa ser numérica")}
  n = length(v)
  M = matrix(0,n,n)
  for(j in 1:n){
    M[,j] = v
  }
  return(M)
}
Funcao(c(5,9,60,10))

# c
Funcao = function(v){
  if( class(v)!="numeric" ){stop("A entrada precisa ser numérica")}
  n = length(v)
  M = matrix(0,n,n)
  for(i in 1:n){
    M[i,] = v
  }
  return(M)
}
Funcao(c(5,9,60,10))

# Q7
# a
PA = function(x0){
  if( class(x0)!="numeric" ){stop("A entrada precisa ser numérica")}
  r = 3
  n = 10
  x = x0
  for(i in 2:n){
    x = c(x, x[i-1] + r)
  }
  return(x)
}
PA(10)

# b
PA = function(x0, r){
  if( (class(x0)!="numeric") || (class(r)!="numeric") ){stop("A entrada precisa ser numérica")}
  n = 10
  x = x0
  for(i in 2:n){
    x = c(x, x[i-1] + r)
  }
  return(x)
}
PA(10, 5)

# c
PA = function(x0, r, n){
  if( (class(x0)!="numeric") || (class(r)!="numeric")  || (class(n)!="numeric")){stop("A entrada precisa ser numérica")}
  if( n<=0 || (n%%1 != 0)){stop("n precisa ser natural")}
  x = x0
  for(i in 2:n)  {    x = c(x, x[i-1] + r)  }
  return(x)
}
PA(10, 5, 8)
# variaveis locais: i, x 
# argumento da funcao: x0, r,n 

# d
SOMA_PA = function(x0, r, n){
  x     = PA(x0,r,n)
  soma  = 0
  for(i in 1:n){
    soma = soma + x[i]
  } 
  return(soma)
}
SOMA_PA(10, 5, 8)
# variaveis locais: x, soma, i 
# argumento da funcao: x0, r,n 

# Q8
# a
Fibonacci = function(n){
  if( n<=0 || (n%%1 != 0) || (class(n)!="numeric")){stop("n precisa ser um número natural maior que zero")}
  if(n==1){return(1)} else{
    if(n==2){return(c(1,1))}else{
      F = c(1,1)
      for(i in 3:n){     F[i] = F[i-1] + F[i-2]          }
      return(F)
    }
  }
}
Fibonacci(1)
Fibonacci(2)
Fibonacci(5)

# b
Fibonacci = function(K){
  if( class(K)!="numeric" ){stop("a entrada precisa ser um número")}
  if(K<=1){print(paste("Não existem termos na sequência de Fibonacci menores que ",K,sep=""))}else{
    F = c(1,1)
    i = 2
    repeat{
      i     = i+1
      c  = F[i-1] + F[i-2]          
      if(c>=K){break} else{F[i] = c}
    }
    return(F)
  }
}
Fibonacci(1)
Fibonacci(2)
Fibonacci(25)

#c
Fibonacci = function(K){
  if( class(K)!="numeric" ){stop("a entrada precisa ser um número")}
  if(K<=1){return(0)}else{
    F = c(1,1)
    i = 2
    repeat{
      i     = i+1
      c  = F[i-1] + F[i-2]          
      if(c>=K){break} else{F[i] = c}
    }
    return(length(F))
  }
}
Fibonacci(1)
Fibonacci(2)
Fibonacci(25)

# Q9
# a
# o vetor v continua sendo (0,0,0,0,0)
# vet = (1, 2, 3, 0, 0)

# b
# Nao pois a variavel vet deveria ser um vetor com tamanho n mas, do jeito que estah, a saida eh 
# um vetor com 5 elementos igual ao vetor v e mudara apenas os k elementos do vetor v, sendo k<=5
f = function(n){
  v = NULL
  for(i in 1:n){v[i]=i}
  return(v)
}
vet=f(3)
vet

# Q10
# a
PG = function(x0, q, n){
  if( (class(x0)!="numeric") || (class(q)!="numeric")  || (class(n)!="numeric")){stop("A entrada precisa ser numérica")}
  if( n<=0 || (n%%1 != 0)){stop("n precisa ser natural")}
  x = x0
  for(i in 2:n)  {    x = c(x, x[i-1] * q)  }
  return(x)
}
PG(10, 2, 8)

# b
SOMA_PG = function(x0, q, m){
  x = PG(x0,q,m)
  soma  = x[1]
  if(m>1){
    for(i in 2:m)  {   soma  = soma + x[i]    }
  }
  return(soma)
}
SOMA_PG(10, 2, 2)

# c
SOMA_PG(1/2, 1/2, 10)

# d
SOMA_PG(1/2, 1/2, 30)

# e
# Como a razao q=1/2 pertence ao intervalo (-1, 1), temos que, quando m tende para infinito,
# o termo da pg x_n tende a zero e por isso a soma converge para x0 / (1-q) = ( 1/2 ) / ( 1/2 ) = 1

# f
# demonstrado a letra e computacionalmente:
options(digits=22)
n = 300
x = PG(1/2, 1/2, n)
x[250:300]
plot(1:n, x, type="l",ylab="valor de cada termo",xlab="índice",
     lwd=2, cex.axis=2, cex.lab=2, bty="n")
SOMA_PG(1/2, 1/2, 30)
SOMA_PG(1/2, 1/2, 300)

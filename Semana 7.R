# Semana 7

# Q1 - Implemente de forma recursiva uma função que recebe como entrada um número
# natural n e retorna a soma de todos os naturais até n, isto é, retorna Sn = Pn i=0 i = 0 + 1 + 2 + . . . + n.
SomaNaturais = function(n){
  if(class(n)!="numeric") stop("A entrada precisa ser um número.")
  if((n%%1!=0) || (n<0)) stop("A entrada precisa ser um número natural.")
  if(n==0) return(0)
  return(n+SomaNaturais(n-1))
}
n=40
SomaNaturais(n)
sum(1:n)

# Q2
# a - Implemente uma função que recebe como entrada um número natural n e retorna o n-ésimo termo da série Sn = Pn i=0 1 i! .
Fatorial = function(n){
  if( (n%%1!=0) || (n<0) || (!is.numeric(n)) )stop("o argumento precisa ser um número natural.")
  if(n==0)return(1)
  return(n*Fatorial(n-1))
}

Soma2 = function(n){
  if(class(n)!="numeric") stop("A entrada precisa ser um número.")
  if((n%%1!=0) || (n<0)) stop("A entrada precisa ser um número natural.")
  if(n==0) return(1)
  return(1/Fatorial(n)+Soma2(n-1))
}

# b - Teste a função implementada para diferentes valores de n e veja se quando n cresce Sn se aproxima de e = 2.718282...
n=50
Soma2(n)
sum(  1/factorial( seq(0,n,by=1) )  )

# Q3
# a
# Entrada: um número natural n
# Saída: a soma da série S_n
# Nome da Função: AproxPi
# 1 Se n não for um número natural, aparece uma mensagem de erro e finaliza a função.
# 2 Se n=0, retorne 4.
# 3 Retorne 4*(-1)^n / (2*n+1) + AproxPi(n-1).

# b
AproxPi = function(n){
  if((class(n)!="numeric") || (n%%1!=0) || (n<0)) stop("A entrada precisa ser um número natural.")
  if(n==0) return(4)
  return(4*(-1)^n / (2*n+1) + AproxPi(n-1))  
}

# c
AproxPi(1100)

# Q4
# a
# Entrada: um número natural n maior que zero
# Saída: a soma da sequência de Fibonacci
# Nome da Função: SomaFibonacci
# 1 Se n não for um número natural maior que 0, aparece uma mensagem de erro e finaliza a função.
# 2 Se n=1, retorne 1.
# 3 Retorne o n-ésimo termo da sequência Fibonacci + SomaFibonacci(n-1).

# b
Fibonacci = function(n){
  if( (n%%1!=0) || (n<=0) || (class(n)!="numeric" && class(n)!="integer") )stop("o argumento precisa ser um número natural.")
  if((n==1) || (n==2)){return(1)} 
  return(Fibonacci(n-1)+Fibonacci(n-2))
}
SomaFibonacci = function(n){
  if( (n%%1!=0) || (n<=0) || (class(n)!="numeric" && class(n)!="integer") )stop("o argumento precisa ser um número natural maior que 0.")
  if(n==1) {return(1)} 
  return(SomaFibonacci(n-1) + Fibonacci(n))  
}
n=10
SomaFibonacci(n)
soma=0
for(i in 1:n){soma=soma+Fibonacci(i)}
soma

# Q5
# a
# Entrada: um número natural n 
# Saída: a soma da sequência S_n
# Nome da Função: Soma_75
# 1 Se n não for um número natural, aparece uma mensagem de erro e finaliza a função.
# 2 Se n=0, retorne 1.
# 3 Retorne 1/(3^n) + Soma_75(n-1).

# b
Soma_75 = function(n){
  if( (n%%1!=0) || (n<0) || (class(n)!="numeric" && class(n)!="integer") )stop("o argumento precisa ser um número natural.")
  if(n==0) return(1)
  return(1/(3^n)+Soma_75(n-1))
}
n=4
Soma_75(n)
sum( 1 / (3^seq(0,n,by=1)) )

# c
Soma_ab = function(x,y){
  if( (y%%1!=0) || (y<0) || (class(y)!="numeric" && class(y)!="integer") )stop("o argumento precisa ser um número natural.")
  if( (x%%1!=0) || (x<0) || (class(x)!="numeric" && class(x)!="integer") )stop("o argumento precisa ser um número natural.")
  a = min(x,y)
  b = max(x,y)
  if(a==b) return(1/(3^a))
  return(1/(3^b) + Soma_ab(a,b-1))
}
a=3
b=9
Soma_ab(a,b)
sum( 1 / (3^seq(a,b,by=1)) )

# caso a<b, ou seja, a tem que ser diferente e menor que b:
Soma_ab2 = function(x,y){
  if( (y%%1!=0) || (y<0) || (class(y)!="numeric" && class(y)!="integer") )stop("o argumento precisa ser um número natural.")
  if( (x%%1!=0) || (x<0) || (class(x)!="numeric" && class(x)!="integer") )stop("o argumento precisa ser um número natural.")
  a = min(x,y)
  b = max(x,y)
  if(b==(a+1)) return(1/(3^a) + 1/(3^b))
  return(1/(3^b) + Soma_ab2(a,b-1))
}
a=3
b=5
Soma_ab2(a,b)
sum( 1 / (3^seq(a,b,by=1)) )

# Q6
# a
# Entrada: um número natural n 
# Saída: o termo da sequência f(n)=2^n
# Nome da Função: Termo
# 1 Se n não for um número natural, aparece uma mensagem de erro e finaliza a função.
# 2 Se n=0, retorne 1.
# 3 Retorne 2 * Termo(n-1).

# b
Termo = function(n){
  if( (n%%1!=0) || (n<0) || (class(n)!="numeric" && class(n)!="integer") )stop("o argumento precisa ser um número natural.")
  if(n==0) return(1) 
  return(2 * Termo(n-1))
}
n = 5
Termo(n)
2^n

# Q7
# a
# Entrada: um número natural n e um número real x
# Saída: o termo da sequência f(n)=x^n
# Nome da Função: Termox
# 1 Se n não for um número natural, aparece uma mensagem de erro e finaliza a função.
# 2 Se x não for um número, aparece uma mensagem de erro e finaliza o programa.
# 3 Se n=0, retorne 1.
# 4 Retorne x * Termox(n-1).

# b
Termox = function(n,x){
  if( (n%%1!=0) || (n<0) || (class(n)!="numeric" && class(n)!="integer") )stop("O primeiro argumento precisa ser um número natural.")
  if(class(x)!="numeric")stop("O segundo argumento precisa ser um número real.")
  if(n==0) return(1) 
  return(x * Termox(n-1,x))
}
n = 5
x = 4
Termox(n,x)
x^n

# Q8
# a
# Entrada: um vetor numérico v
# Saída: a quantidade de elementos nulos presentes no vetor v
# Nome da Função: QuantNulos
# 1 Se v não for um vetor numérico, aparece uma mensagem de erro e finaliza a função.
# 2 Faça n = o comprimento do vetor v
# 3 Faça soma = 0
# 4 Se v_1 for nulo, faça soma = 1
# 5 Se n=1, então retorna soma.
# 6 Faça w = vetor v excluindo o seu primeiro elemento
# 7 Retorna soma + QuantNulos(w)

# b
QuantNulos = function(v){
  if(class(v)!="numeric")stop("O argumento precisa ser um vetor númerico.")
  n = length(v)
  soma = 0
  if(v[1]==0) soma = 1
  if(n==1) return(soma)
  w = v[-1]
  return(QuantNulos(w) + soma)
}
v = c(0, 3, 4, 6, 0, 0, 0, 0, 0)
QuantNulos(v)
length(which(v==0))

# Q9
# a
# Entrada: um vetor numérico v
# Saída: o vetor v na ordem inversa
# Nome da Função: OrdemInversa
# 1 Se v não for um vetor numérico, aparece uma mensagem de erro e finaliza a função.
# 2 Faça n = o comprimento do vetor v
# 3 Se n=1, retorne v
# 4 Faça w = vetor v excluindo o seu último elemento
# 5 Retorne o vetor (v_n, OrdemInversa(w) )

# b
OrdemInversa = function(v){
  if(class(v)!="numeric")stop("O argumento precisa ser um vetor númerico.")
  n = length(v)
  if(n==1)return(v)
  w = v[-n]
  return(c(v[n],OrdemInversa(w)))
}  
OrdemInversa(c(2,3,4,5))

# Q10
# a
MDC = function(x,y){
  if( (y%%1!=0) || (y<0) || (class(y)!="numeric" && class(y)!="integer") )stop("o argumento precisa ser um número natural.")
  if( (x%%1!=0) || (x<0) || (class(x)!="numeric" && class(x)!="integer") )stop("o argumento precisa ser um número natural.")
  a = min(x,y)
  b = max(x,y)
  if(a==0) return(b)
  return( MDC(a,b%%a) )
}
MDC(105, 252)

# b
MDC(125,325)
MDC(2829, 861)
MDC(299,217)

# Q11
# usando o pseudo codigo da apostila
Hanoi = function(N,Origem,Saida,Auxiliar){
  if(N==1)return(paste("Mova um disco do pino ",Origem," para o pino", Saida))
  mov1 = Hanoi(N-1,Origem,Auxiliar,Saida)
  mov2 = paste("Mova um disco do pino ",Origem," para o pino ",Saida)
  mov3 = Hanoi(N-1,Auxiliar,Saida,Origem)
  return(c(mov1, mov2, mov3))
}

Hanoi(1,"A","C","B")
Hanoi(2,"A","C","B")
Hanoi(3,"A","C","B")

# acho mais intuitivo desta forma
Hanoi = function(N,Origem,Auxiliar,Saida){
  if(N==1)return(paste("Mova um disco do pino ",Origem," para o pino", Saida))
  mov1 = Hanoi(N-1,Origem,Saida,Auxiliar)
  mov2 = paste("Mova um disco do pino ",Origem," para o pino ",Saida)
  mov3 = Hanoi(N-1,Auxiliar,Origem,Saida)
  return(c(mov1, mov2, mov3))
}

Hanoi(1,"A","B","C")
Hanoi(2,"A","B","C")
Hanoi(3,"A","B","C")

# a
Hanoi_a = function(N,Origem,Auxiliar,Saida){
  if(N==1) {
    print(paste("Mova um disco do pino ",Origem," para o pino", Saida))
  }else{
    mov1 = Hanoi_a(N-1,Origem,Saida,Auxiliar)
    print(paste("Mova um disco do pino ",Origem," para o pino ",Saida))
    mov3 = Hanoi_a(N-1,Auxiliar,Origem,Saida)
  }
}

Hanoi_a(1,"A","B","C")
Hanoi_a(2,"A","B","C")
Hanoi_a(3,"A","B","C")

# b
Hanoi_b = function(N,Origem,Auxiliar,Saida){
  if(N==1)return(paste("Mova um disco do pino ",Origem," para o pino", Saida))
  mov1 = Hanoi_b(N-1,Origem,Saida,Auxiliar)
  mov2 = paste("Mova um disco do pino ",Origem," para o pino ",Saida)
  mov3 = Hanoi_b(N-1,Auxiliar,Origem,Saida)
  v=c(mov1, mov2, mov3)
  return(v)
}
u=Hanoi_b(2,"A","B","C")
u
class(u)
length(u)
is.vector(u)

# c
Hanoi_c = function(N,Origem,Auxiliar,Saida){
  if(N==1){
    s = Origem
    c = Saida
    return(list(s,c))
  }
  mov1  = Hanoi_c(N-1,Origem,Saida,Auxiliar)
  s = c(mov1[[1]], Origem)
  c = c(mov1[[2]], Saida)
  mov3  = Hanoi_c(N-1,Auxiliar,Origem,Saida)
  s = c(s, mov3[[1]])
  c = c(c, mov3[[2]])
  return(list(s,c))
}
Hanoi_c(2,"A","B","C")
u = Hanoi_c(3,"A","B","C")
u
class(u)
length(u)
u[[1]]
u[[2]]

# Q12
Permutacoes = function(dados){
  n = length(dados)
  if(n==1){return(dados)}
  L = Fatorial(n)
  saida = NULL
  for(i in 1:n){
    saida = rbind(saida, cbind(dados[i], Permutacoes(dados[-i])   ) )       
  }
  return(saida)
}
Permutacoes("A")
Permutacoes(c("A","B","c"))

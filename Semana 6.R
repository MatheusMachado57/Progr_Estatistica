# Semana 6
#  Q1
Fatorial = function(n){
  if( (n%%1!=0) || (n<0) || (class(n)!="numeric") )stop("o argumento precisa ser um número natural.")
  if(n==0)return(1)
  return(n*Fatorial(n-1))
}
Fatorial(1.4)
Fatorial(-1)
Fatorial(10)

# Q2
Maximo = function(v){
  if(class(v)!="numeric")stop("O argumento precisa ser um vetor numérico.")
  n = length(v)
  if(n==1) {return(v)} else{
    w     = v[2:n]
    maxw  = Maximo(w)
    if(v[1]>maxw){return(v[1])} else{return(maxw)}
  }  
}
Maximo(c(1,6,0,9))

# Q3
# a
# Entrada: um vetor numerico v
# Saida: soma dos elementos do vetor v
# nome da funcao: SomaVetor
# 1 defina n como sendo o numero de elementos do vetor v
# 2 Se n=1, retorna v[1]
# 3 Defina w como sendo os demais elementos do vetor v excluindo o primeiro elemento
# 4 Faça soma = v[1] + SomaVetor(w)
# 5 Retorna soma

# b
SomaVetor = function(v){
  if(class(v)!="numeric")stop("O argumento precisa ser um vetor numérico.")
  n = length(v)
  if(n==1) {return(v)} else{
    w       = v[2:n]
    soma    = v[1] + SomaVetor(w)
    return(soma)
  }  
}
SomaVetor(c(1,6,10,9))

# Q4
# a
# Entrada: um vetor numerico v
# Saida: posição do elemento de maior valor
# nome da funcao: PosMaximo
# 1 defina n como sendo o numero de elementos do vetor v
# 2 Se n=1, retorna 1
# 3 Defina w como sendo os demais elementos do vetor v excluindo o último elemento
# 4 Faça pos = PosMaximo(w)
# 5 Se v_n > v_pos então retorna n. Caso contrário, retorna pos.

# b
PosMaximo = function(v){
  if(class(v)!="numeric")stop("O argumento precisa ser um vetor numérico.")
  n = length(v)
  if(n==1) {return(1)} else{
    w     = v[1:(n-1)]
    pos  = PosMaximo(w)
    if(v[n]>v[pos]){return(n)} else{return(pos)}
  }  
}
PosMaximo(c(1,6,0,9))

# Q5
# a
# x_n = x_(n-1) + 2*(n-1) + 1

# b
# Entrada = um numero natural n
# Saida = numero de bolinhas no grupo n
# Nome da funcao: Padrao
# 1 Se n = 1, retorne 1
# 2 Retorne Padrao(n-1) + 2*(n-1) + 1

# c
Padrao = function(n){
  if( (n%%1!=0) || (n<0) || (class(n)!="numeric") )stop("o argumento precisa ser um número natural.")
  if(n==1)return(1)
  return(Padrao(n-1) + 2*(n-1) + 1)
}
Padrao(1)
Padrao(2)

# Q6
# a
x_1 = 1.075*500 
x_1

x_2 = 1.075*x_1
x_2

x_3 = 1.075*x_2
x_3

# b 
x_n = 1.075*x_n-1

# c
# Entrada = um numero natural n
# Saida = dinheiro acumulado em n anos nesse investimento
# Nome da funcao: Investimento
# 1 Se n = 0, retorne 500
# 2 Retorne 1,075xInvestimento(n-1) 

# d
Investimento = function(n){
  if( (n%%1!=0) || (n<0) || (class(n)!="numeric") )stop("o argumento precisa ser um número natural.")
  if(n==0)return(500)
  return(1.075*Investimento(n-1))
}
Investimento(1)
Investimento(2)
Investimento(3)

# Q7
# a
Investimento2 = function(I, j, n){
  if( (n%%1!=0) || (n<0) || (class(n)!="numeric") )stop("o argumento precisa ser um número natural.")
  if(n==0)return(I)
  return((1+j/100)*Investimento(n-1))
}
Investimento2(500,7.5,1)
Investimento2(500,7.5,2)
Investimento2(500,7.5,3)

# b
Investimento2(1000,10,2) - Investimento2(1000,7.5,2)

# Q8
# a
y0 = 1200
j  = 2
p  = 150
y1 = y0 * (1+j/100) - p
y1 
y2 = y1 * (1+j/100) - p
y2 
y3 = y2 * (1+j/100) - p
y3

# b
y_n = (1+j/100)*y_n-1 - p

# c 
# Entrada = um numero natural n
# Saida = valor da divida apos n meses do inicio do financiamento
# Nome da funcao: Divida
# 1 Se n = 0, retorne 1200
# 2 Faca u =1,02xDivida(n-1)-150
# 3 Se u<=o, retorne 0
# 4 Retorne u

# d
Divida = function(n){
  if( (n%%1!=0) || (n<0) || (class(n)!="numeric") )stop("o argumento precisa ser um número natural.")
  if(n==0)return(1200)
  u = 1.02*Divida(n-1)-150
  if(u<=0)return(0) 
  return(u)
}
Divida(1)
Divida(2)
Divida(3)

# Q9
# a
DividaG = function(F,j,K,n){
  if( (n%%1!=0) || (n<0) || (class(n)!="numeric" && class(n)!="integer") )stop("o argumento precisa ser um número natural.")
  if(n==0)return(F)
  u = (1+j/100)*DividaG(F,j,K,n-1)-K
  if(u<=0)return(0) 
  return(u)
}
DividaG(1200,2,120,2)
DividaG(1200,2,150,1)
DividaG(1200,2,150,3)

# b
DividaG(1200,2,150,10)
DividaG(1200,2,120,10)

# Q10
# a
# Entrada = valor do financiamento F, juros j% e valor da parcela mensal K
# Saida = quantidade de meses necessária para quitar o financiamento
# Nome da funcao: DividaMesesR
# 1 Se F <= 0, retorne 0
# 2 Faca n = DividaMesesR(F*(1+j/100) - K, j, K) + 1
# 3 Retorne n

# b
# sem recursao
DividaMeses = function(F,j,K){
  y = F
  n = 0
  repeat{
    n = n+1
    y = (1+j/100)*y-K
    if(y<=0){break}
  }
  return(n)
}
DividaMeses(1200,2,150)
DividaMeses(1200,2,120)  

# com recursao
DividaMesesR = function(F,j,K){
  if(F<=0)return(0)
  n = DividaMesesR(F*(1+j/100) - K, j, K) + 1
  return(n)
}

# c
DividaMesesR(1200,2,150) 
DividaMesesR(1200,2,120) 
DividaMesesR(1200,2,200) 

# Q11
Fibonacci = function(n){
  if( (n%%1!=0) || (n<=0) || (class(n)!="numeric" && class(n)!="integer") )stop("o argumento precisa ser um número natural.")
  if((n==1) || (n==2)){return(1)} 
  return(Fibonacci(n-1)+Fibonacci(n-2))
}
Fibonacci(1)

# Q12
# a
# Entrada = um numero natural n
# Saida = valor de yn
# Nome da funcao: Valor_yn
# 1 Se n = 1 ou n=2, retorne 0
# 2 Retorne 2*Valor_yn(n-1)+Valor_yn(n-2)+n

# b
Valor_yn = function(n){
  if( (n%%1!=0) || (n<0) || (class(n)!="numeric" && class(n)!="integer") )stop("o argumento precisa ser um número natural.")
  if((n==1) || (n==2)){return(0)} 
  return(2*Valor_yn(n-1)+Valor_yn(n-2)+n)
}
Valor_yn(1)

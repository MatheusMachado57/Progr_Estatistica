# Semana 6

#  Q1 - Implemente de forma recursiva uma função que recebe como entrada um número natural n e retorna n!. 
# Não esqueça de verificar se o argumento passado como entrada é realmente um némero natural.

Fatorial = function(n){
  if( (n%%1!=0) || (n<0) || (class(n)!="numeric") )stop("o argumento precisa ser um número natural.")
  if(n==0)return(1)
  return(n*Fatorial(n-1))
}
Fatorial(1.4)
Fatorial(-1)
Fatorial(10)

# Q2 - Implemente de forma recursiva uma função que recebe como entrada um vetor v e retorna o valor máximo desse vetor.
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
# a - No caderno escreva um pseudo-código recursivo para o algoritmo que recebe como entrada um vetor v e retorna a soma dos elementos desse vetor.

# Entrada: um vetor numerico v
# Saida: soma dos elementos do vetor v
# nome da funcao: SomaVetor
# 1 defina n como sendo o numero de elementos do vetor v
# 2 Se n=1, retorna v[1]
# 3 Defina w como sendo os demais elementos do vetor v excluindo o primeiro elemento
# 4 Faça soma = v[1] + SomaVetor(w)
# 5 Retorna soma

# b - Agora no computador implemente o pseudo-código elaborado acima.
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
# a - No caderno escreva um pseudo-código recursivo para o algoritmo que recebe como
# entrada um vetor v e retorna a posição onde se encontra o valor máximo desse vetor. 
# Dica: em vez de definir w = (v2, v3, . . . , vn) defina w = (v1, v2, . . . , vn−1).
# Mas cuidado que isso muda um pouco a forma de pensar.

# Entrada: um vetor numerico v
# Saida: posição do elemento de maior valor
# nome da funcao: PosMaximo
# 1 defina n como sendo o numero de elementos do vetor v
# 2 Se n=1, retorna 1
# 3 Defina w como sendo os demais elementos do vetor v excluindo o último elemento
# 4 Faça pos = PosMaximo(w)
# 5 Se v_n > v_pos então retorna n. Caso contrário, retorna pos.

# b - Agora no computador implemente o pseudo-código elaborado acima.
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

# Q5 - Considere o seguinte padrão geométrico.

# Defina xn como o n´umero de bolinhas no grupo n.

# a - Encontre uma fórmula recursiva para xn, ou seja, escreva xn como função de xn−1.

# x_n = x_(n-1) + 2*(n-1) + 1

# b - Usando a fórmula encontrada, no caderno escreva um pseudo-código recursivo
# para o algoritmo que recebe como entrada um número natural n e retorna o
# número de bolinhas no grupo n.

# Entrada = um numero natural n
# Saida = numero de bolinhas no grupo n
# Nome da funcao: Padrao
# 1 Se n = 1, retorne 1
# 2 Retorne Padrao(n-1) + 2*(n-1) + 1

# c - Agora no computador implemente o pseudo-código elaborado acima.
Padrao = function(n){
  if( (n%%1!=0) || (n<0) || (class(n)!="numeric") )stop("o argumento precisa ser um número natural.")
  if(n==1)return(1)
  return(Padrao(n-1) + 2*(n-1) + 1)
}
Padrao(1)
Padrao(2)

# Q6 - Suponha que você vá investir R$ 500, 00 na poupança e que esta rende 7,5% ao ano.

# a - Calcule na mão o quanto de dinheiro você teria no banco depois de 1, 2 e 3 anos de investimento.
x_1 = 1.075*500 
x_1

x_2 = 1.075*x_1
x_2

x_3 = 1.075*x_2
x_3

# b - Tente achar uma equação que relacione o total de dinheiro acumulado em n anos
# de investimento com o total de dinheiro acumulado em n − 1 anos.
x_n = 1.075*x_n-1

# c - Usando a equação encontrada, no caderno escreva um pseudo-código recursivo
# para o algoritmo que recebe como entrada um número natural n e retorna o
# dinheiro acumulado em n anos nesse investimento.

# Entrada = um numero natural n
# Saida = dinheiro acumulado em n anos nesse investimento
# Nome da funcao: Investimento
# 1 Se n = 0, retorne 500
# 2 Retorne 1,075xInvestimento(n-1) 

# d - Agora no computador implemente o pseudo-código elaborado acima.
Investimento = function(n){
  if( (n%%1!=0) || (n<0) || (class(n)!="numeric") )stop("o argumento precisa ser um número natural.")
  if(n==0)return(500)
  return(1.075*Investimento(n-1))
}
Investimento(1)
Investimento(2)
Investimento(3)

# Q7 - Vamos generalizar o exercício anterior.
# a - Seja I o valor investido em uma aplicação de rentabilidade j% ao ano. 
# Implemente uma função que recebe como entrada I, j e n e retorna o total acumulado nessa
# aplicação após n anos.
Investimento2 = function(I, j, n){
  if( (n%%1!=0) || (n<0) || (class(n)!="numeric") )stop("o argumento precisa ser um número natural.")
  if(n==0)return(I)
  return((1+j/100)*Investimento(n-1))
}
Investimento2(500,7.5,1)
Investimento2(500,7.5,2)
Investimento2(500,7.5,3)

# b - Use a função implementada para descobrir quanto de dinheiro teríamos a mais
# se investíssemos R$ 1.000,00 durante 2 anos em um fundo que rendesse 10% ao
# ano em vez de 7,5%.
Investimento2(1000,10,2) - Investimento2(1000,7.5,2)

# Q8 - Suponha que você vai fazer um financiamento de R$ 1.200,00 e vai pagar juros compostos de 2% ao mês. 
# Considere que você pode pagar R$150,00 por mês.

# a - Calcule na mão o valor da sua dívida depois de 1, 2 e 3 meses.
y0 = 1200
j  = 2
p  = 150
y1 = y0 * (1+j/100) - p
y1 
y2 = y1 * (1+j/100) - p
y2 
y3 = y2 * (1+j/100) - p
y3

# b - Tente achar uma equação que relacione a sua dívida no mês n com a sua dívida no mês n − 1.
y_n = (1+j/100)*y_n-1 - p

# c - Usando a equação encontrada escreva no caderno um pseudo-código recursivo
# para o algoritmo que recebe como entrada um número natural n e retorna a sua
# dívida após n meses do início do financiamento. Não se esqueça de considerar o
# caso em que a dívida foi paga, nesse caso você deve retornar 0.

# Entrada = um numero natural n
# Saida = valor da divida apos n meses do inicio do financiamento
# Nome da funcao: Divida
# 1 Se n = 0, retorne 1200
# 2 Faca u =1,02xDivida(n-1)-150
# 3 Se u<=o, retorne 0
# 4 Retorne u

# d - Agora no computador implemente o pseudo-código elaborado acima.
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

# Q9 - Vamos generalizar o exercício anterior.

# a - Seja F o valor financiado a juros compostos de j% ao mês. 
# Considere K o valor das parcelas fixas que serão pagas todo mês. 
# Implemente uma função recursiva que recebe como entrada F, j, K e 
# n e retorna a dívida existente após n meses desde o início do financiamento.

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

# b - Use a função implementada acima para comparar o valor da sua dívida de R$
# 1.200,00 após 10 meses nos seguintes casos: parcela mensal de R$ 150,00 e parcela
# mensal de R$ 120,00. Considere os mesmos 2% de juros compostos ao mês.
DividaG(1200,2,150,10)
DividaG(1200,2,120,10)

# Q10 - Vamos fazer outro exerc´ıcio que considera um financiamento, mas agora estamos
# interessados na quantidade de meses para se pagar a dívida. Suponha que você
# vai fazer um financiamento de F rais e vai pagar juros compostos de j% ao mês.
# Considere que você pode pagar K reais por mês.

# a - No caderno escreva um pseudo-código recursivo para o algoritmo que recebe como
# entrada F, j e K e retorna o número de meses que você vai demorar para pagar a sua dívida.
# Dica: A simplificação na chamada recursiva ocorre na entrada F.

# Entrada = valor do financiamento F, juros j% e valor da parcela mensal K
# Saida = quantidade de meses necessária para quitar o financiamento
# Nome da funcao: DividaMesesR
# 1 Se F <= 0, retorne 0
# 2 Faca n = DividaMesesR(F*(1+j/100) - K, j, K) + 1
# 3 Retorne n

# b - Agora no computador implemente o pseudo-c´odigo elaborado acima.
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

# c - Use a função implementada acima para comparar o número de meses até a quitação da dívida de R$ 1.200,00 nos seguintes casos: p
# arcela mensal de R$ 150,00; parcela mensal de R$ 120,00 e parcela mensal de R$ 200,00. 
# Considere os mesmos 2% de juros compostos ao mês.

DividaMesesR(1200,2,150) 
DividaMesesR(1200,2,120) 
DividaMesesR(1200,2,200) 

# Q11 - Implemente uma função recursiva que recebe como entrada um número natural n e retorna o n-ésimo termo da sequência de Fibonacci.
Fibonacci = function(n){
  if( (n%%1!=0) || (n<=0) || (class(n)!="numeric" && class(n)!="integer") )stop("o argumento precisa ser um número natural.")
  if((n==1) || (n==2)){return(1)} 
  return(Fibonacci(n-1)+Fibonacci(n-2))
}
Fibonacci(1)

# Q12 - Considere a seguinte sequência definida a partir de uma equação de diferenças de segunda ordem.
# yn = 2yn−1 + yn−2 + n com y1 = 0 e y2 = 0

# a - No caderno escreva um pseudo-código recursivo para o algoritmo que recebe como
# entrada um número natural n e retorna o valor de yn.

# Entrada = um numero natural n
# Saida = valor de yn
# Nome da funcao: Valor_yn
# 1 Se n = 1 ou n=2, retorne 0
# 2 Retorne 2*Valor_yn(n-1)+Valor_yn(n-2)+n

# b - Agora no computador implemente o pseudo-código elaborado acima.
Valor_yn = function(n){
  if( (n%%1!=0) || (n<0) || (class(n)!="numeric" && class(n)!="integer") )stop("o argumento precisa ser um número natural.")
  if((n==1) || (n==2)){return(0)} 
  return(2*Valor_yn(n-1)+Valor_yn(n-2)+n)
}
Valor_yn(1)

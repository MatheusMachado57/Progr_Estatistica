# Semana 4

# Para todos os exercícios a seguir não se esqueça de:
# • Verificar sempre se as entradas passadas pelo usuário são viáveis para os cálculos das funções.
# • Inventar várias entradas para as funções implementadas a fim de verificar se elas estão funcionando corretamente.
# • Sempre que possível chamar as funções já implementadas dentro de uma nova função. Assim você simplifica bastante seu código.

# Q1 - Faça o que se pede sem usar as funções max e which.max do R.
# a  - No computador implemente o algoritmo visto em sala de aula que recebe como entrada um vetor v e retorna o seu valor máximo.
Maximo = function(v){
  if(class(v)!="numeric") stop("Entre com um vetor de números.")
  n     = length(v)
  max   = v[1]
  for (i in 2:n){    if(v[i]>max){max=v[i]}  }
  return(max)
}

Maximo(c(10,0,-2,8,90))

Maximo(c(90,0,-2,8,9))

Maximo(c(90,0,-2,8,90))

Maximo(c(10,0,-2,8,"A"))

# b - Em seu caderno escreva um pseudo-código para o algoritmo que recebe como
# entrada um vetor v e retorna a posição onde se encontra o máximo. Nesse item
# não é para usar o computador.

# Entrada: v = vetor de numeros
# Saida: posicao do maximo em v
# 1 Defina n = tamanho do vetor v
# 2 Defina maxi como sendo o valor máximo do vetor v
# 3 Inicialize o vetor de posições do máximo, denotado por pmaxi
# 4 Inicialize o contador imaxi em 0. Este contador ira guardar a quantidade de vezes que o maximo aparece.
# 5 Inicie i = 1
# 6 Se v_i==maxi, j=j+1 e pmaxi = i
# 7 Incremente i: i = i+1
# 8 Se i<=n, volte para a linha 5
# 9 Retorne pmaxi

# c - Agora, novamente no computador, implemente uma nova função que executa o
# pseudo-código elaborado no item 1b. Não use o mesmo nome da função implementada no item 1a.

Pos_Maximo = function(v){
  if(class(v)!="numeric") stop("Entre com um vetor de números.")
  n      = length(v)
  maxi   = Maximo(v)
  pmaxi  = NULL
  imaxi  = 0 
  for (i in 1:n){    if(v[i]==maxi){imaxi = imaxi + 1; pmaxi[imaxi] = i}  }
  return(pmaxi)
}
Pos_Maximo(c(10,0,-2,8,90))
Pos_Maximo(c(10,0,-2,8,90,90,90))

# Q2 - Faça o que se pede sem usar as funções min e which.min do R.

# a
# Entrada: v = vetor de numeros
# Saida: minimo em v
# 1 Defina n = tamanho do vetor v
# 2 Faca min= v_1
# 3 Inicie i = 2
# 4 Se v_i<min, min = v_i 
# 5 Incremente i: i = i+1
# 6 Se i<=n, volte para a linha 4
# 7 Retorne min

# b - Agora no computador implemente uma função que executa o pseudo-código elaborado do item 2a.
Minimo = function(v){
  if(class(v)!="numeric") stop("Entre com um vetor de números.")
  n     = length(v)
  min   = v[1]
  for (i in 2:n){    if(v[i]<min){min=v[i]}  }
  return(min)
}
Minimo(c(10,0,-2,8,90))

# c - De volta ao caderno escreva um pseudo-código para o algoritmo que recebe como
# entrada um vetor v e retorna a posição onde se encontra o mínimo. Nesse item
# não é para usar o computador.

# Entrada: v = vetor de numeros
# Saida: posicao do minimo em v
# 1 Defina n = tamanho do vetor v
# 2 Defina mini como sendo o valor minimo do vetor v
# 3 Inicialize o vetor de posições do minimo, denotado por pmini
# 4 Inicialize o contador imini em 0. Este contador ira guardar a quantidade de vezes que o minimo aparece.
# 4 Inicie i = 1
# 5 Se v_i==mini, j=j+1 e pmini = i
# 6 Incremente i: i = i+1
# 7 Se i<=n, volte para a linha 5
# 8 Retorne pmini 

# d - Novamente no computador implemente no uma nova função que executa o pseudo-código elaborado no item 2c. 
# Não use o mesmo nome da função implementada no item 2b
Pos_Minimo = function(v){
  if(class(v)!="numeric") stop("Entre com um vetor de números.")
  n      = length(v)
  mini   = Minimo(v)
  pmini  = NULL
  imini  = 0
  for (i in 1:n){    if(v[i]==mini){imini=imini+1; pmini[imini] = i}  }
  return(pmini)
}
Pos_Minimo(c(10,0,-2,8,90))
Pos_Minimo(c(10,0,-2,8,90,-2))

# Q3 - FaÇa o que se pede sem usar as funções mean e sum do R. No computador implemente
# o algoritmo visto em sala de aula que recebe como entrada um vetor v e retorna a
# média amostral dos valores em v.
# OBS: Se quiser use a função mean para comparação.
Media = function(v){
  if(class(v)!="numeric") stop("Entre com um vetor de números.")
  n     = length(v)
  soma  = 0
  for (i in 1:n){    soma = soma + v[i] }
  media = soma / n
  return(media)
}
Media(c(10,0,-2,8,90))
mean(c(10,0,-2,8,90))

# Q4 - Faça o que se pede sem usar a função median do R. Para ordenar o vetor de entrada
# use a função sort do R. No computador implemente o algoritmo visto em sala de
# aula que recebe como entrada um vetor v e retorna a mediana de v.
# OBS: Se quiser use a função median para comparação.
Mediana = function(v){
  if(class(v)!="numeric") stop("Entre com um vetor de números.")
  n     = length(v)
  vtil  = sort(v)
  if(n%%2 != 0){mediana = vtil[(n+1)/2]} else{mediana = (vtil[n/2] + vtil[n/2+1])/2}
  return(mediana)
}
Mediana(c(10,0,-2,8,90))
median(c(10,0,-2,8,90))

Mediana(c(10,0,-2,8,90,6))
median(c(10,0,-2,8,90,6))

# Q5 - Faça o que se pede sem usar a função quantile do R. Para ordenar o vetor de entrada use a função sort do R.

# a - No computador implemente o pseudo-código visto em sala de aula, baseado no Método 1, que recebe como entrada um vetor v e retorna os três quartis.
Quartis_1 = function(v){
  if(class(v)!="numeric") stop("Entre com um vetor de números.")
  n     = length(v)
  vtil  = sort(v)
  if(n%%2==0){
    k = n/2
    j = k+1
  } else {
    k = (n-1)/2
    j = k+2
  }
  v1 = vtil[1:k]
  v2 = vtil[j:n]
  q1 = Mediana(v1)
  q2 = Mediana(v)
  q3 = Mediana(v2)
  return( c(q1,q2,q3) )
}
Quartis_1(c(10,0,-2,8,90))

# b - No caderno escreva um pseudo-código que calcula os três quartis a partir do Método 2.

# Entrada: v = vetor com os numeros
# Saida: quartis de v
# 1 Defina n = tamanho do vetor v
# 2 Defina vtil como sendo o vetor v ordenado
# 3 Se n for par, seja k = n/2 e j = k+1
# 4 Se n for impar, seja k = (n+1)/2 e j= k
# 5 Defina v1 como sendo o vetor vtil das posicoes de 1 ate k
# 6 Defina v2 como sendo o vetor vtil das posicoes de j ate n
# 7 Defina q1 = mediana de v1
# 8 Defina q2 = mediana de v
# 9 Defina q3 = mediana de v2
# 10 Retorne o vetor (q1, q2, q3)

# c - No computador implemente o pseudo-código escrito no item acima.
Quartis_2 = function(v){
  if(class(v)!="numeric") stop("Entre com um vetor de números.")
  n     = length(v)
  vtil  = sort(v)
  if(n%%2==0){
    k = n/2
    j = k+1
  } else {
    k = (n+1)/2
    j = k
  }
  v1 = vtil[1:k]
  v2 = vtil[j:n]
  q1 = Mediana(v1)
  q2 = Mediana(v)
  q3 = Mediana(v2)
  return( c(q1,q2,q3) )
}
Quartis_1(c(10,0,-2,8,90))
Quartis_2(c(10,0,-2,8,90))
quantile(c(10,0,-2,8,90),c(0.25,0.5,0.75))

# Q6 - Fa¸ca o que se pede sem usar a função table ou algo parecido.

# a - No computador implemente o algoritmo visto em sala de aula que recebe como
# entrada um vetor v e retorna uma matriz cuja primeira linha guarda os diferentes
# valores de v e a segunda as frequências absolutas com que esses valores aparecem no vetor de dados.
FreqsAbs = function(v){
  if(class(v)!="numeric") stop("Entre com um vetor de números.")
  n       = length(v)
  val     = NULL
  freq    = NULL
  val[1]  = v[1]
  freq[1] = 1
  j       = 1
  for(i in 2:n){
    existe  = FALSE 
    for(k in 1:j){
      if(v[i]==val[k]){
        freq[k] = freq[k] + 1
        existe  = TRUE
      } 
    }
    if(!existe){
      val[j+1]  = v[i]
      freq[j+1] = 1  
      j         = j + 1
    }
  }
  M = matrix(c(val,freq),nrow=2,byrow=TRUE)
  return(M)
}
FreqsAbs(c(10,0,-2,8,90))
table(c(10,0,-2,8,90))

FreqsAbs(c(10,0,-2,8,90,90,90,0))
FreqsAbs(c(10,0,8,90,90,90,0))

# b - Repita o item 6a gerando agora as frequências relativas. 
# Não use o mesmo nome da função implementada no item 6a.
FreqsRel = function(v){
  if(class(v)!="numeric") stop("Entre com um vetor de números.")
  n       = length(v)
  v       = sort(v)
  val     = NULL
  freq    = NULL
  val[1]  = v[1]
  freq[1] = 1
  j       = 1
  for(i in 2:n){
    existe  = FALSE 
    for(k in 1:j){
      if(v[i]==val[k]){
        freq[k] = freq[k] + 1
        existe  = TRUE
      } 
    }
    if(!existe){
      val[j+1]  = v[i]
      freq[j+1] = 1  
      j         = j + 1
    }
  }
  freq = freq/n
  M = matrix(c(val,freq),nrow=2,byrow=TRUE)
  return(M)
}
FreqsRel(c(10,0,-2,8,90))
FreqsRel(c(10,0,-2,8,90,90,90,0))
FreqsRel(c(10,0,8,90,90,90,0))

# c - Faça agora uma outra função que recebe como argumento além do vetor v um objeto to tipo "logic", vamos chamá-lo de rel. 
# Se rel = T RUE a matriz de saída deve guardar as frequências relativas, caso contrário ela guarda a frequência absoluta.
Freq = function(n, rel){
  if(class(rel)!="logical") stop("O segundo argumento desta funcao deve ser um objeto do tipo logico onde TRUE retorna as freq relativas e FALSE retorna as freq absolutas")
  if(rel==TRUE){
    return(FreqsRel(v))
  }else{
    return(FreqsAbs(v))  
  }
}
Freq(c(10,0,8,90,90,90,0),TRUE)
Freq(c(10,0,8,90,90,90,0),FALSE)
Freq(c(10,0,8,90,90,90,0),"A")

# Q7 - Usando as funções implementadas em 6, implemente o algoritmo visto em sala de aula que recebe como entrada um vetor de dados e retorna a moda desse vetor.
Moda = function(v){
  M = FreqsAbs(v)
  j = Pos_Maximo(M[2,])
  return(M[1,j])
}
u = c(10,0,8,90,90,90,0)
FreqsAbs(u)
Moda(u)
u = c(10,0,8,90,90,0)
FreqsAbs(u)
Moda(u)

# Q8 

# a - Primeiro escreva no caderno um pseudo-código para o algoritmo que recebe como entrada um vetor v e retorna a sua Amplitude Total. 
# Não é para usar o computador ainda. Considere que você já sabe calcular os valores mínimo e máximo do seu vetor de entrada.

# Entrada: v = vetor com os numeros
# Saida: amplitude total de v
# 1 Defina min = valor minimo do vetor v
# 2 Defina max = valor maximo do vetor v
# 3 Retorne at = max - min

# b - Agora no computador implemente o pseudo-código feito do item 8a usando as funções implementadas nos exercícios 1 e 2.
AT = function(v){
  min = Minimo(v)
  max = Maximo(v)
  return( max - min )
}
AT(c(1,0,0,0,20,3))

# Q9
# a - Primeiro escreva no caderno um pseudo-código para o algoritmo que recebe como
# entrada um vetor v e retorna a sua Distância Interquartílica. Não é para usar o
# computador ainda. Considere que você já sabe calcular os quartis de v.

# Entrada: v = vetor com os numeros
# Saida: distancia interquartilica de v
# 1 Defina q1 = primeiro quartil do vetor v
# 2 Defina q3 = terceiro quartil do vetor v
# 3 Retorne di = q3 - q1

# b - Agora no computador implemente o passo-a-passo feito do item 9a usando as funções implementadas no exercício 5.
DI = function(v){
  qs  = Quartis_2(v)
  q1 = qs[1]
  q3 = qs[3]
  return( q3 - q1 )
}
DI(c(1,0,0,0,20,3))

# Q10 - Faça o que se pede sem usar as funções sd, mean e sum do R. 
# No computador implemente o algoritmo visto em sala de aula que recebe como entrada um vetor v e
# retorna a sua variância amostral. Para simplificar use a função implementada no exercício 3.
# OBS: Se quiser use a função sd para comparação.
VarAm = function(v){
  n = length(v)
  m = Media(v)
  soma = 0
  for(i in 1:n){
    soma = soma + (v[i]-m)^2
  }
  s2 = soma / (n-1)
  return(s2)
}
VarAm(c(1,0,0,0,20,3))
var(c(1,0,0,0,20,3))

# Q11
# a - Primeiro escreva no caderno um pseudo-código para o algoritmo que recebe como
# entrada um vetor v e retorna o seu Desvio Médio. Não é para usar o computador
# ainda. Considere que você já sabe calcular a média de v.

# Entrada: v = vetor com os numeros
# Saida: desvio medio dos valores de v
# 1 Defina n = tamanho do vetor v
# 2 Defina m = media amostral do vetor v
# 3 Inicie soma = 0
# 4 Inicie i = 1
# 5 Incremente a variavel soma: soma = soma + |v_i - m|
# 6 Incremente i: i = i+1
# 7 Se i <= n, volte para a linha 5
# 8 Faca s2 = soma / n
# 9 Retorne s2

# b - Agora no computador implemente o pseudo-código feito do item 11a.
DesAm = function(v){
  n = length(v)
  m = Media(v)
  soma = 0
  for(i in 1:n){
    soma = soma + abs(v[i]-m)
  }
  s2 = soma / n 
  return(s2)
}
x = c(1,0,0,0,20,3)
DesAm(x)
library(lsr)
aad(x)
aad

# Q12 - Fa¸ca o que se pede sem usar as funções mean e sum do R.
# a - Implemente o algoritmo visto em sala de aula que recebe como entrada dois
# vetores v e w e retorna a covariância amostral entre eles. Para simplificar use a
# função implementada no exercício 3.
CovAm = function(v,w){
  if( (class(v)!="numeric") || (class(w)!="numeric") ) stop("Os 2 argumentos precisam ser vetores numericos.")
  n = length(v)
  k = length(w)
  if(n!=k) stop("Os 2 vetores precisam ter a mesma dimensao")
  m_v = Media(v)
  m_w = Media(w)
  soma = 0
  for(i in 1:n){
    soma = soma + (v[i]-m_v)*(w[i]-m_w)
  }
  cova = soma / (n-1)
  return(cova)
}
v = c(1,2,5,8)
w = c(5,0,1)
CovAm(v,w)

v = c(1,2,5,8)
w = c(5,0,1,5)
CovAm(v,w)
cov(v,w)

# b - Desafio: vamos generalizar o item 12a. Implemente uma função que recebe como
# entrada uma matriz de dados. Considere que cada coluna dessa matriz representa
# um vetor de dados. A função implementada deve retornar a matriz de covariância
# amostral entre os vetores coluna dessa matriz.

# Lembre-se que a matriz de covariância é definida pela matriz cuja posição (i, j) guarda a
# covariância da variável i com a j.

# Entrada: M = matriz com os dados sendo que cada coluna representa uma variavel
# Saida: matriz de covariancias amostrais
# 1 Defina k = numero de variaveis
# 2 Inicie o array m 
# 3 Faca i = 1
# 4 Calcule a media da variavel i
# 5 Incremente i: i = i+1
# 6 Se i<=k, volte para a linha 4
# 7 Defina n como sendo o tamanho amostral de cada variavel
# 8 Inicie a matriz de covariancias Mcov 
# 9 Faca i = 1
# 10 Faca j = 1
# 11 Calcule Mcov[i,j] como sendo a covariancia da variavel i com a variavel j, isto é, dos dados na coluna i com os dados na coluna j
# 12 Incremente j: j = j+1
# 13 Se j<=k, volte para a linha 11
# 14 Incremente i: i = i+1
# 15 Se i<=k, volte para a linha 10
# 16 Retorne a matriz Mcov 

MCovAm = function(M){
  if(class(M)!="matrix") stop("Entre com uma matriz na qual cada coluna representa um conjunto de dados.")
  k     = ncol(M)
  m     = NULL
  for(i in 1:k){
    m[k] = Media(M[i,])
  }
  n     = nrow(M)
  Mcov  = matrix(NA,k,k)
  for(i in 1:k){
    for(j in 1:k){
      Mcov[i,j] = CovAm(M[,i],M[,j])
    }
  }
  return(Mcov)
}
v = c(1,2,5,8)
w = c(5,0,1,5)
M = matrix(c(v,w),ncol=2)
MCovAm(M)
cov(v,w)
var(v)
var(w)

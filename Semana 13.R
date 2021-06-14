# Semana 13

# Q1
f = function(x){abs(x-3) - 2}
curve(f(x), xlim = c(0,5))

# a)
# b)

# c)
# aproximacao por retangulos usando como altura pontos a esquerda
IntRet_inf = function(a, b, f, n){ 
  x       = a                        #ponto inferior
  S       = 0
  delta = (b-a)/n
  for(i in 1:n){
    S      = S + f(x)*delta 
    x      = x + delta
  }
  return(S)
}

IntRet_inf(0, 5, f, 50)
IntRet_inf(0, 5, f, 100)
IntRet_inf(0, 5, f, 150)

# Q2

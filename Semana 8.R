# Semana 8
# Q1
# a
OrdenaBolha = function(v){
  if( !is.vector(v))stop("O argumento de entrada precisa ser um vetor.")
  n = length(v)
  for(j in 1:(n-1)){
    for(i in 1:(n-j)){
      if(v[i]>v[i+1]){
        aux     = v[i+1]
        v[i+1]  = v[i]
        v[i]    = aux
      }
    }
  }
  return(v)
}
w = c(1,0,5,8,2,30,4)
w = c(1,0,5)
OrdenaBolha(w)
sort(w)

# b
OrdenaBolhaRec = function(v){
  if( !is.vector(v))stop("O argumento de entrada precisa ser um vetor.")
  n = length(v)
  if(n==1){return(v)}
  for(i in 1:(n-1)){
    if(v[i]>v[i+1]){
      aux     = v[i+1]
      v[i+1]  = v[i]
      v[i]    = aux
    }
  }
  w  = v[-n]
  w0 = OrdenaBolhaRec(w) 
  return(c(w0,v[n]))
}
w = c(1,0,5,8,2,30,4)
w = c(1,0,5)
OrdenaBolhaRec(w)
sort(w)

# c
OrdenaBolha2 = function(v){
  if( !is.vector(v))stop("O argumento de entrada precisa ser um vetor.")
  n = length(v)
  for(j in 1:(n-1)){
    troca = F
    i = 1
    for(i in 1:(n-j)){
      if(v[i]>v[i+1]){
        aux     = v[i+1]
        v[i+1]  = v[i]
        v[i]    = aux
        troca   = T
      }
    }
    if(troca==F){break}
  }
  return(v)
}
w = c(1,0,5,8,2,30,4)
w = c(1,0,2:10)
w = c(1,0,5)
OrdenaBolha2(w)
sort(w)

# d
OrdenaBolhaRec2 = function(v){
  if( !is.vector(v))stop("O argumento de entrada precisa ser um vetor.")
  n = length(v)
  if(n==1){return(v)}
  troca = F
  for(i in 1:(n-1)){
    if(v[i]>v[i+1]){
      aux     = v[i+1]
      v[i+1]  = v[i]
      v[i]    = aux
      troca = T
    }
  }
  if(!troca) return(v)
  w  = v[-n]
  w0 = OrdenaBolhaRec2(w) 
  return(c(w0,v[n]))
}
w = c(1,0,5,8,2,30,4)
w = c(1,0,5)
OrdenaBolhaRec2(w)
sort(w)

# Q2
# a
OrdenaBolha_2 = function(v){
  if( !is.vector(v))stop("O argumento de entrada precisa ser um vetor.")
  n = length(v)
  a = 0
  for(j in 1:(n-1)){
    for(i in 1:(n-j)){
      a = a + 1
      if(v[i]>v[i+1]){
        aux     = v[i+1]
        v[i+1]  = v[i]
        v[i]    = aux
      }
    }
  }
  return(list(v, a))
}
w = c(1,0,5,8,2,30,4)
OrdenaBolha_2(w)
sort(w)
sum(seq(1,length(w)-1,by=1))

# b
OrdenaBolhaRec_2 = function(v){
  if( !is.vector(v))stop("O argumento de entrada precisa ser um vetor.")
  n = length(v)
  if(n==1){return(list(v,0))}
  a = 0
  for(i in 1:(n-1)){
    a = a+1
    if(v[i]>v[i+1]){
      aux     = v[i+1]
      v[i+1]  = v[i]
      v[i]    = aux
    }
  }
  w  = v[-n]
  aux = OrdenaBolhaRec_2(w)
  w0 = aux[[1]]
  a  = a + aux[[2]] 
  return(list(c(w0,v[n]), a))
}
w = c(1,0,5,8,2,30,4)
w = c(1,0,5)
OrdenaBolhaRec_2(w)
sort(w)

# c
OrdenaBolha2_2 = function(v){
  if( !is.vector(v))stop("O argumento de entrada precisa ser um vetor.")
  n = length(v)
  a = 0
  for(j in 1:(n-1)){
    troca = F
    i = 1
    for(i in 1:(n-j)){
      a = a+1
      if(v[i]>v[i+1]){
        aux     = v[i+1]
        v[i+1]  = v[i]
        v[i]    = aux
        troca   = T
      }
    }
    if(troca==F){break}
  }
  return(list(v,a))
}
w = c(1,0,5,8,2,30,4)
w = c(1,0,2:10)
w = c(1,0,5)
OrdenaBolha2_2(w)
sort(w)

# d
OrdenaBolhaRec2_2 = function(v){
  if( !is.vector(v))stop("O argumento de entrada precisa ser um vetor.")
  n = length(v)
  if(n==1){return(list(v,0))}
  troca = F
  a = 0
  for(i in 1:(n-1)){
    a = a+1
    if(v[i]>v[i+1]){
      aux     = v[i+1]
      v[i+1]  = v[i]
      v[i]    = aux
      troca = T
    }
  }
  if(!troca) return(list(v,a))
  w  = v[-n]
  aux = OrdenaBolhaRec2_2(w) 
  w0 = aux[[1]]
  a = a + aux[[2]]
  return(list(c(w0,v[n]), a))
}
w = c(1,0,5,8,2,30,4)
w = c(1,0,5)
OrdenaBolhaRec2_2(w)
sort(w)

# Q3
# a
OrdenaBolha_3 = function(v){
  if( !is.vector(v))stop("O argumento de entrada precisa ser um vetor.")
  n = length(v)
  a = 0
  for(j in 1:(n-1)){
    for(i in 1:(n-j)){
      if(v[i]>v[i+1]){
        aux     = v[i+1]
        v[i+1]  = v[i]
        v[i]    = aux
        a       = a + 1        
      }
    }
  }
  return(list(v, a))
}


# b
OrdenaBolhaRec_3 = function(v){
  if( !is.vector(v))stop("O argumento de entrada precisa ser um vetor.")
  n = length(v)
  if(n==1){return(list(v,0))}
  a = 0
  for(i in 1:(n-1)){
    if(v[i]>v[i+1]){
      a       = a+1
      aux     = v[i+1]
      v[i+1]  = v[i]
      v[i]    = aux
    }
  }
  w  = v[-n]
  aux = OrdenaBolhaRec_3(w)
  w0 = aux[[1]]
  a  = a + aux[[2]] 
  return(list(c(w0,v[n]), a))
}

# c
OrdenaBolha2_3 = function(v){
  if( !is.vector(v))stop("O argumento de entrada precisa ser um vetor.")
  n = length(v)
  a = 0
  for(j in 1:(n-1)){
    troca = F
    i = 1
    for(i in 1:(n-j)){
      if(v[i]>v[i+1]){
        a       = a+1
        aux     = v[i+1]
        v[i+1]  = v[i]
        v[i]    = aux
        troca   = T
      }
    }
    if(troca==F){break}
  }
  return(list(v,a))
}

# d
OrdenaBolhaRec2_3 = function(v){
  if( !is.vector(v))stop("O argumento de entrada precisa ser um vetor.")
  n = length(v)
  if(n==1){return(list(v,0))}
  troca = F
  a = 0
  for(i in 1:(n-1)){
    if(v[i]>v[i+1]){
      a       = a+1
      aux     = v[i+1]
      v[i+1]  = v[i]
      v[i]    = aux
      troca = T
    }
  }
  if(!troca) return(list(v,a))
  w  = v[-n]
  aux = OrdenaBolhaRec2_3(w) 
  w0 = aux[[1]]
  a = a + aux[[2]]
  return(list(c(w0,v[n]), a))
}

OrdenaBolha_3(w)
OrdenaBolha2_3(w)
OrdenaBolhaRec_3(w)
OrdenaBolhaRec2_3(w)

# Q4
v = c(10,9,8,7,6,5,4,3,2,1)
v = c(1,3,5,5,4,0,-1,2,6,-2)
v = c(2,0,4,6,8,10,12,14,16,18,20)
v = c("fabio","ana","pedro","bruno", "bruna", "marco")
c1=OrdenaBolha_2(v)
c2=OrdenaBolha2_2(v)
c3=OrdenaBolhaRec_2(v)
c4=OrdenaBolhaRec2_2(v)
c(c1[[2]], c2[[2]], c3[[2]], c4[[2]])

# Q5
novafuncao = function(v){
  if( !is.vector(v))stop("O argumento de entrada precisa ser um vetor.")
  n = length(v)
  for(j in 1:(n-1)){
    for(i in 1:(n-j)){
      if(v[i]>v[i+1]){
        x11()
        plot(v)
        aux     = v[i+1]
        v[i+1]  = v[i]
        v[i]    = aux
      }
    }
  }
  return(v)
}
v = c(5,9,4,2,6,10,3,8,1,7)
w = novafuncao(v)
plot(w)

# Q6
# a
OrdenaBolhaD = function(v){
  if( !is.vector(v))stop("O argumento de entrada precisa ser um vetor.")
  n = length(v)
  for(j in 1:(n-1)){
    for(i in 1:(n-j)){
      if(v[i]<v[i+1]){
        aux     = v[i+1]
        v[i+1]  = v[i]
        v[i]    = aux
      }
    }
  }
  return(v)
}
w = c(1,0,5,8,2,30,4)
OrdenaBolhaD(w)
sort(w, decreasing =T)

# b
OrdenaBolhaRecD = function(v){
  if( !is.vector(v))stop("O argumento de entrada precisa ser um vetor.")
  n = length(v)
  if(n==1){return(v)}
  for(i in 1:(n-1)){
    if(v[i]<v[i+1]){
      aux     = v[i+1]
      v[i+1]  = v[i]
      v[i]    = aux
    }
  }
  w  = v[-n]
  w0 = OrdenaBolhaRecD(w) 
  return(c(w0,v[n]))
}
OrdenaBolhaRecD(w)
sort(w,decreasing=T)

# c
OrdenaBolhaD2 = function(v){
  if( !is.vector(v))stop("O argumento de entrada precisa ser um vetor.")
  n = length(v)
  for(j in 1:(n-1)){
    troca = F
    i = 1
    for(i in 1:(n-j)){
      if(v[i]<v[i+1]){
        aux     = v[i+1]
        v[i+1]  = v[i]
        v[i]    = aux
        troca   = T
      }
    }
    if(troca==F){break}
  }
  return(v)
}
OrdenaBolhaD2(w)
sort(w, decreasing=T)

# d
OrdenaBolhaRecD2 = function(v){
  if( !is.vector(v))stop("O argumento de entrada precisa ser um vetor.")
  n = length(v)
  if(n==1){return(v)}
  troca = F
  for(i in 1:(n-1)){
    if(v[i]<v[i+1]){
      aux     = v[i+1]
      v[i+1]  = v[i]
      v[i]    = aux
      troca = T
    }
  }
  if(!troca) return(v)
  w  = v[-n]
  w0 = OrdenaBolhaRecD2(w) 
  return(c(w0,v[n]))
}
OrdenaBolhaRecD2(w)
sort(w, decreasing=T)

# Q7
OrdenaRapidoRec = function(v){
  if(!is.vector(v)) stop("o argumento de entrada precisa ser um vetor")
  n = length(v)
  if(n==1)return(v)
  for(i in 2:n){
    j       = n
    achoui  = F
    if(v[1]<v[i]){
      achoui = T
      while(v[1]<v[j]){j=j-1}
      if(i<j){
        aux   = v[j]
        v[j]  = v[i]
        v[i]  = aux
      }else{
        aux   = v[1]
        v[1]  = v[j]
        v[j]  = aux
        break
      }
    }
  }
  if(!achoui){
    aux   = v[1]
    v[1]  = v[n]
    v[n]  = aux
  }
  if(j>1)
  {  
    we  = v[1:(j-1)] 
    weo = OrdenaRapidoRec(we)
    vo  = c(weo, v[j])
  }else{vo = v[j]}
  
  if(j<n)
  {
    wd = v[(j+1):n]
    wdo = OrdenaRapidoRec(wd)
    vo  = c(vo, wdo)
  }
  return(vo)
}
v = c(5,3,6,1,4)
v = c(37,33,48,12,92,25,86,57)
OrdenaRapidoRec(v)
sort(v)

# Q8
OrdenaRapidoRec_c = function(v){
  if(!is.vector(v)) stop("o argumento de entrada precisa ser um vetor")
  n = length(v)
  if(n==1)return(list(v,0))
  comp = 0
  for(i in 2:n){
    j       = n
    achoui  = F
    comp    = comp + 1
    if(v[1]<v[i]){
      achoui  = T
      while(v[1]<v[j]){
        j=j-1; comp = comp + 1; 
      }
      comp=comp+1
      comp  = comp + 1 
      if(i<j){
        aux   = v[j]
        v[j]  = v[i]
        v[i]  = aux
      }else{
        aux   = v[1]
        v[1]  = v[j]
        v[j]  = aux
        break
      }
    }
  }
  if(!achoui){
    aux   = v[1]
    v[1]  = v[n]
    v[n]  = aux
  }
  if(j>1){  
    we    = v[1:(j-1)] 
    aux   = OrdenaRapidoRec_c(we)
    weo   = aux[[1]]
    comp  = comp + aux[[2]]    
    vo    = c(weo, v[j])
  }else{vo = v[j]}
  
  if(j<n){
    wd    = v[(j+1):n]
    aux   = OrdenaRapidoRec_c(wd)
    wdo   = aux[[1]]
    comp  = comp + aux[[2]]    
    vo    = c(vo, wdo)
  }
  return(list(vo,comp))
}
v = c(5,3,6,1,4)
v = c(37,33,48,12,92,25,86,57)
OrdenaRapidoRec_c(v)
sort(v)

# Q9
OrdenaRapidoRec_t = function(v){
  if(!is.vector(v)) stop("o argumento de entrada precisa ser um vetor")
  n = length(v)
  if(n==1)return(list(v,0))
  troca = 0
  for(i in 2:n){
    j       = n
    achoui  = F
    if(v[1]<v[i]){
      achoui  = T
      while(v[1]<v[j]){        j=j-1      }
      if(i<j){
        aux   = v[j]
        v[j]  = v[i]
        v[i]  = aux
        troca = troca + 1
      }else{
        aux   = v[1]
        v[1]  = v[j]
        v[j]  = aux
        troca = troca + 1
        break
      }
    }
  }
  if(!achoui){
    aux   = v[1]
    v[1]  = v[n]
    v[n]  = aux
    troca = troca + 1
  }
  
  if(j>1)
  {  
    we    = v[1:(j-1)] 
    aux   = OrdenaRapidoRec_t(we)
    weo   = aux[[1]]
    troca = troca + aux[[2]]    
    vo    = c(weo, v[j])
  }else{vo = v[j]}
  
  if(j<n)
  {
    wd    = v[(j+1):n]
    aux   = OrdenaRapidoRec_t(wd)
    wdo   = aux[[1]]
    troca = troca + aux[[2]]    
    vo    = c(vo, wdo)
  }
  return(list(vo,troca))
}
v = c(5,3,6,1,4)
v = c(37,33,48,12,92,25,86,57)
OrdenaRapidoRec_t(v)
sort(v)

# Q10
v = c(10,9,8,7,6,5,4,3,2,1)
v = c(1,3,5,5,4,0,-1,2,6,-2)
v = c(2,0,4,6,8,10,12,14,16,18,20)
v = c("fabio","ana","pedro","bruno", "bruna", "marco")
a1=OrdenaRapidoRec_c(v)
a2=OrdenaRapidoRec_t(v)
c(a1[[2]], a2[[2]])

# Q11
OrdenaRapidoRecd = function(v){
  if(!is.vector(v)) stop("o argumento de entrada precisa ser um vetor")
  n = length(v)
  if(n==1)return(v)
  for(i in 2:n){
    j       = n
    achoui  = F
    if(v[1]>v[i]){
      achoui = T
      while(v[1]>v[j]){j=j-1}
      if(i<j){
        aux   = v[j]
        v[j]  = v[i]
        v[i]  = aux
      }else{
        aux   = v[1]
        v[1]  = v[j]
        v[j]  = aux
        break
      }
    }
  }
  if(!achoui){
    aux   = v[1]
    v[1]  = v[n]
    v[n]  = aux
  }
  
  if(j>1)
  {  
    we  = v[1:(j-1)] 
    weo = OrdenaRapidoRecd(we)
    vo  = c(weo, v[j])
  }else{vo = v[j]}
  
  if(j<n)
  {
    wd = v[(j+1):n]
    wdo = OrdenaRapidoRecd(wd)
    vo  = c(vo, wdo)
  }
  return(vo)
}
v = c(5,3,6,1,4)
OrdenaRapidoRecd(v)
sort(v, decreasing=T)

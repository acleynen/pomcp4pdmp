mu_prime_1_rond_Phi <- function(zet, ti, m_) {
  mu = (b1*(zet*exp(-(vprime2*(m_==2)+
                        vprime1*(m_==1))*ti)))^(beta1)
  return(mu)
}

#tau1 vector
#nu1 vector

mu_1 <- function(s)
{
  if (s >= 0 & s <= tau1[1]){mu = s*nu1[1]/tau1[1]}
  if (s >= tau1[1] & s <= tau1[2]){mu = nu1[1]}
  if (s >= tau1[2] & s <= tau1[3]){mu = nu1[1] + (s-tau1[2])*(nu1[2]-nu1[1])/(tau1[3]-tau1[2])}
  if (s >= tau1[3]){mu = nu1[2]}
  return(mu)
}


mu_2 <- function(s)
{
  if (s >= 0 & s <= tau2[1]){mu = s*nu2[1]/tau2[1]}
  if (s >= tau2[1] & s <= tau2[2]){mu = nu2[1]}
  if (s >= tau2[2] & s <= tau2[3]){mu = nu2[1] + (s-tau2[2])*(nu2[2]-nu2[1])/(tau2[3]-tau2[2])}
  if (s >= tau2[3]){mu = nu2[2]}
  return(mu)
}

x = c(0,1,0,0)
a=c("a", 1000)

transition_continue <- function(x, a) {
  #x = [m, zeta, u, w]
  m = x[1]
  zeta = x[2]
  u = x[3]
  w = x[4]
  
  #w=temps depuis début
  #a = [l,r]
  l = a[1]
  r = as.numeric(a[2])
  
  u_1 = u
  
  if (m == 0 & l == "null"){
    clock = 0
    while (clock < r) {
      #calcul de lambda_bar
      s = seq(from = u,
              to = u_1 + r,
              by = 0.1)
      lambda_bar = (s %>%
                                   lapply(mu_1) %>% 
                                   unlist() + s %>%
                                   lapply(mu_2) %>% 
                                   unlist()) %>% max()
      
      #rejet ou non
      S_bar = rexp(1, rate = lambda_bar)
      Xi = runif(1, min = 0, max = lambda_bar)
        if (Xi < mu_1(S_bar + u) + mu_2(S_bar + u) & S_bar+clock < r)
          #le saut est retenu u=0 et on va attendre jusqu'au prochain r
        {
          u <- r-(S_bar+clock)
          #choix de la destiation
          p = runif(1, min = 0, max = 1)
          if (p < mu_1(S_bar + u) / (mu_1(S_bar + u) + mu_2(S_bar + u)))
            #destination 1
          {
            m = 1
            t_star=(log(D/zeta))/v1not + clock + S_bar
            zeta = zeta*exp(v1not*(u))
          }else{
            #destiation 2
            m = 2
            t_star = (log(D/zeta))/v2not + clock + S_bar
            zeta = zeta*exp(v2not*(u))
          }
          #est-on mort ?
            if (zeta >= D)
            {#si c'est le cas on récupère u le temps passé depuis l'atteinte de la mort 
              u = r - t_star
              m = 3
              zeta = D
            }
          clock = r #stop while
        }else{
          clock = clock + S_bar
          u = u + S_bar}
          #le saut n'est pas retenu, on mettra à jour u pour le prochain tirage 
    }
    if (m==0)
      {
      #le saut est trop tard, il n'a pas lieu, u gagne r, sinon on lui rajoute S_bar 
      u = r + u_1
    }
    w = w + r
    x = c(m, zeta, u, w)
    return(x)
  }
  if (m == 0 & l == "a") {
    u_1 = u
    clock = 0
    while (clock < r) {
      #calcul de lambda_bar
      s = seq(from = u,
              to = u_1 + r,
              by = 0.1)
      lambda_bar = s %>%
                      lapply(mu_2) %>% 
                      unlist() %>% max()
      #rejet ou non
      S_bar = rexp(1, rate = lambda_bar)
      Xi = runif(1, min = 0, max = lambda_bar)
        if (Xi < mu_2(S_bar + u)& S_bar+clock < r)
          #le saut est retenu
        {
          u = r - (clock + S_bar)# a priori
          #ici pas de choix de la destiation
          m = 2
          t_star = (log(D/zeta))/v2not + clock + S_bar
          zeta = zeta*exp(v2not*u)
          #est-on mort ? mettons à jour
          if (zeta >= D)
            {
              m = 3
              u = r - t_star
              zeta = D
          }
          clock = r #stop while
          #le saut n'est pas retenu
        } else{
          clock = S_bar + clock
          u = u + S_bar
        }
    }
    if (m == 0)
      #le saut est trop tard, il n'a pas lieu, u gagne r
    {
      u = r + u_1
    }
    w = w + r
    x = c(m, zeta, u, w)
    return(x)
    }
  if (m == 0 & l == "b") {
    u_1=u
    clock = 0
    while (clock < r) {
      #calcul de lambda_bar
      s = seq(from = u,
              to = u_1 + r,
              by = 0.1)
      lambda_bar = s %>%
              lapply(mu_1) %>% 
              unlist() %>% max()
      
      #rejet ou non
      S_bar = rexp(1, rate = lambda_bar)
      Xi = runif(1, min = 0, max = lambda_bar)
      
        if (Xi < mu_1(S_bar + u) & S_bar+clock < r)
          #le saut est retenu
        {
          u = r-(clock + S_bar)
          #ici pas de choix de la destiation
          m = 1
          t_star = log(D / zeta)/V1 + clock + S_bar
          zeta = zeta*exp(V1 * u)
          #est-on mort ? si oui il faut mettre a jour u, zeta et m
            if (zeta >= D)
            {
              m = 3
              u = r - t_star
              zeta = D
            }
          clock = r #stop while
        }else{
          #le saut n'est pas retenu
          u = u + S_bar
          clock = clock + S_bar}}
          if (m==0)
            #le saut est trop tard, il n'a pas lieu, u gagne r 
          {u = r + u_1}
    
    w = w + r
    x = c(m, zeta, u, w)
    return(x)
    }
  if (m == 1 & l == "null") {
    #deterministe
    u = u + r
    t_star = (log(D / zeta)) / v1not
    zeta = zeta * exp(v1not * r)
    #est-on mort ?
    if (zeta >= D)
    {
      m = 3
      u = r - t_star
      zeta = D
    }
    w = w + r
    x = c(m, zeta, u, w)
    return(x)
  }
  if (m == 1 & l == "a") {
    ##stochastique
    clock = 0
    t_star_H = log(zeta/zeta_0)/vprime1 #H pour healed
    if (r>t_star_H){
      m=0
      }# a priori on va guérir
    while (clock < t_star_H & clock < r) {
      #calcul de lambda_bar
      s = seq(from = clock,
              to = min(t_star_H, r),
              by = 0.1)#comment gérer si on était déja dans ce cas, le s ne sert qu'à gérer le flot de zeta depuis la dernière visite
      lambda_bar = max(mu_prime_1_rond_Phi(zeta, s, m))
      
      #rejet ou non
      S_bar = rexp(1, rate = lambda_bar)
      Xi = runif(1, min = 0, max = lambda_bar)
      if (S_bar + clock > t_star_H | S_bar + clock > r)
        #le saut est d'office trop tard donc on suit le flot jusqu'à r
      {
        u = r + u*(m!=0) - t_star_H*(m==0) #dernier temps de saut est soit t* soit n'a pas eu lieu depuis la dernière visite
        zeta = zeta*exp(-vprime1*(r-clock))#on poursuit mais en gardant ce qui s'est passé avec les rejets 
        #si on n'a pas le temps de gérir on va juste baisser 
        clock = t_star_H #stop while
      }else{
        if (Xi < mu_prime_1_rond_Phi(zeta, S_bar+clock, m))
          #le saut est retenu
        {
          u = r - (clock + S_bar)
          #pas de choix de la destiation
          m = 2
          zeta = zeta*exp(-(S_bar)*vprime1)
          t_star_D =  log(D/zeta)/V2  #d pour death
          zeta = zeta*exp(u*V2)#on baisse puis on monte 
          clock = r #stop while dcp
          #est-on mort ?
          if (zeta >= D)
          {
            u = r - t_star_D - u
            m = 3
            zeta = D
          }
        } else{
          #le saut n'est pas retenu, maj de u inutile
          clock = clock + S_bar
          zeta = zeta*exp(-vprime1*S_bar) #on modifie zeta pour le prochain flot
        }
      }
    }#end while
    #on peut être à nouveau sain. Il faut relancer pour avoir une maladie
    if (m==0)
    {
      zeta = zeta_0
      #d'ou lintérêt de séparer clock et u
      u = 0
      clock = t_star_H
      while (clock < r) {
        #calcul de lambda_bar
        s = seq(from = u,
                to = u_1 + r,
                by = 0.1)
        lambda_bar = s %>%
                    lapply(mu_2) %>% 
                    unlist() %>% max()
        #rejet ou non
        S_bar = rexp(1, rate = lambda_bar)
        Xi = runif(1, min = 0, max = lambda_bar)
        t_star_D = (log(D / zeta))/V2 + clock + S_bar
        if (clock + S_bar + t_star_D > r)
          #le saut est d'office trop tard on garde le prec
        {
          clock = r #stop while
        } else{
          if (Xi < mu_2(S_bar + u))
            #le saut est retenu
          {
            u = r - (clock + S_bar)
            clock = r
            #ici pas de choix de la destiation
            m = 2
            zeta = zeta * exp(v2not * u)
            #est-on mort ?
              if (zeta >= D)
              {
                m = 3
                u = r - t_star_D
                zeta = D
              }
          }else{
            #le saut n'est pas retenu
            u = u + S_bar
            clock = clock + S_bar
          }
        }
      }
      if (m==0){
        u=r-t_star_H
        zeta = zeta_0}
    }#end rechute si sain
    w = w + r
    x = c(m, zeta, u, w)
    return(x)
  }
  if (m == 1 & l == "b") {
    u = u + r
    t_star = (log(D / zeta)) / V1
    zeta = zeta * exp(V1*r)
    #est-on mort ?
    if (zeta >= D)
    {
      m = 3
      u = r - t_star
      zeta = D
    }
    w = w + r
    x = c(m, zeta, u, w)
    return(x)
  }
  if (m == 2 & l == "null") {
    u = u + r
    t_star = log(D/zeta)/v2not
    zeta = zeta * exp(v2not*r)
    #est-on mort ?
    if (zeta >= D)
    {
      m = 3
      u = r - t_star
      zeta = D
    }
    w = w + r
    x = c(m, zeta, u, w)
    return(x)
  }
  if (m == 2 & l == "a") {
    u = u + r
    t_star = (log(D / zeta)) / V2
    zeta = zeta * exp(V2 * r)
    #est-on mort ?
    if (zeta >= D)
    {
      m = 3
      u = r - t_star
      zeta = D
    }
    w = w + r
    x = c(m, zeta, u, w)
    return(x)
  }
  if (m == 2 & l == "b") {
    ##stochastique
    clock = 0
    t_star_H = log(zeta/zeta_0)/vprime2 #H pour healed
    if (r>t_star_H){
      m=0}# a priori on va guérir
    while (clock < t_star_H & clock < r) {
      #calcul de lambda_bar
      s = seq(from = clock,
              to = min(t_star_H,r),
              by = 0.1)#comment gérer si on était déja dans ce cas, le s ne sert qu'à gérer le flot de zeta depuis la dernière visite
      lambda_bar = max(mu_prime_1_rond_Phi(zeta, s, m))
      
      #rejet ou non
      S_bar = rexp(1, rate = lambda_bar)
      Xi = runif(1, min = 0, max = lambda_bar)
      if (S_bar + clock > t_star_H | S_bar + clock > r)
        #le saut est d'office trop tard
      {
        u = r + u*(m!=0) - t_star_H*(m==0) #dernier temps de saut est soit t* soit celui d'avant la dernière visite
        zeta = zeta*exp(-vprime2*(r-clock))#on poursuit mais en gardant ce qui s'est passé avec les rejets 
        #dans le cas ou on aura le temps de guérir cela sera remplacé plus bas
        clock = t_star_H #stop while
      }else{
        if (Xi < mu_prime_1_rond_Phi(zeta, S_bar + clock, m))
          #le saut est retenu
        {
          u = r - (clock + S_bar)
          #pas de choix de la destiation
          m = 1
          zeta = zeta*exp(-(S_bar)*vprime2)
          t_star_D =  log(D/zeta)/V1  #d pour death
          zeta = zeta*exp(u*V1)
          clock = r #stop while dcp
          #est-on mort ?
          if (zeta >= D)
          {
            u = r - t_star_D - u 
            m = 3
            zeta = D
          }
        } else{
          #le saut n'est pas retenu (inutile de maj u)
          clock = clock + S_bar
          zeta = zeta*exp(-vprime2*S_bar)
        }
      }
    }#end while
    if (m==0) #on a eu l'occasion de guérir
    {
      zeta = zeta_0
      u = 0
      clock = t_star_H
      while (clock < r) {
        #calcul de lambda_bar
        s = seq(from = u,
                to = u_1 + r,
                by = 0.1)
        lambda_bar = s %>%
                  lapply(mu_1) %>% 
                  unlist() %>% max()
               
        #rejet ou non
        S_bar = rexp(1, rate = lambda_bar)
        Xi = runif(1, min = 0, max = lambda_bar)
        t_star_D = (log(D / zeta))/V2 + clock + S_bar
        if (clock + S_bar + t_star_D > r)
          #le saut est d'office trop tard on laisse le u précedent
        {
          clock = r
        } else{
          if (Xi < mu_1(S_bar + u))
            #le saut est retenu
          {
            u = r - (clock + S_bar)
            clock = r
            #ici pas de choix de la destiation
            m = 2
            zeta = zeta * exp(v2not * u)
            #est-on mort ?
            {
              if (zeta >= D)
              {
                m = 3
                u = r - t_star_D
                zeta = D
              }
            }
          } else{
            #le saut n'est pas retenu
            u = u + S_bar
            clock = clock + S_bar
          }
        }
      }
    
      if (m==0){
        u=r-t_star_H
        zeta = zeta_0}
      }
    
    w = w + r
    x = c(m, zeta, u, w)
    return(x)  
  }
  if (m == 3)
  {
    w = w + r
    u = u + r
    x = c(m, zeta, u, w)
    return(x) 
  }
}

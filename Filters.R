################ This function allows to sample particles from the conditional filter
sample <- function(filtre_cafe, w){
  indi <- which(rmultinom(1, 1, filtre_cafe)==1)
  zeta <- xval[indi]
  if (indi <= 81){m <- 0
  u <- delta*(indi-1)}
  if (indi > 81 & indi < 113){
    m <- 1
    u <- 10
    #u = time since last jump, has no influence over simulations when m=1 or 2
  }
  if (indi >= 113){
    m <- 2
    u <- 10
    #u = time since last jump, has no influence over simulations when m=1 or 2
  }
  return(c(m, zeta, u, w))
}

filtre_adaptator <- function(filtre, w)
  {
  f <- list()
  for (i in 1:K){
    f[[i]] <- sample(filtre, w)
  }
  return(f)
}


################# This function updates the particle filter. If filter cannot be updated with current filter value (all simulated particles rejected), then simulates two steps from the filter at previous time-point



Belief_update <- function(prev_belief, a, o, h, ante_belief)
{
  Belief <- list()
  i = 0
  count = 0
  while (length(Belief) < K & count < 100000) {
    count = count + 1
    s = prev_belief[[round(runif(1, 1, K))]]
    s_prime <- transition_continue(s, a)
    o_prime <- s_prime %>% observation()
    if (abs(o - o_prime) < eps)
    {
      i = i + 1
      Belief[[i]] = s_prime
    }
  }# stops here if the new filter is full. Otherwise, simulation from previous filter with two steps
  count = 0
  anam <- str_split(h, ";") %>% unlist()
  a_o <- str_split(anam[length(anam) - 1], ":") %>% unlist()
  if (!is.null(ante_belief)){
  while (length(Belief) < K & count < 100000) {
    count = count + 1
    s = ante_belief[[round(runif(1, 1, K))]]
    s_prime <- s %>%
      transition_continue(a_o[1:2]) 
    o_prime <- s_prime %>% observation()
    if (abs(as.numeric(a_o[3])-o_prime) < eps)
      {pass <- TRUE
      s_prime <- s_prime %>% transition_continue(a)
      o_prime <- s_prime %>% observation()
    }else{pass <- FALSE}
    if (abs(o - o_prime) < eps & pass == TRUE)
    {
      i = i + 1
      Belief[[i]] = s_prime
    }
  }}
  if (i==0){
    count = 0
    df <- data.frame()
    while (count < 10000)
    {
      count = count + 1
      s = prev_belief[[round(runif(1, 1, K))]]
      s_prime <- transition_continue(s, a)
      o_prime <- s_prime %>% observation()
      df[nrow(df)+1, 1:5] <- c(s_prime, abs(o-as.numeric(o_prime)))
      df <- df %>% arrange(V5)
      df <- df[1:min(c(nrow(df), K)),]
    }
    for (i in 1:K){Belief[[i]]<-c(df[i, 1],df[i, 4],df[i, 4],df[i, 4])}
  }
  if (i != K) {
    for (k in (i + 1):K) {
      Belief[[k]] <- Belief[[round(runif(1, 1, i), 0)]]
    }
  }
  return(Belief)
}  

	

observation <- function(state)
{
	obs = round((state[2] + rnorm(1, 0, 1))/ discret)*discret
	return(obs)
}



rollout <- function(s){
  cost = 0
  if (s[4]>=H){return(0)}
  
  dec <- politique_gold(s)
  
  s_prime <- transition_continue(s, dec)
  
  cost = cost_per_stage(dec, s_prime, s) + 
    rollout(s_prime)
  return(cost)}



simulate <- function(s, h, tree,c){
  if (s[4] >= H) {
    return(0)
  }
  #vérifier sur le delief a déja ete  visité
  if (!(h %in% tree$hist)){
    for (i in 1:nrow(decision)){
      d <- decision[i, ] %>%
        unlist() %>%
        str_c(collapse = ":")
      
      h_new <- str_c(c(h, d), collapse = ";")
      
      tree <- tree %>%
        rbind(c(h, d, h_new, 1, 0))#, "emptyset"))
      
      names(tree) <- c("hist", 
                       "a", 
                       "ha",
                       "N(ha)",
                       "V(ha)"
      )#,"B(h)")
    }
    return(list(rollout(s), tree))} 
  #c'est la fin de la boucle ! On a fini de visiter
  #l'arbre de façon e-greedy
  
  
  T_h <- filter(tree, hist == h)
  
  Nh <- T_h %>%
    select('N(ha)') %>%
    unlist() %>%
    as.numeric() %>%
    sum()
  
  a <- T_h[which.min(as.numeric(T_h$`V(ha)`) -
                       c*sqrt(log(Nh)/as.numeric(T_h$`N(ha)`))),] %>%
    select(a) %>%
    unlist() %>%
    strsplit(a, split = ":") %>%
    unlist()
  
  s_prime <- transition_continue(s, a)
  o <- observation(s_prime)
  
  ha <- h %>%
    c(str_c(a, collapse = ":")) %>%
    str_c(collapse = ";")
  
  hao <- ha %>%
    c(o) %>%
    str_c(collapse = ";")
  
  forward_search <- simulate(s_prime, hao, tree,c)
  
  if (length(forward_search)==1)
  {
    R <- cost_per_stage(a, s_prime, s) + forward_search
  }else{
    tree <- forward_search[[2]]
    R <- cost_per_stage(a, s_prime, s) + forward_search[[1]]}
  
  Nha <- tree[which(tree$ha == ha),4] %>% as.numeric()
  tree[which(tree$ha == ha),4] <- Nha + 1
  
  #maj de la fonction valeur
  Vha <- tree[which(tree$ha == ha), 5] %>% as.numeric()
  tree[which(tree$ha == ha), 5] <- Vha + (R - Vha)/Nha
  
  return(list(R, tree))
}

search <- function(filtre, w)
{
  tree <- data.frame()
  h <- "empty"
  #simu <- max_sim-pente*w^(dec_sim)
  
  for(i in 1:simu){
    #print(paste(round(100*(i/simu), 2), '%'))
    s <- filtre[[round(runif(1, 1, K))]]
    tree <- simulate(s, h, tree,c)[[2]]
  }
  
  root <- filter(tree, hist == h)
  
  a <- root[which.min(root$`V(ha)`), 2] %>%
    strsplit(split = ":") %>%
    unlist()
  return(a)
}

politique_gold <- function(state, prev_dec, prev_state)
{
  if (state[1]==0)
  {
    l="null"
    r=15
  }
  if (state[1]==1)
  {
    l="a"
    r=15
  }
  if (state[1]==2)
  {
    l="b"
    r=15
  }
  if (state[1]==3)
  {
    l="null"
    r=15
  }
  return(c(l,r))
}



dynamicc <-function(filtre)
{
	matfilt<-matrix(unlist(filtre),nrow=4)
	freq=as.data.frame(table(matfilt[1,])/ncol(matfilt))
	c=freq[,2]%*%log(freq[,2])/log(1/3)
	return(c)
}

rev_entropy <-function(filtre)
{
	c=1-dynamicc(filtre)
	return(c)
}

rev_entropy_2 <-function(filtre)
{

	c=1-dynamicc(filtre)/2
	return(c)
}

c99 <-function(filtre)
{
	return(0.99)
}

Nsim<-function(w)
{
	if (w<H/4) return(simmax)
	if (w<H/2) return(round(3*simmax/4))
	if (w<H*3/4) return(round(simmax/2))
	return(round(simmax/4))
}

NsimEnt<-function(filtre)
{
	return(nbas+dynamicc(filtre)*nent)
}

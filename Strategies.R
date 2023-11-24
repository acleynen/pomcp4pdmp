random_trajectory <- function(f_0, x_0)
{
	t = system.time({
  C = 0 #initialisation of the cost of the trajectory
  x = x_0
  LT=0
 	TreatTime=0
 	PFS=c()   
    
  while (x[4] < 2400 & x[1]!=3){
  	LT=LT+1
    m = x[1] #the mode of the patient
    l = sample(c("a","b","null"),1)  # randomly select a treatment
    r = sample(c(15,30,60),1) # randomly select a next visit date
    action=c(l,r)  
    if (l != "null") TreatTime=TreatTime+r
    
    x <- transition_continue(x, action) # simulates the next value of the process given current value and decision
    if (x[1]!=0) PFS=c(PFS,x[4]-x[3]) # computes progression-free survival if change of mode

    #Cost computation
    C = C + kappa*(x[2]-1)*r +
      Beta_a*(m==0 & l != "null")*r + c_v
  }
  C = C + (x[1]==3)*M
  if (x[1]==3) 
  {RealLength=x[4] # Survival of patient if he dies before horizon
  z=1} else 
  {RealLength=2400 # Else censored survival is horizon
  z=0}  
  print(c(C,TreatTime,LT,RealLength,z,PFS[1]))
  })
  return(c(C,TreatTime,LT,RealLength,z,PFS[1]))
}  



pomcp_trajectory <- function(policy, update_filter, f_0, x_0)
{
	t = system.time({
  C = 0 #initialisation of the cost of the trajectory
  x = x_0
  filter <- list(f_0)
  h <- vector()  
  ctradeoff<-c()
  LT=0
 	TreatTime=0
 	PFS=c()   
    
  while (x[4] < 2400 & x[1]!=3){
  	LT=LT+1
    m = x[1] #the mode of the patient

    Polc <- policy(filter[[length(filter)]],x[4])
    action <- Polc[1:2]
    ctradeoff<-c(ctradeoff,Polc[3])
     
    l = action[1] # treatment part of the action
    r = action[2] %>% as.numeric() # next visit date part of the action
    if (l != "null") TreatTime=TreatTime+r # if we treat, update time spent under treatment
    
    x <- transition_continue(x, action) # simulates the next value of the process given current value and decision
    if (x[1]!=0) PFS=c(PFS,x[4]-x[3])

    
    #Cost computation
    C = C + kappa*(x[2]-1)*r +
      Beta_a*(m==0 & l != "null")*r + c_v
    
    o = x[2] + rnorm(1, 0, 1)
    
    h <- str_c(c(h, str_c(c(action, round(o/discret)*discret), collapse = ":" )), collapse = ";")
    if (length(filter)-1>0)
    {
      ante_belief <- filter[[length(filter)-1]]
    } else {ante_belief <- NULL}
    
    filter <- append(filter, 
                     update_filter(filter[[length(filter)]], 
                                action, 
                                o, 
                                h, 
                                ante_belief) %>%
                       list()
                     )
    
    filter <- list(filter[[length(filter)-1]] , filter[[length(filter)]])
    
  }
  C = C + (x[1]==3)*M
  if (x[1]==3) 
  {RealLength=x[4]
  z=1} else 
  {RealLength=2400
  z=0}  
  print(c(C,TreatTime,LT,RealLength,z,PFS[1]))
  })
  return(c(C,TreatTime,LT,RealLength,z,PFS[1]))
}  



DP_trajectory <- function(policy, update_filter, f_0, x_0)
{
	t = system.time({
  C = 0 #initialisation of the cost of the trajectory
  x = x_0
  filter <- f_0
  ctradeoff<-c()
  LT=0
 	TreatTime=0
 	PFS=c()
    
  while (x[4] < 2400 & x[1]!=3){
  	LT=LT+1
    m = x[1] #the mode of the patient

    belief <- filter_adaptator(filter, x[4])
    Polc<-policy(belief,x[4])
    action <- Polc[1:2]
    ctradeoff<-c(ctradeoff,Polc[3])
     
    l = action[1] # treatment part of the action
    r = action[2] %>% as.numeric() # next visit date part of the action
    if (l != "null") TreatTime=TreatTime+r # if we treat, update time spent under treatment
    
    x <- transition_continue(x, action) # simulates the next value of the process given current value and decision
    if (x[1]!=0) PFS=c(PFS,x[4]-x[3])
    
    #Cost computation
    C = C + kappa*(x[2]-1)*r +
      Beta_a*(m==0 & l != "null")*r + c_v
    
     o = x[2] + rnorm(1, 0, 1)
    
    filter <- update_filter(filter, action, o)

  }
  if (x[1]==3) 
  {RealLength=x[4]
  z=1} else 
  {RealLength=2400
  z=0}
  
  C = C + (x[1]==3)*M
  print(c(C,TreatTime,LT,RealLength,z,PFS[1]))
  })
  return(c(C,TreatTime,LT,RealLength,z,PFS[1]))
}  




H=2400 # Horizon in days

## Parameters of jump time distribution

tau1 = c(750, 5*365, 6*365)
tau2 = c(500, 5*365, 6*365)
nu1 = c(-2*log(0.8)/750, -2*((-2*log(0.8)/(2*750))*(6*365+5*365-750)+log(0.1))/(2*H-365*5-6*365)) 
nu2 = c(-2*log(0.8)/500, -2*((-2*log(0.8)/(2*500))*(6*365+5*365-500)+log(0.1))/(2*H-365*5-6*365))
D=40 # Death frontier 
zeta_0=1 # process value in mode 0
## Careful, in mode 1 use  mu'2 !!! mu'2(s)=(b2*s)^beta2
beta1=-0.8 # shape parameter for mu'1
beta2=-0.8# 

b1=1000 # scale parameter for mu'1 
b2=1000 # 
b=c(b1,b2)
beta=c(beta1,beta2)

v1not=0.02 # slope for uncontrolled disease 1 
v2not=0.006 # 
v1=0.01 # slope for disease 1 with treatment b
V1=v1
v2=0.003 # slope for disease 2 with treatment a
V2=v2
v=c(v1not,v1,v2not,v2)
vprime1=0.077 # slope for disease 1 with treatment a
vprime2=0.025 # slope for disease 2 with treatment b
vprime=c(vprime1,vprime2)
sigma2=1


decision = data.frame()

i = 0
Possible_Visits = c(15, 30, 60)

for (l in c("a", "b", "null")) {
  for (r in Possible_Visits) {
    i = i + 1
    decision[i, 1:2] = c(l, r)
  }
}




c_v = 1 #  visit cost
cost_per_stage <- function(prev_decision, new_state, prev_state)
{
  if (new_state[1]!=3)#si on est pas mort
  {
  c = c_v + (new_state[2]-zeta_0)*as.numeric(prev_decision[2])*k + 
    iatro*(prev_state[1]==0 & prev_decision[1] != "null")*as.numeric(prev_decision[2])
  }
  if (new_state[1]==3 & new_state[4]!=2400){c = 0}#if patient dies before end of horizon, do not count
  if (new_state[1]==3 & new_state[4]==2400){c = M}#if patient has died at any time, and it is the end of the horizon, then count death cost
  return(c)
}


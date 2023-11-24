# pomcp4pdmp
Implementation of POMCP algorithm adapted for PDMP models

This repository contains all code necessary to reproduce the simulation study presented in the manuscript ...

## PDMP parameters

All PDMP parameters (flow, intensity, noise), MPD parameters (horizon, decisions), and Cost parameters (visit cost, death cost, etc) are encoded in file R/Parameters.R
The PDMP flows are defined by the slope of the exponential behavior of the marker. They are given by
 - v1not=0.02 # slope for uncontrolled disease 1 
 - v2not=0.006 # slope for uncontrolled disease 2 
 - v1=0.01 # slope for disease 1 with treatment b
 - v2=0.003 # slope for disease 2 with treatment a
 - vprime1=0.077 # slope for disease 1 with treatment a
 - vprime2=0.025 # slope for disease 2 with treatment b
The intensity, characterized by the risk function, depends on the state of the patient and is separated by disease and treatment. For the "standard relapse" (from remission to disease state i (in 1,2)), the risk function is defined by Figure ? of the manuscript and has parameters
 - tau1 = c(750, 5*365, 6*365) # (early relapse average, late relapse average, non-relapsing patients) in disease 1
 - tau2 = c(500, 5*365, 6*365) # (early relapse average, late relapse average, non-relapsing patients) in disease 2
 - nu1 = c(-2*log(0.8)/750, -2*((-2*log(0.8)/(2*750))*(6*365+5*365-750)+log(0.1))/(2*H-365*5-6*365)) # proportion of patient relapsing before tau1 = 20%, proportion of patients not relapsing = 10%
 - nu2 = c(-2*log(0.8)/500, -2*((-2*log(0.8)/(2*500))*(6*365+5*365-500)+log(0.1))/(2*H-365*5-6*365)) # proportion of patient relapsing before tau2 = 20%, proportion of patients not relapsing = 10%
For the therapeutic escape (from one disease under treatment to the other disease), the risk corresponds to a Weibull distribution with parameters 
 - beta2=-0.8 # shape parameter
 - b1=1000 # scale parameter 
The noise of the observations is a centered Gaussian distribution with variance 1
The Horizon is set to 2400 days, decisions include all combinations of treatments (a, b, none) and visit dates (15,30,60).
The marker dependent cost is defined as  c(x,d,x')=cv + kappa|zeta'-zeta0|* r +beta*r  if process is in mode 0 with
 - cv=1
 - kappa=1/6
 - beta=0.1 
And the death cost is set to 110

All these parameters are easily tunable by the user.


## Global implementation

File R/Strategies.R implements three different policy choices and cost estimation. The random_trajectory function implements the random policy which randomly selects treatment and visit time. The pomcp_trajectory function calls the pomcp algorithm to simulate trajectories and obtain the optimal current decision. It then calls the update_filter function to update the belief on the new state from simulation of accepted particles. Finally, the DP_trajectory function choses the optimal decision from the pomcp algorithm but updates the filter with the recurrence formula describe in paper "Numerical Method to solve impulse control problems for partially observed piece-wide deterministic Markov Processes" (https://arxiv.org/abs/2112.09408).

File R/POMCP_to_policy implements the original POMCP algorithm with options to take advantage of the PDMP model, for instance a "gold" policy for the rollout function, choices to fit the exploration / exploitation tradoff, or to adapt the number of simulations performed in the difficult cases.

Finally R/PDMP_Simulator implements our specific PDMP model, allowing to simulate a next observation from a current state and a current decision, while R/Functions_GridsInit.R allows to compute the recurrence update of the conditional filter given a current filter and a next observation.
 
## Simulations in the paper

All simulations can be performed by running commands such as the following:

Rscript scripts/Run_Trajectories.R 500 1000 0.5 0.1 Random

where the first argument is the number of particles used to compute the optimal policy from the POMCP algorithm, the second argument is the number of simulations to perform to explore the tree, the third argument is the tradeoff between exploring and exploiting the tree, the fourth argument is the grid size to produce nodes within the tree, and finally the last argument chooses the strategy to use for updating the filter and taking decisions. 

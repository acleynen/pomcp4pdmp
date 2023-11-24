library(glue)
library(stringr)
library(dplyr)
library(parallel)



source("Parameters.R")
source("PDMP_Simulator.R")
source("Functions_GridsInit.R")
source("POMCP_to_policy.R")
source("Filters.R")
source("Strategies.R")

load("ConditionalFilter_Object.RData")



args <- commandArgs(trailingOnly = TRUE)
K=as.numeric(args[1])
simu=as.numeric(args[2])
c=as.numeric(args[3])
discret=as.numeric(args[4])
FilterType=args[5]

print(paste("Number of search n:",simu))
print(paste("Number of particules K:",K))
print(paste("Tradeoff c:",c))
print(paste("Discretization d:",discret))



#parameter values for cost
kappa = 1/6
Beta_a = 1.5/15
Beta_b = Beta_a
c_v = 1
M=110
iatro = Beta_a
k=kappa

# initialisation
x_0 = c(0, 1, 0, 0)


if (FilterType=="Conditional")
{
	f_0=c(1,rep(0,nOmega-2))
	FilterFunction=Update_filter
	para <- function(s)
	{
		return(DP_trajectory(search, FilterFunction, f_0,x_0))
	}
		
}
if (FilterType=="Particles")
{
	f_0 <- rep(list(x_0), K)
	FilterFunction=Belief_update	
	para <- function(s)
	{
		return(pomcp_trajectory(search, FilterFunction, f_0,x_0))
	}
		
}	
	
if (FilterType=="Random")
{
	f_0 <- rep(list(x_0), K)
	para <- function(s)
	{
		return(random_trajectory(f_0,x_0))
	}

}



CC <- mclapply(1:500, FUN = para, mc.cores = 28)


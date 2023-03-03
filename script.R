# load data
library(readxl)
library(lubridate)
require(dplyr)
data_damself <- read_excel('data.xlsx',trim_ws = TRUE)  %>%
  mutate(variable_code = ifelse (variable == "bites", 0,1))

# transition matrix
require(reshape)
wide_df <- cast (formula = individual ~ sequence_obs,
      value="variable_code",
      fill = NA,
      fun.aggregate = sum,  
  data = data_damself)


# designing the model to test transitions/dynamics

sink("Model_dyn.txt")
cat("
    model {
    
    ############ Priors
    
    for (i in 1:nind){
      gamma[i]~dunif (0,1) # parameter of state transition
      phi[i]~dunif (0,1) # parameter of state persistence
      }
    
    ############ Model ########################
    for (i in 1:nind){
    
        for (t in 2:n_seq){
    
            ### dynamic model
            ### defining occupancy probability  ###
            muZ[i,t] <- y[i,t-1] * (phi[i]) + ### if bite, p of persist biting
                        (1-y[i,t-1]) * gamma[i] ###  if chase, p of bite
                        
            y[i,t] ~ dbern(muZ[i,t])
    
        }#t
    }#i
    
    # derived parameters
    av_gamma <- mean(gamma)
    av_phi <- mean(phi)
    
    }## end of the model
    
    ",fill = TRUE)
sink()

## bundle data

str(jags.data <- list(y = wide_df[,-1], 
                      nind = nrow(wide_df), 
                      n_seq= ncol(wide_df[,-1])))

# Set initial values
zst <- apply(wide_df[,-1], 1, max, na.rm = TRUE)	# Observed occurrence as inits for z
zst[zst == '-Inf'] <- 1 # max of c(NA,NA,NA) with na.rm = TRUE returns -Inf, change to 1
inits <- function(){ list(y = zst)}


## Parameters to monitor
#########################

## long form
params <- c(
  ## parameters
  "gamma","phi","av_gamma","av_phi")

## MCMC settings
######################
## short form
 na <- 1000; nb <- 2000; ni <- 4000; nc <- 3; nt <- 1
require(jagsUI)
require(coda)
samples_MCMC  <- jags(data = jags.data,
                  parameters.to.save=params, 
                  model = "Model_dyn.txt", 
                  inits = NULL,
                  n.chains = nc, 
                  n.thin = nt, 
                  n.iter = ni, 
                  n.burnin = nb)


# data to plot
df_plot <- data.frame (
  samples_MCMC$summary[(grep ("av",rownames(samples_MCMC$summary))),],
  State = c("Bite->chase", "Bite->bite")
)

# plot
require(ggplot2)
dodge <- c(0.4,0.4)
pd <- position_dodge(dodge)
pdf_pt <- position_dodge(dodge)

a <- ggplot (df_plot, 
             
             aes  (y=mean, x=State, 
                   colour=State, fill=State)) +
  
  geom_errorbar(aes(ymin=X2.5.,ymax=X97.5.),width = 0.2,size=1,
                position=pd)  + 
  
  theme_classic() + 
  
  geom_hline(yintercept=0.5, color="gray10", size=1,alpha=0.4)+
  
  geom_point(position=(pdf_pt), 
             size=3.5)+ 
  
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "gray50", size=0.5)+
  
  scale_color_manual(values=c("#f2a154", "#0e49b5")) + 
  
  xlab("State") + 
  
  ylab ("Probability of transition") + 
  
  #xlim(-0.5,0.5) +
  
  theme(axis.text.x = element_text(angle = 0,size=10),
        legend.position = "none")

a




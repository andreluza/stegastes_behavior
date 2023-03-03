# . . . . . .  . . . . .  . . . . .  . . .
#       Stegastes fuscus behavior        #
# . . . . . .  . . . . .  . . . . .  . . .


# load packages
library(readxl)
library(lubridate)
require(dplyr)
library(tidyr)
require(jagsUI)
require(coda)


# load data
data_damself <- read_excel('data.xlsx',trim_ws = TRUE)  
data_damself <- data_damself %>% 
  filter (count >0) %>%
  uncount(count) %>%
  group_by(variable) %>%
  mutate(behavior =  +(row_number() <= first(variable))) %>%
  ungroup

# each behavior
data_damself<-data_damself%>% 
  group_by(individual) %>%
  #arrange(variable)%>%
  mutate (Count = 1:n(),
          code_beh_bite = recode (variable, 
                             "bites" = 1,
                             "interespecific_chase"=0),
          code_beh_chase = recode (variable, 
                                  "bites" = 0,
                                  "interespecific_chase"=1)
          )

# transition matrix
require(reshape)
# bites
wide_df_bites <- cast (formula = individual ~ Count,
                       value="code_beh_bite",
                       fill = NA,
                       fun.aggregate = sum,  
                       data = data_damself)
# chase
wide_df_chase <- cast (formula = individual ~ Count,
                       value="code_beh_chase",
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

str(jags.data <- list(y = wide_df_bites[,-1], 
                      nind = nrow(wide_df_bites), 
                      n_seq= ncol(wide_df_bites[,-1])))

# Set initial values
zst <- apply(wide_df[,-1], 1, max, na.rm = TRUE)	# Observed occurrence as inits for z
zst[zst == '-Inf'] <- 1 # max of c(NA,NA,NA) with na.rm = TRUE returns -Inf, change to 1
inits <- function(){ list(y = zst)}


## MCMC settings
na <- 1000; nb <- 2000; ni <- 4000; nc <- 3; nt <- 1

## Parameters to monitor
params <- c("gamma","phi","av_gamma","av_phi")

# apply test for the two datasets
mcmc_res <- lapply (list (wide_df_bites,
              wide_df_chase), function (i) {

    ## bundle data
    str(jags.data <- list(y = i[,-1], 
                          nind = nrow(i), 
                          n_seq= ncol(i[,-1])))
    
    # run MCMC
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
      State = c("Shift", "Keep")
    )

df_plot
})
names(mcmc_res) <- c("Bite", "Chase")
mcmc_res<- do.call(rbind, mcmc_res)
mcmc_res$Behavior <- sapply (strsplit (rownames(mcmc_res),"\\."), "[[",1)

# plot
require(ggplot2)
dodge <- c(0.4,0.4)
pd <- position_dodge(dodge)
pdf_pt <- position_dodge(dodge)
# plot
a <- ggplot (mcmc_res, 
             
             aes  (y=mean, x=Behavior, 
                   colour=State, fill=State)) +
  
  geom_errorbar(aes(ymin=X2.5.,ymax=X97.5.),width = 0.2,size=1,
                position=pd)  + 
  
  theme_classic() + 
  
  geom_hline(yintercept=0.5, linetype="dashed", 
             color = "gray50", size=0.5)+
  
  geom_point(position=(pdf_pt), 
             size=3.5)+ 
  
  scale_color_manual(values=c("#f2a154", "#0e49b5")) + 
  
  xlab("State") + 
  
  ylab ("Probability of transition") + 
  
  #xlim(-0.5,0.5) +
  
  theme(axis.text = element_text(angle = 0,size=12),
        axis.title = element_text(angle = 0,size=14),
        legend.position = c(0.8,0.9))

a

# forraging vs percution rate



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


# load data Andre
data_damself <- read_excel('data.xlsx',trim_ws = TRUE)  

# load data Renato
data_damself_renato <- read_excel('Dados_Stegastes_renato.xlsx',trim_ws = TRUE)  %>%
  mutate (sequence_obs = as.numeric(sequence_obs)) %>%
  mutate (individual = max(data_damself$individual)+individual)

# bind
data_damself<- rbind (data_damself,
                      data_damself_renato)

# arrange data
data_damself_shifts <- data_damself %>% 
  filter (sequence_obs != "NA") %>%
  filter (count >0) %>%
  uncount(count) %>%
  group_by(variable) %>%
  mutate(behavior =  +(row_number() <= first(variable))) %>%
  ungroup

# each behavior
data_damself_shifts<-data_damself_shifts%>% 
  group_by(individual) %>%
  #arrange(variable)%>%
  mutate (Count = 1:n(),
          code_beh_bite = recode (variable, 
                             "bites" = 1,
                             "interspecific_chase"=0,
                             "intraspecific_chase"=0),
          code_beh_chase = recode (variable, 
                                  "bites" = 0,
                                  "interspecific_chase"=1,
                                  "intraspecific_chase" =1)
          )



# transition matrix
require(reshape)
# bites
wide_df_bites <- cast (formula = individual ~ Count,
                       value="code_beh_bite",
                       fill = NA,
                       fun.aggregate = sum,  
                       data = data_damself_shifts)
# chase
wide_df_chase <- cast (formula = individual ~ Count,
                       value="code_beh_chase",
                       fill = NA,
                       fun.aggregate = sum,  
                       data = data_damself_shifts)



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
                        (1-y[i,t-1]) * gamma[i]  ###  if chase, p of shifting to bite
                        
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
             
             aes  (y=mean, x=State, 
                   fill=Behavior),colour="black") +
  
  geom_errorbar(aes(ymin=X2.5.,ymax=X97.5.),width = 0.2,size=1,
                position=pd)  + 
  

  theme_classic() + 
  
  geom_hline(yintercept=0.5, linetype="dashed", 
             color = "gray50", size=0.5)+
  
  geom_vline(xintercept=1.5, linetype="dashed", 
             color = "gray50", size=0.5)+
  
  geom_point(position=(pdf_pt), 
             size=3.5)+ 
  
  xlab("State") + 
  
  ylab ("Probability of transition") + 
  
  #xlim(-0.5,0.5) +
  
  theme(axis.text = element_text(angle = 0,size=12),
        axis.title = element_text(angle = 0,size=14),
        legend.position = "none") + 
  
  annotate(geom="text",x=2,
           y=0.62,
           label="Chase->Bite",
           color="black")+
  annotate(geom="text",x=2.1,
           y=0.37,
           label="Bite->Chase",
           color="black")+
annotate(geom="text",x=0.87,
         y=0.76,
         label="Bite",
         color="black")+
  annotate(geom="text",x=1.1,
           y=0.57,
           label="Chase",
           color="black")


a

# forraging vs percution rate
data_damself %>% 
  group_by (age, variable) %>%
  summarise(ct = sum (count))
  

test<-data_damself %>% 
  mutate (coded_behavior = recode (variable, 
                         "bites" = 1,
                         "interspecific_chase"=0,
                         "intraspecific_chase"=0)) %>%
  group_by (individual, coded_behavior) %>%
  summarise(ct = sum (count)) %>%
  cast(individual~coded_behavior, value="ct")

colnames(test) <- c("individual", "chase", "bit")

# persecution vs forraging
ggplot (test %>%
          filter (chase != "NA"), 
        aes (x=bit, y=chase))+
  geom_point ()+
  geom_smooth(method = "lm")


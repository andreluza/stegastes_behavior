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
require(here)


# load data Andre
data_damself <- read_excel(here ("Data",'STEFUS_data_ALLuza.xlsx'),trim_ws = TRUE)  

# load data Renato
data_damself_renato <- read_excel(here ("Data",'STEFUS_data_RJunqueira.xlsx'),trim_ws = TRUE)  %>%
  mutate (sequence_obs = as.numeric(sequence_obs)) %>%
  mutate (individual = max(data_damself$individual)+individual)

# load data Thomas
data_damself_thomas <- read_excel(here ("Data",'STEFUS_data_TBanha.xlsx'),trim_ws = TRUE)  %>%
  mutate (sequence_obs = as.numeric(sequence_obs)) %>%
  mutate (individual = max(data_damself_renato$individual)+individual)

# load data Carol
data_damself_carol <- read_excel(here ("Data",'STEFUS_data_TBanha.xlsx'),trim_ws = TRUE)  %>%
  mutate (sequence_obs = as.numeric(sequence_obs)) %>%
  mutate (individual = max(data_damself_thomas$individual)+individual)

# bind
data_damself<- rbind (data_damself,
                      data_damself_renato,
                      data_damself_thomas)


# arrange data for state-space model
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
                             "intraspecific_chase"=0,
                             "both_chase"=0),
          code_beh_chase = recode (variable, 
                                  "bites" = 0,
                                  "interspecific_chase"=1,
                                  "intraspecific_chase"=1,
                                  "both_chase"=1)
          )



# transition matrix
require(reshape)
# bites
wide_df_bites <- cast (formula = site+individual ~ Count,
                       value="code_beh_bite",
                       fill = NA,
                       fun.aggregate = sum,  
                       data = data_damself_shifts)
# chase
wide_df_chase <- cast (formula = site+individual ~ Count,
                       value="code_beh_chase",
                       fill = NA,
                       fun.aggregate = sum,  
                       data = data_damself_shifts)



# designing the model to test transitions/dynamics
sink("Model_dyn.txt")
cat("
    model {
    
    ############ Priors
    
    for (i in 1:nsite){
      gamma[i]~dunif (0,1) # parameter of state transition
      phi[i]~dunif (0,1) # parameter of state persistence
      }
    
    ############ Model ########################
    for (i in 1:nind){
    
        for (t in 2:n_seq){
    
            ### dynamic model
            ### defining occupancy probability  ###
            muZ[i,t] <- y[i,t-1] * (phi[site[i]]) + ### if bite, p of persist biting
                        (1-y[i,t-1]) * gamma[site[i]]  ###  if chase, p of shifting to bite
                        
            y[i,t] ~ dbern(muZ[i,t])
    
        }#t
    }#i
    
    # derived parameters
    av_gamma <- mean(gamma)
    av_phi <- mean(phi)
    
    
    }## end of the model
    
    ",fill = TRUE)
sink()

## MCMC settings
na <- 1000; nb <- 2000; ni <- 4000; nc <- 3; nt <- 1

## Parameters to monitor
params <- c("gamma","phi","av_gamma","av_phi")

# apply test for the two datasets
mcmc_res <- lapply (list (wide_df_bites,
              wide_df_chase), function (i) {

    ## bundle data
    str(jags.data <- list(y = i[,-c(1,2)], 
                          nind = nrow(i), 
                          n_seq= ncol(i[,-c(1,2)]),
                          site=as.numeric(as.factor (i$site)),
                          nsite=length(unique(as.numeric(as.factor (i$site))))))
    
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
    
    # results per site
    df_plot_site <- data.frame (
      samples_MCMC$summary[1:6,],
      State = c(rep("Shift",3),rep ("Keep",3)),
      Site = c("Bare", "Cabe", "Seg","Bare", "Cabe", "Seg")
    )
    
    
res <- list (df_plot=df_plot,
             df_plot_site=df_plot_site)
res

})

mcmc_res_site <- sapply (mcmc_res, "[", "df_plot_site")
names(mcmc_res_site) <- c("Bite", "Chase")
mcmc_res_site <- do.call(rbind, mcmc_res_site)
mcmc_res_site$Behavior <- sapply (strsplit (rownames(mcmc_res_site),"\\."), "[[",1)

# plot
require(ggplot2)
dodge <- c(0.4,0.4)
pd <- position_dodge(dodge)
pdf_pt <- position_dodge(dodge)
# plot
a <- ggplot (mcmc_res_site, 
             
             aes  (y=mean, x=State, 
                   fill=Behavior,colour=Site)) +
  facet_grid(~Site)+
  
  geom_errorbar(aes(ymin=X2.5.,ymax=X97.5.),width = 0.2,size=1,
                position=pd)  + 
  
  scale_colour_viridis_d(option = "magma",begin = 0.1,end=0.7)+

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

pdf (here ("output", "rates.pdf"))
a
dev.off()

# averages

mcmc_res_av <- sapply (mcmc_res, "[", "df_plot")
names(mcmc_res_av) <- c("Bite", "Chase")
mcmc_res_av <- do.call(rbind, mcmc_res_av)
mcmc_res_av$Behavior <- sapply (strsplit (rownames(mcmc_res_av),"\\."), "[[",1)
mcmc_res_av[,c(1,3,7)]

# ------------------------------

# number of individuals
length(unique(data_damself$individual))

# time
length(unique(data_damself$individual)) * 3 / 60

# average rates
data_damself %>% 
  filter (variable != "NA") %>%
  mutate (coded_behavior = recode (variable, 
                                   "bites" = 1,
                                   "interspecific_chase"=0,
                                   "intraspecific_chase"=0,
                                   "both_chase"=0)) %>%
  group_by (site,age) %>%
  summarise(av = mean(count),
            sd=sd(count))


# average rates
data_damself %>% 
  filter (variable != "NA") %>%
  mutate (coded_behavior = recode (variable, 
                                   "bites" = 1,
                                   "interspecific_chase"=0,
                                   "intraspecific_chase"=0,
                                   "both_chase"=0)) %>%
  group_by (site,age) %>%
  summarise(av = mean(count),
            sd=sd(count))

# boxplots
data_damself %>% 
  filter (variable != "NA") %>%
  filter (variable != "both_chase") %>%
  mutate (coded_behavior = recode (variable, 
                                   "bites" = 1,
                                   "interspecific_chase"=0,
                                   "intraspecific_chase"=0,
                                   "both_chase"=0)) %>%
  group_by (site,individual,variable,age) %>%
  summarize(sum_count = sum (count)) %>%
  #group_by (site,variable,age) %>%
  #summarise(av = mean (sum_count),
  #          sd=sd(sum_count))
  ggplot (aes (x=variable,y=sum_count, fill=age))+
  geom_boxplot()+
  scale_fill_viridis_d(option="magma",end=0.5)+
  theme_bw()+
  facet_wrap(~site,nrow=3)+
  xlab ("Behavior")+
  ylab ("Count")+
  theme(axis.text.x=element_text(angle=25,vjust=0.5),
        legend.position = c(0.8,0.9))


# forraging vs percution rate
test_rel<- data_damself %>% 
  filter (variable != "NA") %>%
  mutate (coded_behavior = recode (variable, 
                         "bites" = 1,
                         "interspecific_chase"=0,
                         "intraspecific_chase"=0,
                         "both_chase"=0)) %>%
  group_by (individual, coded_behavior,age) %>%
  summarise(ct = sum (count)) %>%
  cast(individual+age~coded_behavior, 
       value="ct",
       function.aggregate = sum)

colnames(test_rel) <- c("individual", "age","chase", "bit")
test_rel$chase[is.na(test_rel$chase)] <- 0

# persecution vs forraging
ggplot (test_rel %>%
          filter (chase != "NA"), 
        aes (y=bit, x=chase,group=age,colour=age))+
  scale_colour_viridis_d(option="magma",end=0.5)+
  geom_point ()+
  geom_smooth(method = "lm") + 
  xlab ("Count of chases") + 
  ylab ("Count of bites")+
  theme_bw() +
  theme (axis.title = element_text(size=14),
         axis.text = element_text(size=12),
         legend.position = c(0.9,0.9)) + 
  annotate(geom="text",
           x=16,
           y=11,
           label="rho=-0.18,ns",
           size=5,
           color="#B3005E")+
  annotate(geom="text",
           x=10,
           y=13,
           label="rho=0.08,ns",
           size=5,
           color="black")

# age
cor.test (test_rel$chase,test_rel$bit) #overall relationship
cor.test (test_rel$chase[which(test_rel$age == "adult")],
          test_rel$bit[which(test_rel$age == "adult")])
cor.test (test_rel$chase[which(test_rel$age == "intermediary")],
          test_rel$bit[which(test_rel$age == "intermediary")])


# chased spp

table(gsub (" ","", gsub('[0-9]+', '', unlist(strsplit (data_damself$species_chased, ",")))))

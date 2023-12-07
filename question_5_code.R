#Code to transform the data and fit linear model
#Install required packages
install.packages("janitor","dplyr", "ggplot2")
library(janitor)
library(dplyr)
library(ggplot2)

#Clean column names
virus_data <- clean_names(Cui_etal2014)

#Check this worked
head(virus_data)

#Create a new data subset with log transformed data
log_virus_data <- virus_data %>%
  mutate(log_V = log(virion_volume_nm_nm_nm)) %>%
  mutate(log_L = log(genome_length_kb))


#Fit the model
model1 <- lm(log_V ~ log_L, log_virus_data)

#View summary of the model
summary(model1)

##Code to reproduce plot
log_virus_data %>%
  ggplot(aes(x=log_L, 
             y=log_V))+
  geom_point()+
  geom_smooth(method="lm", linewidth=0.8)+
  labs(x="log[Genome length (kb)]", 
       y ="log[Virion volume(nm3)]")+
  theme(axis.title = element_text(face="bold"))+
  theme_bw()
library(NHANES)
library(dplyr)
library(ggplot2)
library(tidyr)


DF <- NHANES


# histogram ---------------------------------------------------------------


DF %>% #Ctrl+shift+M
  ggplot(aes(BMI))+
  geom_histogram(aes(y=..density..),fill="red3", color="white" )+
  geom_density()+
  theme_light()



DF %>% #Ctrl+shift+M
  select(BMI, Diabetes) %>% 
  na.omit() %>% 
  ggplot(aes(x=BMI, fill=Diabetes))+
  geom_histogram(color="white")+
  facet_wrap(~Diabetes, scales = "free")

DF %>% #Ctrl+shift+M
  select(BMI, Diabetes) %>% 
  na.omit() %>% 
  ggplot(aes(x=BMI, fill=Diabetes))+
  geom_density(alpha=0.5)+
  geom_vline(xintercept = 26.2, linetype="dashed")+
  geom_vline(xintercept = 32.6,linetype="dashed")

DF %>% #Ctrl+shift+M
  select(BMI, Diabetes) %>% 
  na.omit() %>% 
  mutate(BMI = log(BMI)) %>% 
  ggplot(aes(x=BMI, fill=Diabetes))+
  geom_density(alpha=0.5)


DF %>% #Ctrl+shift+M
  select(BMI, Diabetes) %>% 
  na.omit() %>% 
  group_by(Diabetes) %>% 
  summarise(
    conditional_mean = mean(BMI),
    conditional_median = median(BMI),
            )



model1 <- lm(BMI ~ Diabetes, data=DF)
summary(model1)




library(tidyverse)
library(gridExtra)
library(ggforce)
library(corrplot)

### Modelling wages
# Data: ATLAS INE 2019 for Barcelona
# May generate negative, but we censor it to 0. Eventually this replicates very well
## the actual wage distribuion.
wage_sim <- rnorm(n = 10000, mean = 21668, sd = 10000)
dfbcn <- data.frame(wage = wage_sim,
           u1 = rep(0,10000),
           u2 = rep(0,10000),
           u3 = rep(0,10000),
           u4 = rep(0,10000),
           u5 = rep(0,10000),
           u6 = rep(0,10000),
           u7 = rep(0,10000),
           u8 = rep(0,10000))
# monthly
dfbcn$wage <- ifelse(dfbcn$wage < 0, 0, dfbcn$wage/14)
summary(dfbcn$wage)


# With separated ordered index of security & amenities.
dfbcn1 <- dfbcn %>% 
  mutate(u1 = 0.8*log(2),
         u2 = 0.8*log(4),
         u3 = 0.8*log(7),
         u4 = 0.8*log(8),
         u5 = 0.8*log(7)-0.2,
         u6 = 0.8*log(5)-0.2,
         u7 = 0.8*log(2)-0.2,
         u8 = 0.8*log(1),
         c1 = ifelse(wage - 300*2 >= 0, 1, 0),
         c2 = ifelse(wage - 300*4 >= 0, 1, 0),
         c3 = ifelse(wage - 300*7 >= 0, 1, 0),
         c4 = ifelse(wage - 300*8 >= 0, 1, 0),
         c5 = ifelse(wage - 300*7 - 70 >= 0, 1, 0),
         c6 = ifelse(wage - 300*5 - 70 >= 0, 1, 0),
         c7 = ifelse(wage - 300*2 - 70 >= 0, 1, 0),
         c8 = ifelse(wage - 300*1 >= 0, 1, 0),
         )

dfbcn <- dfbcn %>% 
  mutate(u1 = (1/3)*log(2)+(2/3)*log(1),
         u2 = (1/3)*log(3)+(2/3)*log(1),
         u3 = (1/3)*log(4)+(2/3)*log(2),
         u4 = (1/3)*log(4)+(2/3)*log(3),
         u5 = (1/3)*log(3)+(2/3)*log(4),
         u6 = (1/3)*log(2)+(2/3)*log(4),
         u7 = (1/3)*log(1)+(2/3)*log(3),
         u8 = (1/3)*log(1)+(2/3)*log(1),
         c1 = ifelse(wage - 200*2 - 300*1 - 70 >= 0, 1, 0),
         c2 = ifelse(wage - 200*3 - 300*1 - 70 >= 0, 1, 0),
         c3 = ifelse(wage - 200*4 - 300*2 >= 0, 1, 0),
         c4 = ifelse(wage - 200*4 - 300*3 >= 0, 1, 0),
         c5 = ifelse(wage - 200*3 - 300*4 >= 0, 1, 0),
         c6 = ifelse(wage - 200*2 - 300*4 >= 0, 1, 0),
         c7 = ifelse(wage - 200*1 - 300*3 >= 0, 1, 0),
         c8 = ifelse(wage - 200*1 - 300*1 >= 0, 1, 0),
  )

### CHOOSING THE MAXIMUM
# Setting the utility of non-affordable options to a minimum
dfbcnmax <- dfbcn %>% 
  mutate(u1 = ifelse(c1 == 1, u1, -1),
         u2 = ifelse(c2 == 1, u2, -1),
         u3 = ifelse(c3 == 1, u3, -1),
         u4 = ifelse(c4 == 1, u4, -1),
         u5 = ifelse(c5 == 1, u5, -1),
         u6 = ifelse(c6 == 1, u6, -1),
         u7 = ifelse(c7 == 1, u7, -1),
         u8 = ifelse(c8 == 1, u8, -0.99), ## Like this, we make sure than whenever 8 is not affordable, none is chosen.
         )


## Choosing the maximum between the affordable (or 8 if none is affordable)
dfbcnmax$max <- pmax(dfbcnmax$u1, dfbcnmax$u2, dfbcnmax$u3, dfbcnmax$u4,
                     dfbcnmax$u5, dfbcnmax$u6, dfbcnmax$u7, dfbcnmax$u8)

# Dummy for each max
dfbcnmax <- dfbcnmax %>% 
  mutate(max1 = ifelse(max == u1, 1, 0),
         max2 = ifelse(max == u2, 1, 0),
         max3 = ifelse(max == u3, 1, 0),
         max4 = ifelse(max == u4, 1, 0),
         max5 = ifelse(max == u5, 1, 0),
         max6 = ifelse(max == u6, 1, 0),
         max7 = ifelse(max == u7, 1, 0),
         max8 = ifelse(max == u8, 1, 0))

# Indicators for the level of security and infrastructure in each chosen point
dfbcnmax <- dfbcnmax %>% 
  mutate(security = ifelse(max == u1 | max == u6, 2,
                           ifelse(max == u2 | max == u5, 3,
                                  ifelse(max == u3 | max == u4, 4, 
                                         ifelse(max == u7 | max == u8, 1, 0)))),
         infrastructure = ifelse(max == u1 | max == u2 | max == u8, 1,
                                  ifelse(max == u3, 2,
                                      ifelse(max == u4 | max == u7, 3, 
                                         ifelse(max == u5 | max == u6, 4, 0)))))

dfbcnmax <- dfbcnmax %>% 
  mutate(max1 = ifelse(max == u1, 1, 0),
         max2 = ifelse(max == u2, 1, 0),
         max3 = ifelse(max == u3, 1, 0),
         max4 = ifelse(max == u4, 1, 0),
         max5 = ifelse(max == u5, 1, 0),
         max6 = ifelse(max == u6, 1, 0),
         max7 = ifelse(max == u7, 1, 0),
         max8 = ifelse(max == u8, 1, 0))

dfbcnmax %>%  
  filter(u8 != -0.99) %>%
  select(19:26) %>% 
  summary(dfbcnmax)

## What's left after paying rent & commuting costs?
dfbcnmax$net_wage = ifelse(dfbcnmax$max1 == 1, dfbcnmax$wage - 200*2 - 300*1 -70, 
                        ifelse(dfbcnmax$max2 == 1, dfbcnmax$wage - 200*3 - 300*1 -70, 
                               ifelse(dfbcnmax$max3 == 1, dfbcnmax$wage - 200*4 - 300*2, 
                                      ifelse(dfbcnmax$max4 == 1, dfbcnmax$wage - 200*4 - 300*3, 
                                             ifelse(dfbcnmax$max5 == 1, dfbcnmax$wage - 200*3 - 300*4, 
                                                    ifelse(dfbcnmax$max6 == 1, dfbcnmax$wage - 200*2 - 300*4, 
                                                           ifelse(dfbcnmax$max7 == 1, dfbcnmax$wage - 200*1 - 300*3, 
                                                                  ifelse(dfbcnmax$max8 == 1 & dfbcnmax$wage - 500 > 0, dfbcnmax$wage - 200*1 - 300*1, dfbcnmax$wage))))))))

summary(dfbcnmax$net_wage)

## Chosen point
dfbcnmax$point = ifelse(dfbcnmax$max1 == 1, 1, 
                           ifelse(dfbcnmax$max2 == 1, 2, 
                                  ifelse(dfbcnmax$max3 == 1, 3, 
                                         ifelse(dfbcnmax$max4 == 1, 4, 
                                                ifelse(dfbcnmax$max5 == 1, 5, 
                                                       ifelse(dfbcnmax$max6 == 1, 6, 
                                                              ifelse(dfbcnmax$max7 == 1, 7, 
                                                                     ifelse(dfbcnmax$max8 == 1, 8, 0))))))))
## Bear in mind some people have gross wages=net wages!!
## Those that could not afford to live anywhere.
no_location <- dfbcnmax %>% 
  filter(max == -0.99)

sum(no_location$max8==1)/10000

# Group by point
results <- dfbcnmax %>% 
  filter(max != -0.99) %>% 
  group_by(max) %>% 
  summarise(point = mean(point), mean_wage = mean(wage), sd_wage = sd(wage), mean_security = mean(security),
            mean_infrastructure = mean(infrastructure),
            mean_netwage = mean(net_wage), sd_netwage = sd(net_wage), n = n(), share = n()/10000)


# Number of people with location
sum(results$`n()`)


#### SECOND STAGE MAXIMIZATION
dfbcnmax <- dfbcnmax %>% 
  mutate(housing = ((net_wage)*(security*(1-(1/3))))/(1*(infrastructure*(1/3)+security*(1-(1/3)))),
         education = ((net_wage)*(infrastructure*(1/3)))/(2*(infrastructure*(1/3)+security*(1-(1/3)))),
         net_wage_inverse = education*2 + housing*1,
         utility2 = infrastructure*(1/3)*log(education) + security*(1-(1/3))*log(housing)
         )


results_inequality <- dfbcnmax %>% 
  filter(max != -0.99) %>% 
  group_by(max) %>% 
  summarise(point = mean(point), mean_wage = mean(wage), sd_wage = sd(wage), mean_security = mean(security),
            mean_infrastructure = mean(infrastructure),
            mean_netwage = mean(net_wage), sd_netwage = sd(net_wage),
            mean_education = mean(education), mean_housing = mean(housing),
            n = n(), share = n()/10000)

cor(results_inequality$mean_wage, results_inequality$mean_education)
cor(results_inequality$mean_wage, results_inequality$mean_housing)



######---------------########
# GRAPH 1
w1 <- rnorm(n = 10000, mean = 21668, sd = 10000)
w2 <- rnorm(n = 10000, mean = 21668, sd = 10000)
w3 <- rnorm(n = 10000, mean = 21668, sd = 10000)
w4 <- rnorm(n = 10000, mean = 21668, sd = 10000)
w5 <- rnorm(n = 10000, mean = 21668, sd = 10000)
w6 <- rnorm(n = 10000, mean = 21668, sd = 10000)
w7 <- rnorm(n = 10000, mean = 21668, sd = 10000)
w8 <- rnorm(n = 10000, mean = 21668, sd = 10000)
w9 <- rnorm(n = 10000, mean = 21668, sd = 10000)
w10 <- rnorm(n = 10000, mean = 21668, sd = 10000)
w11 <- rnorm(n = 10000, mean = 21668, sd = 10000)
w12 <- rnorm(n = 10000, mean = 21668, sd = 10000)
w13 <- rnorm(n = 10000, mean = 21668, sd = 10000)

graph <- data.frame(w1=w1, w2=w2, w3=w3, w4=w4, w5=w5, w6=w6, w7=w7, w8=w8, w9=w9,
                    w10=w10, w11=w11, w12=w12, w13=w13)


graph$w1 <- ifelse(graph$w1 < 0, 0, graph$w1/14)
graph$w2 <- ifelse(graph$w2 < 0, 0, graph$w2/14)
graph$w3 <- ifelse(graph$w3 < 0, 0, graph$w3/14)
graph$w4 <- ifelse(graph$w4 < 0, 0, graph$w4/14)
graph$w5 <- ifelse(graph$w5 < 0, 0, graph$w5/14)
graph$w6 <- ifelse(graph$w6 < 0, 0, graph$w6/14)
graph$w7 <- ifelse(graph$w7 < 0, 0, graph$w7/14)
graph$w8 <- ifelse(graph$w8 < 0, 0, graph$w8/14)
graph$w9 <- ifelse(graph$w9 < 0, 0, graph$w9/14)
graph$w10 <- ifelse(graph$w10 < 0, 0, graph$w10/14)
graph$w11 <- ifelse(graph$w11 < 0, 0, graph$w11/14)
graph$w12 <- ifelse(graph$w12 < 0, 0, graph$w12/14)
graph$w13 <- ifelse(graph$w13 < 0, 0, graph$w13/14)

plot_wage <- ggplot(data = graph) + 
  geom_density(mapping = aes(x = w1), color = "blue") + 
  geom_density(mapping = aes(x = w2), color = "red") + 
  geom_density(mapping = aes(x = w3), color = "pink") + 
  geom_density(mapping = aes(x = w4), color = "yellow") +   
  geom_density(mapping = aes(x = w5), color = "orange") + 
  geom_density(mapping = aes(x = w6), color = "green") +   
  geom_density(mapping = aes(x = w7), color = "grey") + 
  geom_density(mapping = aes(x = w8), color = "gold") + 
  geom_density(mapping = aes(x = w9), color = "darkblue") + 
  geom_density(mapping = aes(x = w10), color = "darkgreen") + 
  geom_density(mapping = aes(x = w11), color = "magenta") + 
  geom_density(mapping = aes(x = w12), color = "black") + 
  geom_density(mapping = aes(x = w13), color = "brown") + 
  geom_vline(xintercept = 1547.71, linetype = "dashed") +
  labs(title = "Simulations of Wage Distributions for Barcelona (n=10000)",
       x = "Monthly Wage in 2021€", y ="Density")
plot_wage <- plot_wage + theme_classic()
plot_wage

# GRAPH 2
plot_distribution <- 
  ggplot(results) +
  geom_bar(aes(x = reorder(factor(point), -mean_wage), y = mean_wage, fill = factor(point)),  stat = "identity", alpha = .34) + 
  geom_bar(aes(x = reorder(factor(point), -mean_netwage), y = mean_netwage, fill = factor(point)),  stat = "identity", alpha = .9) + 
  geom_errorbar(aes(x = factor(point), ymin=mean_wage-sd_wage, ymax=mean_wage+sd_wage), width=.2,
                position=position_dodge(.9)) + 
  geom_errorbar(aes(x = factor(point), ymin=mean_netwage-sd_netwage, ymax=mean_netwage+sd_netwage), width=.1,
                position=position_dodge(.9)) + 
  coord_flip() + 
  labs(title = "Distribution of Wages and Net Wages over Locations (n=10000)",
       y = "Monthly Mean Wage in 2021€", x ="Location", fill = "Location") + theme_classic()
plot_distribution

grid.arrange(plot_wage, plot_distribution,nrow=1)
plot

## GRAPH 3
plot_results <- dfbcnmax %>% 
  filter(max != -0.99) %>% 
 ggplot() +
 geom_jitter(aes(y = education, x = point, color = factor(point))) +
  geom_smooth(aes(x = point, y = education), method = "lm", ) +
  facet_zoom(ylim = c(0, 100))  + theme_classic()
plot_results
 


## GRAPH 4
df_cor <- dfbcnmax %>% 
  filter(max5 == 0 & max != -0.99)  %>% 
  select(c("wage", "max","net_wage", "security", "infrastructure", "education", "housing", "utility2")) %>% 
  rename(Wage = wage, Utility_1 = max, Net_Wage = net_wage, Security = security,
         Infrastructure = infrastructure, Education = education, Housing = housing, Utility_2 = utility2)

matrix_cor <- round(cor(df_cor), 2)

plot_correlations  <- ggcorrplot::ggcorrplot(matrix_cor, type = "lower",
                                 lab = TRUE, colors = c("blue", "green", "red")) +
  labs(title = "Correlation Matrix (excl. point 5)")
plot_correlations


library(truncnorm)
library(dplyr)
library(tidyr)
library(lmerTest)
library(doParallel)
library(ggplot2)

rgamma.meansd=function(n,mean,sd)
{
  k=(sd^2)/mean
  s=mean/k
  
  return(rgamma(n=n,shape=s,scale=k))
}

simulate.mc2.data <- function(main.effect, class.effect, student.effect){
  
  n.classes <- 200
  
  class.enrollment <- round(rgamma.meansd(n.classes, mean=50, sd=20))
  
  student.sd <- 0.15 # sd of student scores after taking all other factors into acocunt; essentially, residual variance in the model.
  
  control.mu <- 0.70 # what will the central tendency of class scores be?
  control.sd <- 0.05 # how much variance is there in class scores?
  
  class.mu <- rnorm(n.classes, control.mu, control.sd) # baseline score for each class
  
  treatment.sd <- 0.6*student.sd # class-level sd of cohen's d for retrieval practice; how much do effects vary between classes?
  treatment.mu <- main.effect*student.sd # overall benefit of retrieval practice on the class level, scaled to student sd.
  
  class.treatment.effect <- rnorm(n.classes, mean = treatment.mu, sd = treatment.sd)
  
  class.moderator.sd <- 0.6*student.sd # sd of class moderator (variance from expected tendency due to class moderator)
  class.moderator.coeff <- class.effect*class.moderator.sd # coefficient of class moderator effect
  
  class.moderator.vals <- sample(c(-.5, .5), n.classes, replace=T) #discrete moderator
  
  class.moderator.effect <- rnorm(n.classes, mean=class.moderator.coeff*class.moderator.vals, sd=class.moderator.sd)
  
  student.moderator.sd <- 0.6*student.sd
  student.moderator.mu <- student.effect*student.sd
  student.moderator.coeff <- rnorm(n.classes, mean=student.moderator.mu, sd=student.moderator.sd) # coefficient of student moderators, with heterogenity at class level
  
  student.moderator.vals <- sample(c(-.5,.5), sum(class.enrollment), replace=T) #discrete student moderator
  
  simulated.data <- tibble(
    student = factor(rep(1:sum(class.enrollment))),
    class = factor(rep(1:n.classes, class.enrollment)),
    student.moderator = student.moderator.vals
  )
  
  simulated.data %<>%
    rowwise() %>%
    mutate(
      class.moderator = class.moderator.vals[class],
      #student.moderator.effect = rnorm(1, mean=student.moderator.coeff[class]*student.moderator, sd=student.moderator.sd),
      #treatment = rnorm(1,mean=class.mu[class] + class.treatment.effect[class] + class.moderator.effect[class] + student.moderator.effect, sd = student.sd),
      #condition = rnorm(1,mean=class.mu[class], sd = student.sd),
      y = rnorm(1, class.treatment.effect[class] + class.moderator.effect[class] + student.moderator.coeff[class]*student.moderator, sd = student.sd))
  
  return(simulated.data)
  #ggplot(simulated.data, aes(x=class, y=y, color=condition))+
  #  geom_boxplot()+
  #  theme_bw()
}

run.sim <- function(main, class, student){
  data <- simulate.mc2.data(main, class, student)
  result <- lmer(y ~ class.moderator + student.moderator + (student.moderator|class), data=data)
  #result <- lmer(y ~ 1 + class.moderator + (1|class), data=data)
  summary(result)
  main.detected <- summary(result)$coefficients["(Intercept)", 5] < 0.05
  class.mod.detected <- summary(result)$coefficients["class.moderator", 5] < 0.05
  student.mod.detected <- summary(result)$coefficients["student.moderator", 5] < 0.05
  return(list(
    main=main.detected,
    class=class.mod.detected,
    student=student.mod.detected)
  )
}

simulate.power <- function(main, class, student){
  n.sims <- 25
  sim.pwr.data <- tibble(run=1:n.sims, main=F, class=F, student=F)
  for(i in 1:n.sims){
    sim.result <- run.sim(main, class, student)
    sim.pwr.data[i,"main"] = sim.result$main
    sim.pwr.data[i,"class"] = sim.result$class
    sim.pwr.data[i,"student"] = sim.result$student
    #print(i)
  }
  main.pwr <- sum(sim.pwr.data$main)/n.sims
  class.pwr <- sum(sim.pwr.data$class)/n.sims
  student.pwr <- sum(sim.pwr.data$student)/n.sims
  return(list(
    main=main.pwr,
    class=class.pwr,
    student=student.pwr
  ))
}

# class.pwr.table <- tibble(
#   class.size=seq(100,200,25),
#   main=0,
#   class=0,
#   student=0
# )
# 
# for(i in 1:nrow(class.pwr.table)){
#   pwr <- simulate.power(class.pwr.table$class.size[i])
#   class.pwr.table[i,"main"] <- pwr$main
#   class.pwr.table[i,"class"] <- pwr$class
#   class.pwr.table[i,"student"] <- pwr$student
# }
# 
# class.pwr.table <- class.pwr.table %>% pivot_longer(cols=2:4,names_to="effect",values_to="power")
# 
# ggplot(class.pwr.table, aes(x=class.size,y=power,color=effect))+
#   geom_line()

effect.pwr.table <- expand.grid(main=c(0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8),class=c(0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8),student=c(0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8))

# effect.pwr.table$main.pwr <- 0
# effect.pwr.table$class.pwr <- 0
# effect.pwr.table$student.pwr <- 0
# 
# for(i in 1:nrow(effect.pwr.table)){
#   print(paste("row",i,"of",nrow(effect.pwr.table)))
#   effects <- effect.pwr.table[i,]
#   pwr <- simulate.power(effects$main, effects$class, effects$student)
#   effect.pwr.table[i,"main.pwr"] <- pwr$main
#   effect.pwr.table[i,"class.pwr"] <- pwr$class
#   effect.pwr.table[i,"student.pwr"] <- pwr$student
# }
# 
# effect.pwr.table <- expand.grid(student=seq(0.1,1,0.1))
# 
# effect.pwr.table$student.pwr <- 0
# 
# for(i in 1:nrow(effect.pwr.table)){
#   print(paste("row",i,"of",nrow(effect.pwr.table)))
#   effects <- effect.pwr.table[i,]
#   pwr <- simulate.power(0.3, 0.1, effects$student)
#   effect.pwr.table[i,"student.pwr"] <- pwr$student
# }
# 
# ggplot(effect.pwr.table, aes(x=student,y=student.pwr))+
#   geom_point()+
#   geom_line()+
#   coord_cartesian(ylim=c(0,1))

cl <- makeCluster(4,outfile="log-par-2.txt")
registerDoParallel(cl)
getDoParWorkers()

result <- foreach(i=1:nrow(effect.pwr.table), .combine=rbind, .packages = c('lmerTest','dplyr')) %dopar% {
  print(paste("row",i,"of",nrow(effect.pwr.table)))
  effects <- effect.pwr.table[i,]
  pwr <- simulate.power(effects$main, effects$class, effects$student)
  return(list(
    main = effects$main,
    class = effects$class,
    student = effects$student,
    main.pwr = pwr$main,
    class.pwr = pwr$class,
    student.pwr = pwr$student
  ))
}

stopCluster(cl)

result.df <- as_tibble(result) %>% unnest(1:6)

saveRDS(result.df, "pwr-table.rds")

pwr.summary <- result.df %>%
  group_by(main) %>%
  summarize(power = mean(main.pwr)) %>%
  mutate(effect="main") %>%
  rename(effect.size = main) %>%
  bind_rows(
    result.df %>%
      group_by(class) %>%
      summarize(power = mean(class.pwr)) %>%
      mutate(effect="class") %>%
      rename(effect.size = class)
  ) %>%
  bind_rows(
    result.df %>%
      group_by(student) %>%
      summarize(power = mean(student.pwr)) %>%
      mutate(effect="student") %>%
      rename(effect.size = student)
  )

ggplot(pwr.summary, aes(x=effect.size, y=power, color = effect))+
  geom_point()+
  geom_line()+
  labs(x="Standardized Effect Size", y="Estimated Power", color="Level")+
  theme_bw(base_size = 14)

### test plotting
# main = 0.3
# class = 0.3
# student = 0.3
# data <- simulate.mc2.data(main, class, student)
# ggplot(data, aes(x=class, y=y, group=class))+
#   #geom_smooth(method="lm", se=F)+
#   geom_boxplot()+
#   theme_minimal()
# 
# t.test(data$y)  

### empirical check on main effect size
main = 0.3
class = 0.3
student = 0.3

simulate.mc2.data(main, class, student) %>% group_by(class,student.moderator) %>%
  summarize(m = mean(y), sd = sd(y), d=m/sd) %>%
  ungroup() %>%
  mutate(m.z = scale(m)) %>%
  lm(formula=m.z ~ student.moderator)

simulate.mc2.data(main, class, student) %>% group_by(class, student.moderator) %>%
  summarize(m = mean(y), sd = sd(y), d=m/sd) %>%
  ungroup() %>%
  mutate(m.z = scale(m)) %>%
  ggplot(aes(x=student.moderator,y=m.z))+
    geom_point()+
  geom_smooth(method='lm')

simulate.mc2.data(main, class, student) %>% group_by(class) %>%
  summarize(m = mean(y), sd = sd(y), d=m/sd) %>%
  pull(d) %>%
  hist()

a <- replicate(50, {simulate.mc2.data(main, class, student) %>% group_by(class) %>%
            summarize(m = mean(y), sd = sd(y), d=m/sd) %>%
            pull(d) %>%
            mean()})
hist(a)





library(igraph)
library(network)
library(NetworkExtinction)


#Set up random graph
fw<-erdos.renyi.game(100,1/100)



#Compare in and out degree distributions, calculate AIC & BIC
degree_distribution_in <- function(fw, name){
  AIC <- Cumulative <- Exp <- fit <- model <- truncated <- NULL
  totaldegree<- degree(fw, mode="in")
  K <- 0:max(totaldegree)
  For.Graph<- data.frame(K = K, Cumulative = NA, Scenario = name)
  for(i in 1:length(K)){
    For.Graph$Cumulative[i] <- sum(totaldegree>K[i])/length(totaldegree)
  }
  
  exp.model <- nls(Cumulative~exp(-K/y),start= list(y=0.1), data = For.Graph)
  
  For.Graph$Exp <- predict(exp.model)
  Summs.exp <- glance(exp.model)
  Summs.exp$model <- "Exponential"
  
  power <- filter(For.Graph, K != 0 & Cumulative != 0)
  powerlaw.model <- nls(Cumulative~K^-y, start= list(y=0), data = power)
  power$power <- predict(powerlaw.model)
  For.Graph <- full_join(For.Graph, power)
  Summs.power <- glance(powerlaw.model)
  Summs.power$model <- "Power"
  
  truncated.powerlaw.model <- nls(Cumulative~(K^-y)*(exp(-K/y)), start = list(y=1), data = power)
  power$truncated <- predict(truncated.powerlaw.model)
  For.Graph <- full_join(For.Graph, power)
  Summs.truncated <- glance(truncated.powerlaw.model)
  Summs.truncated$model <- "truncated"
  
  Summs <- full_join(Summs.exp, Summs.power)
  Summs <- full_join(Summs, Summs.truncated)
  Summs <- arrange(Summs, AIC)
  
  DF2 <- For.Graph %>% filter(K != 0 & Cumulative != 0) %>% gather(key = model, value = fit, Exp, power, truncated)
  
  g <- ggplot(DF2, aes_string(x = "K", y = "Cumulative")) + geom_line() + geom_point()+ scale_x_log10() + scale_y_log10() + theme_classic() + geom_line(aes_string(y ="fit", color = "model"))
  

  
  return(list(DDvalues = For.Graph, models = Summs, graph = g))
}



degree_distribution_out <- function(fw, name){
  AIC <- Cumulative <- Exp <- fit <- model <- truncated <- NULL
  totaldegree<- degree(fw, mode="out")
  K <- 0:max(totaldegree)
  For.Graph<- data.frame(K = K, Cumulative = NA, Scenario = name)
  for(i in 1:length(K)){
    For.Graph$Cumulative[i] <- sum(totaldegree>K[i])/length(totaldegree)
  }
  
  exp.model <- nls(Cumulative~exp(-K/y),start= list(y=0.1), data = For.Graph)
  
  For.Graph$Exp <- predict(exp.model)
  Summs.exp <- glance(exp.model)
  Summs.exp$model <- "Exponential"
  
  power <- filter(For.Graph, K != 0 & Cumulative != 0)
  powerlaw.model <- nls(Cumulative~K^-y, start= list(y=0), data = power)
  power$power <- predict(powerlaw.model)
  For.Graph <- full_join(For.Graph, power)
  Summs.power <- glance(powerlaw.model)
  Summs.power$model <- "Power"
  
  truncated.powerlaw.model <- nls(Cumulative~(K^-y)*(exp(-K/y)), start = list(y=1), data = power)
  power$truncated <- predict(truncated.powerlaw.model)
  For.Graph <- full_join(For.Graph, power)
  Summs.truncated <- glance(truncated.powerlaw.model)
  Summs.truncated$model <- "truncated"
  
  Summs <- full_join(Summs.exp, Summs.power)
  Summs <- full_join(Summs, Summs.truncated)
  Summs <- arrange(Summs, AIC)
  
  DF2 <- For.Graph %>% filter(K != 0 & Cumulative != 0) %>% gather(key = model, value = fit, Exp, power, truncated)
  
  g <- ggplot(DF2, aes_string(x = "K", y = "Cumulative")) + geom_line() + geom_point()+ scale_x_log10() + scale_y_log10() + theme_classic() + geom_line(aes_string(y ="fit", color = "model"))
  
  
  return(list(DDvalues = For.Graph, models = Summs, graph = g))
}

indeg<-degree_distribution_in(fw,"test")
indeg2<-as.data.frame(indeg$models)

outdeg<-degree_distribution_out(fw,"test")
outdeg2<-as.data.frame(outdeg$models)



####Run functions----


  
  t2<-fw
  
  t_in<-degree_distribution_in(t2,name="Test")
  t_in2<-as.data.frame(t_in$models)
  AIC_in<-which(t_in2 == min(t_in2$AIC, na.rm = TRUE), arr.ind = TRUE)
  best_model_AIC_in<-(t_in2[AIC_in[1],c("model")])
  BIC_in<-which(t_in2 == min(t_in2$BIC, na.rm = TRUE), arr.ind = TRUE)
  best_model_BIC_in<-(t_in2[BIC_in[1],c("model")])
  t_in3<-cbind(t_in2,deg_type="in",bestAIC=best_model_AIC_in,bestBIC=best_model_BIC_in)
  
  
  t_out<-degree_distribution_out(t2,name="Test")
  t_out2<-as.data.frame(t_out$models)
  AIC_out<-which(t_out2 == min(t_out2$AIC, na.rm = TRUE), arr.ind = TRUE)
  best_model_AIC_out<-(t_out2[AIC_out[1],c("model")])
  BIC_out<-which(t_out2 == min(t_out2$BIC, na.rm = TRUE), arr.ind = TRUE)
  best_model_BIC_out<-(t_out2[BIC_out[1],c("model")])
  t_out3<-cbind(t_out2,deg_type="out",bestAIC=best_model_AIC_out,bestBIC=best_model_BIC_out)
  
  t_comb<-rbind(t_in3,t_out3)

  t_comb<-reshape::cast(t_comb,best~model,value="AIC")





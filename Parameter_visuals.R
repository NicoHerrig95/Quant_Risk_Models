################################################################################  
######################### PARAMETER VISUALS ####################################
################################################################################ 



save_parameter_visuals = T
library(latex2exp)

cols = c("NNet1" = "red",
         "NNet2" = "darkblue",
         "NNet3" = "darkgreen")


# plot period
if (period == "covid"){plot_period <- "- turbulent period"
} else if (period == "calm"){plot_period <- "- calm period"}



################################################################################  
######################### PLOTTING DF #########################################
################################################################################ 

# automatically assigns either set for calm or turbulent(covid) period
if(period == "calm"){ 
  compare_STOXX50 <- data.frame(date = `^STOXX50E_test`$date,
                                pi_1_vanilla = EUSTOXX_vanilla_calm$pi1,
                                pi_2_vanilla = EUSTOXX_vanilla_calm$pi2,
                                sigma_1_vanilla = EUSTOXX_vanilla_calm$sigma1,
                                sigma_2_vanilla = EUSTOXX_vanilla_calm$sigma2,
                                mu_1_vanilla = EUSTOXX_vanilla_calm$mu1,
                                mu_2_vanilla = EUSTOXX_vanilla_calm$mu2,
                                pi_1_reg = EUSTOXX_regularized_calm$pi1,
                                pi_2_reg = EUSTOXX_regularized_calm$pi2,
                                sigma_1_reg = EUSTOXX_regularized_calm$sigma1,
                                sigma_2_reg = EUSTOXX_regularized_calm$sigma2,
                                mu_1_reg = EUSTOXX_regularized_calm$mu1,
                                mu_2_reg = EUSTOXX_regularized_calm$mu2,
                                pi_1_C3 = EUSTOXX_C3_calm$pi1,
                                pi_2_C3 = EUSTOXX_C3_calm$pi2,
                                pi_3_C3 = EUSTOXX_C3_calm$pi3,
                                sigma_1_C3 = EUSTOXX_C3_calm$sigma1,
                                sigma_2_C3 = EUSTOXX_C3_calm$sigma2,
                                sigma_3_C3 = EUSTOXX_C3_calm$sigma3,
                                mu_1_C3 = EUSTOXX_C3_calm$mu1,
                                mu_2_C3 = EUSTOXX_C3_calm$mu2,
                                mu_3_C3 = EUSTOXX_C3_calm$mu3)
  
  
  
  compare_FTSE <- data.frame(date = `^FTSE_test`$date,
                             pi_1_vanilla = FTSE_vanilla_calm$pi1,
                             pi_2_vanilla = FTSE_vanilla_calm$pi2,
                             sigma_1_vanilla = FTSE_vanilla_calm$sigma1,
                             sigma_2_vanilla = FTSE_vanilla_calm$sigma2,
                             mu_1_vanilla = FTSE_vanilla_calm$mu1,
                             mu_2_vanilla = FTSE_vanilla_calm$mu2,
                             pi_1_reg = FTSE_regularized_calm$pi1,
                             pi_2_reg = FTSE_regularized_calm$pi2,
                             sigma_1_reg = FTSE_regularized_calm$sigma1,
                             sigma_2_reg = FTSE_regularized_calm$sigma2,
                             mu_1_reg = FTSE_regularized_calm$mu1,
                             mu_2_reg = FTSE_regularized_calm$mu2,
                             pi_1_C3 = FTSE_C3_calm$pi1,
                             pi_2_C3 = FTSE_C3_calm$pi2,
                             pi_3_C3 = FTSE_C3_calm$pi3,
                             sigma_1_C3 = FTSE_C3_calm$sigma1,
                             sigma_2_C3 = FTSE_C3_calm$sigma2,
                             sigma_3_C3 = FTSE_C3_calm$sigma3,
                             mu_1_C3 = FTSE_C3_calm$mu1,
                             mu_2_C3 = FTSE_C3_calm$mu2,
                             mu_3_C3 = FTSE_C3_calm$mu3)
  
  compare_GSPC <- data.frame(date = `^GSPC_test`$date,
                             pi_1_vanilla = GSPC_vanilla_calm$pi1,
                             pi_2_vanilla = GSPC_vanilla_calm$pi2,
                             sigma_1_vanilla = GSPC_vanilla_calm$sigma1,
                             sigma_2_vanilla = GSPC_vanilla_calm$sigma2,
                             mu_1_vanilla = GSPC_vanilla_calm$mu1,
                             mu_2_vanilla = GSPC_vanilla_calm$mu2,
                             pi_1_reg = GSPC_regularized_calm$pi1,
                             pi_2_reg = GSPC_regularized_calm$pi2,
                             sigma_1_reg = GSPC_regularized_calm$sigma1,
                             sigma_2_reg = GSPC_regularized_calm$sigma2,
                             mu_1_reg = GSPC_regularized_calm$mu1,
                             mu_2_reg = GSPC_regularized_calm$mu2,
                             pi_1_C3 = GSPC_C3_calm$pi1,
                             pi_2_C3 = GSPC_C3_calm$pi2,
                             pi_3_C3 = GSPC_C3_calm$pi3,
                             sigma_1_C3 = GSPC_C3_calm$sigma1,
                             sigma_2_C3 = GSPC_C3_calm$sigma2,
                             sigma_3_C3 = GSPC_C3_calm$sigma3,
                             mu_1_C3 = GSPC_C3_calm$mu1,
                             mu_2_C3 = GSPC_C3_calm$mu2,
                             mu_3_C3 = GSPC_C3_calm$mu3)
  
  
} else if (period == "covid"){
  compare_STOXX50 <- data.frame(date = `^STOXX50E_test`$date,
                                pi_1_vanilla = EUSTOXX_vanilla_covid$pi1,
                                pi_2_vanilla = EUSTOXX_vanilla_covid$pi2,
                                sigma_1_vanilla = EUSTOXX_vanilla_covid$sigma1,
                                sigma_2_vanilla = EUSTOXX_vanilla_covid$sigma2,
                                mu_1_vanilla = EUSTOXX_vanilla_covid$mu1,
                                mu_2_vanilla = EUSTOXX_vanilla_covid$mu2,
                                pi_1_reg = EUSTOXX_regularized_covid$pi1,
                                pi_2_reg = EUSTOXX_regularized_covid$pi2,
                                sigma_1_reg = EUSTOXX_regularized_covid$sigma1,
                                sigma_2_reg = EUSTOXX_regularized_covid$sigma2,
                                mu_1_reg = EUSTOXX_regularized_covid$mu1,
                                mu_2_reg = EUSTOXX_regularized_covid$mu2,
                                pi_1_C3 = EUSTOXX_C3_covid$pi1,
                                pi_2_C3 = EUSTOXX_C3_covid$pi2,
                                pi_3_C3 = EUSTOXX_C3_covid$pi3,
                                sigma_1_C3 = EUSTOXX_C3_covid$sigma1,
                                sigma_2_C3 = EUSTOXX_C3_covid$sigma2,
                                sigma_3_C3 = EUSTOXX_C3_covid$sigma3,
                                mu_1_C3 = EUSTOXX_C3_covid$mu1,
                                mu_2_C3 = EUSTOXX_C3_covid$mu2,
                                mu_3_C3 = EUSTOXX_C3_covid$mu3)
  
  
  
  compare_FTSE <- data.frame(date = `^FTSE_test`$date,
                             pi_1_vanilla = FTSE_vanilla_covid$pi1,
                             pi_2_vanilla = FTSE_vanilla_covid$pi2,
                             sigma_1_vanilla = FTSE_vanilla_covid$sigma1,
                             sigma_2_vanilla = FTSE_vanilla_covid$sigma2,
                             mu_1_vanilla = FTSE_vanilla_covid$mu1,
                             mu_2_vanilla = FTSE_vanilla_covid$mu2,
                             pi_1_reg = FTSE_regularized_covid$pi1,
                             pi_2_reg = FTSE_regularized_covid$pi2,
                             sigma_1_reg = FTSE_regularized_covid$sigma1,
                             sigma_2_reg = FTSE_regularized_covid$sigma2,
                             mu_1_reg = FTSE_regularized_covid$mu1,
                             mu_2_reg = FTSE_regularized_covid$mu2,
                             pi_1_C3 = FTSE_C3_covid$pi1,
                             pi_2_C3 = FTSE_C3_covid$pi2,
                             pi_3_C3 = FTSE_C3_covid$pi3,
                             sigma_1_C3 = FTSE_C3_covid$sigma1,
                             sigma_2_C3 = FTSE_C3_covid$sigma2,
                             sigma_3_C3 = FTSE_C3_covid$sigma3,
                             mu_1_C3 = FTSE_C3_covid$mu1,
                             mu_2_C3 = FTSE_C3_covid$mu2,
                             mu_3_C3 = FTSE_C3_covid$mu3)
  
  compare_GSPC <- data.frame(date = `^GSPC_test`$date,
                             pi_1_vanilla = GSPC_vanilla_covid$pi1,
                             pi_2_vanilla = GSPC_vanilla_covid$pi2,
                             sigma_1_vanilla = GSPC_vanilla_covid$sigma1,
                             sigma_2_vanilla = GSPC_vanilla_covid$sigma2,
                             mu_1_vanilla = GSPC_vanilla_covid$mu1,
                             mu_2_vanilla = GSPC_vanilla_covid$mu2,
                             pi_1_reg = GSPC_regularized_covid$pi1,
                             pi_2_reg = GSPC_regularized_covid$pi2,
                             sigma_1_reg = GSPC_regularized_covid$sigma1,
                             sigma_2_reg = GSPC_regularized_covid$sigma2,
                             mu_1_reg = GSPC_regularized_covid$mu1,
                             mu_2_reg = GSPC_regularized_covid$mu2,
                             pi_1_C3 = GSPC_C3_covid$pi1,
                             pi_2_C3 = GSPC_C3_covid$pi2,
                             pi_3_C3 = GSPC_C3_covid$pi3,
                             sigma_1_C3 = GSPC_C3_covid$sigma1,
                             sigma_2_C3 = GSPC_C3_covid$sigma2,
                             sigma_3_C3 = GSPC_C3_covid$sigma3,
                             mu_1_C3 = GSPC_C3_covid$mu1,
                             mu_2_C3 = GSPC_C3_covid$mu2,
                             mu_3_C3 = GSPC_C3_covid$mu3)
  
}






################################################################################  
######################### PI PARAMETER #########################################
################################################################################ 


ggplot(data= compare_STOXX50) +
  geom_line(aes(y = pi_1_vanilla, x = date, color = "NNet1"))+
  geom_line(aes(y = pi_2_vanilla, x = date, color = "NNet1"))+
  geom_line(aes(y = pi_1_reg, x = date, color = "NNet2"))+
  geom_line(aes(y = pi_2_reg, x = date, color = "NNet2"))+
  geom_line(aes(y = pi_1_C3, x = date, color = "NNet3"))+
  geom_line(aes(y = pi_2_C3, x = date, color = "NNet3"))+
  geom_line(aes(y = pi_3_C3, x = date, color = "NNet3"))+
  scale_color_manual(values = cols)+
  labs(colour = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0))+
  theme(legend.position="bottom")+
  labs(
    title = substitute(paste(bold("Comparison of Mixture Parameters"))),
    x = substitute(paste(bold("Date"))),
    y = TeX(r"($\pi$ )"),
    color = "",
    subtitle = paste("EURO STOXX 50", plot_period, sep = " "))


if(save_parameter_visuals == T){
  if(period == "calm"){
    ggsave("pi_params_EUSTOXX_calm.png")
  } else if (period == "covid"){
    ggsave("pi_params_EUSTOXX_covid.png")
  }
}
  


# FTSE
ggplot(data= compare_FTSE) +
  geom_line(aes(y = pi_1_vanilla, x = date, color = "NNet1"))+
  geom_line(aes(y = pi_2_vanilla, x = date, color = "NNet1"))+
  geom_line(aes(y = pi_1_reg, x = date, color = "NNet2"))+
  geom_line(aes(y = pi_2_reg, x = date, color = "NNet2"))+
  geom_line(aes(y = pi_1_C3, x = date, color = "NNet3"))+
  geom_line(aes(y = pi_2_C3, x = date, color = "NNet3"))+
  geom_line(aes(y = pi_3_C3, x = date, color = "NNet3"))+
  scale_color_manual(values = cols)+
  labs(colour = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0))+
  theme(legend.position="bottom")+
  labs(
    title = substitute(paste(bold("Comparison of Mixture Parameters"))),
    x = substitute(paste(bold("Date"))),
    y = TeX(r"($\pi$ )"),
    color = "",
    subtitle = paste("FTSE 100", plot_period, sep = " "))




if(save_parameter_visuals == T){
  if(period == "calm"){
    ggsave("pi_params_FTSE_calm.png")
  } else if (period == "covid"){
    ggsave("pi_params_FTSE_covid.png")
  }
}


# S&ß 500

ggplot(data= compare_GSPC) +
  geom_line(aes(y = pi_1_vanilla, x = date, color = "NNet1"))+
  geom_line(aes(y = pi_2_vanilla, x = date, color = "NNet1"))+
  geom_line(aes(y = pi_1_reg, x = date, color = "NNet2"))+
  geom_line(aes(y = pi_2_reg, x = date, color = "NNet2"))+
  geom_line(aes(y = pi_1_C3, x = date, color = "NNet3"))+
  geom_line(aes(y = pi_2_C3, x = date, color = "NNet3"))+
  geom_line(aes(y = pi_3_C3, x = date, color = "NNet3"))+
  scale_color_manual(values = cols)+
  labs(colour = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0))+
  theme(legend.position="bottom")+
  labs(
    title = substitute(paste(bold("Comparison of Mixture Parameters"))),
    x = substitute(paste(bold("Date"))),
    y = TeX(r"($\pi$ )"),
    color = "",
    subtitle = paste("S&P 500", plot_period, sep = " "))



if(save_parameter_visuals == T){
  if(period == "calm"){
    ggsave("pi_params_SP_calm.png")
  } else if (period == "covid"){
    ggsave("pi_params_SP_covid.png")
  }
}




################################################################################  
######################### SIGMA PARAMETER ######################################
################################################################################ 

ggplot(data= compare_STOXX50) +
  geom_line(aes(y = sigma_1_vanilla, x = date, color = "NNet1"))+
  geom_line(aes(y = sigma_2_vanilla, x = date, color = "NNet1"))+
  geom_line(aes(y = sigma_1_reg, x = date, color = "NNet2"))+
  geom_line(aes(y = sigma_2_reg, x = date, color = "NNet2"))+
  geom_line(aes(y = sigma_1_C3, x = date, color = "NNet3"))+
  geom_line(aes(y = sigma_2_C3, x = date, color = "NNet3"))+
  geom_line(aes(y = sigma_3_C3, x = date, color = "NNet3"))+
  scale_color_manual(values = cols)+
  labs(colour = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0))+
  theme(legend.position="bottom")+
  labs(
    title = substitute(paste(bold("Comparison of Variance Parameters"))),
    x = substitute(paste(bold("Date"))),
    y = TeX(r"($\sigma$ )"),
    color = "",
    subtitle = paste("EURO STOXX 50", plot_period, sep = " "))



if(save_parameter_visuals == T){
  if(period == "calm"){
    ggsave("sigma_params_EUSTOXX_calm.png")
  } else if (period == "covid"){
    ggsave("sigma_params_EUSTOXX_covid.png")
  }
}


ggplot(data= compare_FTSE) +
  geom_line(aes(y = sigma_1_vanilla, x = date, color = "NNet1"))+
  geom_line(aes(y = sigma_2_vanilla, x = date, color = "NNet1"))+
  geom_line(aes(y = sigma_1_reg, x = date, color = "NNet2"))+
  geom_line(aes(y = sigma_2_reg, x = date, color = "NNet2"))+
  geom_line(aes(y = sigma_1_C3, x = date, color = "NNet3"))+
  geom_line(aes(y = sigma_2_C3, x = date, color = "NNet3"))+
  geom_line(aes(y = sigma_3_C3, x = date, color = "NNet3"))+
  scale_color_manual(values = cols)+
  labs(colour = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0))+
  theme(legend.position="bottom")+
  labs(
    title = substitute(paste(bold("Comparison of Variance Parameters"))),
    x = substitute(paste(bold("Date"))),
    y = TeX(r"($\sigma$ )"),
    color = "",
    subtitle = paste("FTSE 100", plot_period, sep = " "))


if(save_parameter_visuals == T){
  if(period == "calm"){
    ggsave("sigma_params_FTSE_calm.png")
  } else if (period == "covid"){
    ggsave("sigma_params_FTSE_covid.png")
  }
}

ggplot(data= compare_GSPC) +
  geom_line(aes(y = sigma_1_vanilla, x = date, color = "NNet1"))+
  geom_line(aes(y = sigma_2_vanilla, x = date, color = "NNet1"))+
  geom_line(aes(y = sigma_1_reg, x = date, color = "NNet2"))+
  geom_line(aes(y = sigma_2_reg, x = date, color = "NNet2"))+
  geom_line(aes(y = sigma_1_C3, x = date, color = "NNet3"))+
  geom_line(aes(y = sigma_2_C3, x = date, color = "NNet3"))+
  geom_line(aes(y = sigma_3_C3, x = date, color = "NNet3"))+
  scale_color_manual(values = cols)+
  labs(colour = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0))+
  theme(legend.position="bottom")+
  labs(
    title = substitute(paste(bold("Comparison of Variance Parameters"))),
    x = substitute(paste(bold("Date"))),
    y = TeX(r"($\sigma$ )"),
    color = "",
    subtitle = paste("S&P 500", plot_period, sep = " "))

if(save_parameter_visuals == T){
  if(period == "calm"){
    ggsave("sigma_params_SP_calm.png")
  } else if (period == "covid"){
    ggsave("sigma_params_SP_covid.png")
  }
}



################################################################################  
######################### MU PARAMETER ######################################
################################################################################ 

ggplot(data= compare_STOXX50) +
  geom_line(aes(y = mu_1_vanilla, x = date, color = "NNet1"))+
  geom_line(aes(y = mu_2_vanilla, x = date, color = "NNet1"))+
  geom_line(aes(y = mu_1_reg, x = date, color = "NNet2"))+
  geom_line(aes(y = mu_2_reg, x = date, color = "NNet2"))+
  geom_line(aes(y = mu_1_C3, x = date, color = "NNet3"))+
  geom_line(aes(y = mu_2_C3, x = date, color = "NNet3"))+
  geom_line(aes(y = mu_3_C3, x = date, color = "NNet3"))+
  scale_color_manual(values = cols)+
  labs(colour = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0))+
  theme(legend.position="bottom")+
  labs(
    title = substitute(paste(bold("Comparison of Location Parameters"))),
    x = substitute(paste(bold("Date"))),
    y = TeX(r"($\mu$ )"),
    color = "",
    subtitle = paste("EURO STOXX 50", plot_period, sep = " "))



if(save_parameter_visuals == T){
  if(period == "calm"){
    ggsave("mu_params_EUSTOXX_calm.png")
  } else if (period == "covid"){
    ggsave("mu_params_EUSTOXX_covid.png")
  }
}


ggplot(data= compare_FTSE) +
  geom_line(aes(y = mu_1_vanilla, x = date, color = "NNet1"))+
  geom_line(aes(y = mu_2_vanilla, x = date, color = "NNet1"))+
  geom_line(aes(y = mu_1_reg, x = date, color = "NNet2"))+
  geom_line(aes(y = mu_2_reg, x = date, color = "NNet2"))+
  geom_line(aes(y = mu_1_C3, x = date, color = "NNet3"))+
  geom_line(aes(y = mu_2_C3, x = date, color = "NNet3"))+
  geom_line(aes(y = mu_3_C3, x = date, color = "NNet3"))+
  scale_color_manual(values = cols)+
  labs(colour = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0))+
  theme(legend.position="bottom")+
  labs(
    title = substitute(paste(bold("Comparison of Location Parameters"))),
    x = substitute(paste(bold("Date"))),
    y = TeX(r"($\mu$ )"),
    color = "",
    subtitle = paste("FTSE 100", plot_period, sep = " "))


if(save_parameter_visuals == T){
  if(period == "calm"){
    ggsave("mu_params_FTSE_calm.png")
  } else if (period == "covid"){
    ggsave("mu_params_FTSE_covid.png")
  }
}

ggplot(data= compare_GSPC) +
  geom_line(aes(y = mu_1_vanilla, x = date, color = "NNet1"))+
  geom_line(aes(y = mu_2_vanilla, x = date, color = "NNet1"))+
  geom_line(aes(y = mu_1_reg, x = date, color = "NNet2"))+
  geom_line(aes(y = mu_2_reg, x = date, color = "NNet2"))+
  geom_line(aes(y = mu_1_C3, x = date, color = "NNet3"))+
  geom_line(aes(y = mu_2_C3, x = date, color = "NNet3"))+
  geom_line(aes(y = mu_3_C3, x = date, color = "NNet3"))+
  scale_color_manual(values = cols)+
  labs(colour = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 0),
        plot.subtitle = element_text(hjust = 0.5, vjust = 0))+
  theme(legend.position="bottom")+
  labs(
    title = substitute(paste(bold("Comparison of Location Parameters"))),
    x = substitute(paste(bold("Date"))),
    y = TeX(r"($\mu$ )"),
    color = "",
    subtitle = paste("S&P 500", plot_period, sep = " "))

if(save_parameter_visuals == T){
  if(period == "calm"){
    ggsave("mu_params_SP_calm.png")
  } else if (period == "covid"){
    ggsave("mu_params_SP_covid.png")
  }
}

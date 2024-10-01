library(ggplot2)
parent_dir <- "<set_dir>"

##need to create Output and Graphs directory in parent folder
data_dir <- paste0(parent_dir,"Output/")
output_dir <- paste0(parent_dir,"Graphs/")

g_legend <- function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  legend
} 

N_scenario <- 300
#Output <- matrix(0,ncol=4,nrow=N_scenario)
width_dim <- 8
height_dim <- 6

#for(i in 1:N_scenario){
  
#  Output[i,] <- readRDS(paste0(data_dir,"ICTMC_4_2024_output_",i,".RDS"))[i,]
#}

scenario_list <- read.csv(paste0(parent_dir,"scenario_list_4.csv"),header=TRUE)

full_output <- cbind(scenario_list,
                     Output,
                     rep(0,N_scenario),
                     rep(0,N_scenario),
                     rep(0,N_scenario),
                     rep(0,N_scenario),
                     rep(0,N_scenario),
                     rep(0,N_scenario),
                     rep(0,N_scenario),
                     rep(0,N_scenario),
                     rep(0,N_scenario),
                     rep("",N_scenario),
                     rep("",N_scenario))

names(full_output)
names(full_output)[11:25] <- c("number_of_trials","Singular_fit","significant_and_singular","significant","convergence_rate","Sigular_and_sig_rate","Power",
                               "L_convergence_rate","L_Sigular_and_sig_rate","L_Power",
                               "U_convergence_rate","U_Sigular_and_sig_rate","U_Power",
                               "ICC level", "Alt ICC Level"
) 

full_output$convergence_rate <- full_output$Singular_fit/full_output$number_of_trials
full_output$Sigular_and_sig_rate <- full_output$significant_and_singular/full_output$number_of_trials
full_output$Full_Power <- full_output$significant/full_output$number_of_trials

full_output$L_convergence_rate <- full_output$convergence_rate - 1.96*sqrt((full_output$convergence_rate*(1-full_output$convergence_rate))/full_output$number_of_trials)
full_output$L_Sigular_and_sig_rate <- full_output$Sigular_and_sig_rate - 1.96*sqrt((full_output$Sigular_and_sig_rate*(1-full_output$Sigular_and_sig_rate))/full_output$number_of_trials)
full_output$L_Full_Power <- full_output$Full_Power - 1.96*sqrt((full_output$Full_Power*(1-full_output$Full_Power))/full_output$number_of_trials)

full_output$U_convergence_rate <- full_output$convergence_rate + 1.96*sqrt((full_output$convergence_rate*(1-full_output$convergence_rate))/full_output$number_of_trials)
full_output$U_Sigular_and_sig_rate <- full_output$Sigular_and_sig_rate + 1.96*sqrt((full_output$Sigular_and_sig_rate*(1-full_output$Sigular_and_sig_rate))/full_output$number_of_trials)
full_output$U_Full_Power <- full_output$Full_Power + 1.96*sqrt((full_output$Full_Power*(1-full_output$Full_Power))/full_output$number_of_trials)

plot_power <- function(data){
  
  pd <- position_dodge(20)
  wd <- 50
  
  p_power <- ggplot(data,
                    aes(x=n_sample_size,y=Full_Power,colour=Legend)) +
    geom_line(position=pd)+
    geom_errorbar(aes(ymin=L_Full_Power, ymax=U_Full_Power), width=wd,position=pd)+
    geom_point(position=pd)
  
  p_converge <- ggplot(data,
                       aes(x=n_sample_size,y=1-convergence_rate,colour=Legend)) +
    geom_line(position=pd)+
    geom_errorbar(aes(ymax=1-L_convergence_rate, ymin=1-U_convergence_rate), width=wd,position=pd)+
    geom_point(position=pd)
  
  p_sig <- ggplot(data,
                  aes(x=n_sample_size,y=Sigular_and_sig_rate,colour=Legend)) +
    geom_line(position=pd)+
    geom_errorbar(aes(ymin=L_Sigular_and_sig_rate, ymax=U_Sigular_and_sig_rate), width=wd,position=pd)+
    geom_point(position=pd)
  
  data_2 = data.frame(
    n_sample_size = rep(data$n_sample_size,2),
    Legend = rep(data$Legend,2),
    Value = c(data$Full_Power,1-data$convergence_rate),
    L_CI = c(data$L_Full_Power,1-data$U_convergence_rate),
    U_CI = c(data$U_Full_Power,1-data$L_convergence_rate),
    Type = c(rep("Power",dim(data)[1]),rep("Convergence Rate",dim(data)[1]))
  )
  
  p_full <- ggplot(data_2,
                   aes(x=n_sample_size,y=Value,colour=Legend)) +
    geom_line(position=pd,aes(linetype=Type))+
    geom_errorbar(aes(ymin=L_CI, ymax=U_CI), width=wd,position=pd)+
    geom_point(position=pd)+
    theme(legend.position="top")+
    scale_y_continuous(limits = c(0, 1))+
    scale_x_continuous(limits = c(125,625))+
    labs(x = "Sample Size", y = "")+
    theme(plot.title = element_text(hjust = 0.5),text=element_text(size=17))
  
  invisible(list(p_power=p_power,p_converge=p_converge,p_sig=p_sig,p_full=p_full))  
}

##only interested in sigma_11 of 0.5 for ICTMC poster
full_output <- full_output[full_output$sigma_11==0.5,]

##GRAPH 1 - Different ICC - Even Recruitment - Varying Correlation
common_ICC <- full_output[
  (full_output$ICC_status=="common" | full_output$ICC_status=="differing correlation same effect")  & 
    full_output$recruitment_status=="even",]
common_ICC$Legend <- common_ICC$sigma_12
##didn't save corr =1 for common ICC need to manually change
uni_sigma12 <- unique(common_ICC$sigma_12)
common_ICC$sigma_12[common_ICC$ICC_status=="common"] <- common_ICC$sigma_11[common_ICC$ICC_status=="common"] 
common_ICC$Legend <- common_ICC$sigma_12


uni_sigma12 <- unique(common_ICC$sigma_12)
common_ICC$Legend[common_ICC$Legend==uni_sigma12[1]] <- "Matching Surgeon Effect"
common_ICC$Legend[common_ICC$Legend==uni_sigma12[2]] <- "Low Correlation"
common_ICC$Legend[common_ICC$Legend==uni_sigma12[3]] <- "Moderate Correlation"
common_ICC$Legend[common_ICC$Legend==uni_sigma12[4]] <- "High Correlation"


active_plot_1 <- plot_power(common_ICC) 

pdf(paste0(output_dir,"differing_ICC",".pdf"),width=width_dim,height=height_dim)
active_plot_1$p_full + 
  ggtitle("Different ICC - Even Recruitment - Varying Correlation") +
  guides(linetype =  "none",colour="none" )
dev.off()

legend_tmp <- g_legend(active_plot_1$p_full) 
grid.newpage()
pdf(paste0(output_dir,"legend_1.pdf"),width=2*width_dim,height=1)
grid.draw(legend_tmp)
dev.off()


##GRAPH 2 - Different ICC - Uneven Recruitment - Varying Correlation
uneven_recruitment <- full_output[
  (full_output$ICC_status=="common" | full_output$ICC_status=="differing correlation same effect")  & 
    full_output$recruitment_status=="highly uneven",]
uneven_recruitment$Legend <- uneven_recruitment$sigma_12
##didn't save corr =1 for common ICC need to manually change
uni_sigma12 <- unique(uneven_recruitment$sigma_12)
uneven_recruitment$sigma_12[uneven_recruitment$ICC_status=="common"] <- uneven_recruitment$sigma_11[uneven_recruitment$ICC_status=="common"] 
uneven_recruitment$Legend <- uneven_recruitment$sigma_12


uni_sigma12 <- unique(uneven_recruitment$sigma_12)
uneven_recruitment$Legend[uneven_recruitment$Legend==uni_sigma12[1]] <- "Matching Surgeon Effect"
uneven_recruitment$Legend[uneven_recruitment$Legend==uni_sigma12[2]] <- "Low Correlation"
uneven_recruitment$Legend[uneven_recruitment$Legend==uni_sigma12[3]] <- "Moderate Correlation"
uneven_recruitment$Legend[uneven_recruitment$Legend==uni_sigma12[4]] <- "High Correlation"

active_plot <- plot_power(uneven_recruitment)

pdf(paste0(output_dir,"uneven_differing_ICC",".pdf"),width=width_dim,height=height_dim)
active_plot$p_full  + 
  ggtitle("Different ICC - Uneven Recruitment - Varying Correlation") +
  guides(linetype =  "none", color= "none" )
dev.off()



##GRAPH 3 - Different ICC - Even Recruitment - Varying ICC
differing_ICC <- full_output[(full_output$ICC_status=="common" |full_output$ICC_status=="Higher ICC in Exp") & 
                               full_output$recruitment_status=="even" & 
                               full_output$sigma_11==0.5,]

uni_sig22 <- unique(differing_ICC$sigma_22)
differing_ICC$Legend <- differing_ICC$sigma_22
differing_ICC$Legend[differing_ICC$sigma_22==uni_sig22[1] & differing_ICC$ICC_status=="common" ] <- "Same Surgeon Effect"
differing_ICC$Legend[differing_ICC$sigma_22==uni_sig22[1] & differing_ICC$ICC_status=="Higher ICC in Exp" ] <- "Same Magnitude"
differing_ICC$Legend[differing_ICC$sigma_22==uni_sig22[2]] <- "Moderate Increase"
differing_ICC$Legend[differing_ICC$sigma_22==uni_sig22[3]] <- "Large Increase"

plot_uneven_corr <- plot_power(differing_ICC)


pdf(paste0(output_dir,"even_scaling_ICC",".pdf"),width=width_dim,height=height_dim)
plot_uneven_corr$p_full + 
  ggtitle("Different ICC - Even Recruitment - Varying ICC") +
  guides(linetype =  "none",colour="none" )
dev.off()

legend_tmp <- g_legend(plot_uneven_corr$p_full) 
grid.newpage()
pdf(paste0(output_dir,"legend_2.pdf"),width=2*width_dim,height=1)
grid.draw(legend_tmp)
dev.off()


##GRAPH 4 - Different ICC - Uneven Recruitment - Varying ICC
differing_ICC_2 <- full_output[(full_output$ICC_status=="common" |full_output$ICC_status=="Higher ICC in Exp") & 
                                 full_output$recruitment_status=="even" & 
                                 full_output$sigma_11==0.5,]

uni_sig22 <- unique(differing_ICC$sigma_22)
differing_ICC_2$Legend[differing_ICC_2$sigma_22==uni_sig22[1] & differing_ICC$ICC_status=="common"] <- "Same Surgeon Effect"
differing_ICC_2$Legend[differing_ICC_2$sigma_22==uni_sig22[1] & differing_ICC$ICC_status=="Higher ICC in Exp"] <- "Same Magnitude"
differing_ICC_2$Legend[differing_ICC_2$sigma_22==uni_sig22[2]] <- "Moderate Increase"
differing_ICC_2$Legend[differing_ICC_2$sigma_22==uni_sig22[3]] <- "Large Increase"

plot_uneven_corr_uneven_rec <- plot_power(differing_ICC_2)
plot_uneven_corr_uneven_rec$p_full <- plot_uneven_corr_uneven_rec$p_full + labs(color='Correlation') 

pdf(paste0(output_dir,"uneven_scaling_ICC",".pdf"),width=width_dim,height=height_dim)
plot_uneven_corr_uneven_rec$p_full + 
  ggtitle("Different ICC - Uneven Recruitment - Varying ICC") +
  guides(linetype =  "none",colour="none" )
dev.off()


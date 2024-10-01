##Use: 
# Rscript 1-100 1 
##from shell to run

input_arg <- as.numeric(commandArgs(TRUE))
source("<path_to_functions_script/functions.R")
library("lme4")
output_path = "/nobackup/medcvy/Output/"


##more efficient to group jobs
scenario_indicator <- (3*(input_arg[1]-1)+1):(3*(input_arg[1]))

par_data <- read.csv("<path_to_scenario_list/scenario_list.csv",header=TRUE)

print("running simulations for these rows")
print(scenario_indicator)

if (input_arg[2]!=1){
  
  print("flagged as not to run, please check inputs")
  
}

n_trials <- 3000

output <- matrix(0,nrow=300,ncol=4)

for(row_number in scenario_indicator){

  run_seed <- row_number*1000 

  set.seed(run_seed)
  
  print(paste0("scenario: ",row_number))

  for(i in 1:n_trials){
    
    data <- simulate_data(
      df=par_data,
      number_surgeons=40,
      count=row_number
    )
    
    
    model_out <- glmer(data$formula,data$data,family="binomial")
    is_single <- 1*isSingular(model_out)
    is_sig <- 1*(summary(model_out)$coefficients[2,4] <0.05)
    
    
    output[row_number,1] <- output[row_number,1] +1
    output[row_number,2] <- output[row_number,2] +is_single 
    output[row_number,4] <- output[row_number,4] +is_sig
    
    if(is_single==1){
      output[row_number,3] <- output[row_number,3] +is_sig
    }
    
  }
 saveRDS(output,file=paste0(output_path,"ICTMC_2024_output_",row_number,".RDS"))
 
}

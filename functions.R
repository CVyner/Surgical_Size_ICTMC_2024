library("mvtnorm")

##utility functions
logit <- function(p){
  log(p/(1-p))
}

logistic <- function(x){
  
  1/(1+exp(-x))
  
}

simulate_data <- function(df,number_surgeons=40,count){
  
  data <- df[df$count==count,]
  n_sample_size <- data$n_sample_size
  null_p <- data$null_p
  alt_p <- data$alt_p
  recruitment_status <- data$recruitment_status
  ICC_status <- data$ICC_status
  sigma_11 <- data$sigma_11
  sigma_12 <- data$sigma_12
  sigma_22 <- data$sigma_22
  
  null_p_logit <- logit(null_p)
  alt_p_logit <- logit(alt_p)
  
  ##simulate recruitment
  if(recruitment_status=="even"){
    
    group_ID <- rep(1:number_surgeons,length.out=n_sample_size)
    
  }else if(recruitment_status=="random"){
    
    ##force at least one per surgeon, even after
    group_ID <- c(1:number_surgeons,sample(x=1:number_surgeons, 
                                           size=n_sample_size-number_surgeons,
                                           replace=TRUE,
                                           prob = rep(1/number_surgeons,number_surgeons)
    ))
    
    
  }else if(recruitment_status=="highly uneven"){
    group_ID <- c(1:number_surgeons,sample(x=1:number_surgeons, 
                                           size=n_sample_size-number_surgeons,
                                           replace=TRUE,
                                           prob = c(rep(0.5/5,5),rep(0.5/(number_surgeons-5),number_surgeons-5)
      
    )))
  }
  
  alpha_per_surgeon <- rmvnorm(number_surgeons,mean=c(0,0),sigma=matrix(c(sigma_11,sigma_12,sigma_12,sigma_22),ncol=2))
  alpha_observed <- alpha_per_surgeon[group_ID,]
  
  ##randomise
  trt_allocation <- rep(0:1,length.out=n_sample_size)[sample(1:n_sample_size,size=n_sample_size,replace = FALSE)]
  
  ##linear predictor - logit scale
  lp <- logit(trt_allocation*alt_p+ (1-trt_allocation)*null_p)
  
  ##add random effects
  if(ICC_status=="common"){
    
    lp <- lp + alpha_observed[,1]
      
  
  }else{
    exp_ind <- trt_allocation == 1
    cntl_ind <- !exp_ind
    
    
    lp[exp_ind] <- lp[exp_ind] + alpha_observed[exp_ind,2]
    
    lp[cntl_ind] <- lp[cntl_ind] + alpha_observed[cntl_ind,1]
  }
  ##convert lp to probabilites
  probs <- logistic(lp)
    
  ##simulate response
  response <- rbinom(n_sample_size,1,probs)  
  
  ##create formula
  if(ICC_status=="common"){
    glm_form <- formula(response~as.factor(trt_allocation) + (1 |group_ID))
  }else{
    glm_form <- formula(response~as.factor(trt_allocation) + (as.factor(trt_allocation) | group_ID))
  }
  
  
  invisible(
    list(
      formula=glm_form,
      data=data.frame(
        response=response,
        trt_allocation=trt_allocation,
        group_ID=group_ID
      )
    )
  )
}


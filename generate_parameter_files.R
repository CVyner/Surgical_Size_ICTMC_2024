##generate paramter file list

generate_data_frame <- function(count,n_sample_size,null_p,alt_p,recruitment_status,ICC_status,sigma_11,sigma_22,sigma_12){
  

  invisible(
    data.frame(count=count,
               n_sample_size=n_sample_size,
               null_p=null_p,
               alt_p=alt_p,
               recruitment_status=recruitment_status,
               ICC_status=ICC_status,
               sigma_11=sigma_11,
               sigma_22=sigma_22,
               sigma_12=sigma_12)
  )
  
}

count <- 1

set_sigma <- function(ICC,p){
  invisible(
    ICC*sqrt(p*(1-p))
  )
}

##initialise dataframe

df<-generate_data_frame(
  numeric(0),
  numeric(0),
  numeric(0),
  numeric(0),
  character(0),
  character(0),
  numeric(0),
  numeric(0),
  numeric(0)
)



for(recruitment_status in c("even","random","highly uneven")){

  for( n_sample_size in seq(150,600,by=50)){
    
    
    ##common, but varying ICC
    ICC_status = "common"
    for( Sigma_all in c(0.1,0.5,1,2)){
        
      
        null_p <- 0.25
        alt_p <- 0.125
      
        corr <- 0
        
        sigma_11 <- Sigma_all
        sigma_22 <- Sigma_all
        sigma_12 <- corr*sqrt(sigma_11)*sqrt(sigma_22)
        
        df <- rbind(df,generate_data_frame(
          count=count,
          n_sample_size=n_sample_size,
          null_p=null_p,
          alt_p=alt_p,
          recruitment_status=recruitment_status,
          ICC_status=ICC_status,
          sigma_11=sigma_11,
          sigma_22=sigma_22,
          sigma_12=sigma_12))
        count <- count +1
    }
    
    ##varying correlation - same ICC
    ICC_status = "differing correlation same effect"
    
    sigma_11 <- 0.5
    sigma_22 <- 0.5
      
      for( corr in c(0.05,0.5,0.85)){
      
        sigma_12 <- corr*sqrt(sigma_11)*sqrt(sigma_22)
        
        df <- rbind(df,generate_data_frame(
          count=count,
          n_sample_size=n_sample_size,
          null_p=null_p,
          alt_p=alt_p,
          recruitment_status=recruitment_status,
          ICC_status=ICC_status,
          sigma_11=sigma_11,
          sigma_22=sigma_22,
          sigma_12=sigma_12))
        count <- count+1
    }
    
    
    ##Higher ICC in exp
    ICC_status <- "Higher ICC in Exp"
    sigma_11 <- 0.5
    corr <- 0.5
    for(sigma_22 in c(c(0.5,1,2))){

        
      sigma_12 <- corr*sqrt(sigma_11)*sqrt(sigma_22)
        
        df <- rbind(df,generate_data_frame(
          count=count,
          n_sample_size=n_sample_size,
          null_p=null_p,
          alt_p=alt_p,
          recruitment_status=recruitment_status,
          ICC_status=ICC_status,
          sigma_11=sigma_11,
          sigma_22=sigma_22,
          sigma_12=sigma_12))
        
        count <- count+1
        
    }
    
      
  }
}

write.csv(df,file="U:/ICTMC/scenario_list.csv")


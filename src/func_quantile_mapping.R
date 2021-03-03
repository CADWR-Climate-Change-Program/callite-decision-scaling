# Quantile-mapping for CalLite streamflow 
# UMASS HydroSystems Research Group

qMapCalLite <- function(flowSimNode,flowHistNode,flowCalLiteSubnode,mainNodes,subNodes) {
  
  dssin_qmapped <- vector("list", length = length(flowSimNode))

  for (run in 1:length(flowSimNode)) { #Set the simulation number
    
    pb <- txtProgressBar(min = 0, max = length(mainNodes), style = 3)
    
    q_val_subnode_mainnode <- list(length(mainNodes))
    for (imain in 1:length(mainNodes)) { #Set the mainnode we're talking about
      
      # Observed flow for a main node	
      mainnode_data <- flowHistNode[,imain+2]
      
      # Simulated flow for a main node
      mainnode_sim <- flowSimNode[[run]][,imain+2]
      
      q_val_subnode <- list(length(subNodes[[imain]]))
      for (isub in 1:length(subNodes[[imain]])) { #Set the subnode we're talking about
        
        subnode_data <- flowCalLiteSubnode[,as.character(subNodes[[imain]][isub])]
        
        parhat_main <- array(NA,c(12,2))
        parhat_sub  <- array(NA,c(12,2))
        
        mainnode_mondata_mon <- vector("list", length = 12)
        eprob_main_mon <- vector("list", length = 12)
        tprob_main_mon <- vector("list", length = 12)
        subnode_mondata_mon <- vector("list", length = 12)
        eprob_sub_mon <- vector("list", length = 12)
        tprob_sub_mon <- vector("list", length = 12)

        for (imon in 1:12) { #Create empirical and theoretical prob distributions for each month
          
          # Weibul empirical & theoritical prob for MAIN NODE monthly observation
          mainnode_mondata <- mainnode_data[which(flowHistNode[,2]==imon)]
          
          eprob_main <- seq(1,length(mainnode_mondata),1)/(1+length(mainnode_mondata))
          
          parhat1 <- (mean(mainnode_mondata)/std(mainnode_mondata))^2 #shape parameter 
          parhat2 <- var(mainnode_mondata)/mean(mainnode_mondata) #scale parameter
          tprob_main  <- pgamma(sort(unique(mainnode_mondata)),parhat1,scale=parhat2,log.p=FALSE)
          
          parhat_main[imon,1] <- parhat1
          parhat_main[imon,2] <- parhat2
          
          mainnode_mondata_mon[[imon]] <- mainnode_mondata
          eprob_main_mon[[imon]] <- eprob_main
          tprob_main_mon[[imon]] <- tprob_main
          
          # Weibul empirical & theoritical prob for SUB NODE monthly observation
          subnode_mondata <- subnode_data[which(flowCalLiteSubnode[,2]==imon)]
          
          eprob_sub <- seq(1,length(subnode_mondata),1)/(1+length(subnode_mondata))
          
          parhat1 <- (mean(subnode_mondata)/std(subnode_mondata))^2 #shape parameter 
          parhat2 <- var(subnode_mondata)/mean(subnode_mondata) #scale parameter
          tprob_sub <- pgamma(sort(unique(subnode_mondata)), parhat1,scale = parhat2, log.p = FALSE)
          
          parhat_sub[imon,1] <- parhat1
          parhat_sub[imon,2] <- parhat2
          
          subnode_mondata_mon[[imon]] <- subnode_mondata
          eprob_sub_mon[[imon]] <- eprob_sub
          tprob_sub_mon[[imon]] <- tprob_sub #not currently used in qmapping
          
        }
        
        #QUANTILE MAP BETWEEN THAT MAINNODE, THAT SUBNODE, IN THAT MONTH
        quantile_val_subnode <- array(0,length(mainnode_sim))
        
        for (j in 1:length(mainnode_sim)) {
          
          selmon <- flowSimNode[[run]][j,2]
          selval <- mainnode_sim[j]
          
          subnode_mondata_selmon <- subnode_mondata_mon[[selmon]]
          mainnode_mondata_selmon <- mainnode_mondata_mon[[selmon]]
          eprob_main_selmon <- eprob_main_mon[[selmon]]
          tprob_main_selmon <- tprob_main_mon[[selmon]]
          eprob_sub_selmon <- eprob_sub_mon[[selmon]]
          
          if (length(unique(subnode_mondata_selmon)) == 1) {    #all same value
            
            quantile_val <- unique(subnode_mondata_selmon)
            
          } else {
            
            #if in the historic range, use weibull empirical distribution
            if (selval <= max(mainnode_mondata_selmon) && selval >= min(mainnode_mondata_selmon)) {
              
              #if there is more than 1 mainnode selection with the same value, which one do you quantile
              #map to? they are stacked on top of each other. So pick the exceedence prob randomly in the 
              #appropriate range.
              if (sum(mainnode_mondata_selmon == selval) > 1) {
                sortdata <- sort(mainnode_mondata_selmon)
                mvalind  <- which(sortdata == selval)
                randind  <- sample(mvalind, size=1, replace = FALSE, prob = NULL)
                nonexprob_main <- eprob_main_selmon[randind]
              } else {
                tryCatch(
                  expr = nonexprob_main <- 
                    suppressWarnings(interp1(sort(mainnode_mondata_selmon),eprob_main_selmon,selval,method = 'linear')),
                  error = function(e) {
                    print('interpolate error:')
                    print(mainnode_mondata_selmon)
                  }
                )
              }
            
            #if the mainnode selection is above the historic range, then use the upper end of the gamma distribution
            } else if (selval > max(mainnode_mondata_selmon)) {
              
              cdf_selval <- pgamma(selval, parhat_main[selmon,1],scale = parhat_main[selmon,2], log.p = FALSE)
              cdf_maxval <- tail(tprob_main_selmon,n=1)
              cdf_delta <- cdf_selval - cdf_maxval
              nonexprob_main <- min(tail(eprob_main_selmon,n=1) + cdf_delta,0.9999)
            
            #if the mainnode selection is below the historic range, then use the lower end of the gamma distribution
            } else if (selval < min(mainnode_mondata_selmon)) {
              
              cdf_selval <- pgamma(selval, parhat_main[selmon,1],scale = parhat_main[selmon,2], log.p = FALSE)
              cdf_minval <- tprob_main_selmon[1]
              cdf_delta <- cdf_minval - cdf_selval;
              nonexprob_main <- max(eprob_main_selmon[1] - cdf_delta,0.0001)
              
            }
            
            #having processed the mainnodes, move on to the subnodes - similar process
            #if you're in the historic range of eprobs of the subnode, then interpolate between historic values
            if (nonexprob_main <= max(eprob_sub_selmon) && nonexprob_main >= min(eprob_sub_selmon)) {
              
              sortdata <- sort(subnode_mondata_selmon)
              quantile_val <- suppressWarnings(interp1(eprob_sub_selmon,sortdata,nonexprob_main,method = 'linear'))
              
            } else {  #if you're outside of the historic range of eprobs, quantile map on the gamma distribution
              
              if (!is.nan(parhat_sub[selmon,1])) {
                
                if (nonexprob_main > max(eprob_sub_selmon)) {
                  extval1 = qgamma(nonexprob_main,parhat_sub[selmon,1],scale=parhat_sub[selmon,2])
                  extval2 = qgamma(tail(eprob_sub_selmon,n=1),parhat_sub[selmon,1],scale=parhat_sub[selmon,2])                          			
                  val_delta = extval1 - extval2
                  quantile_val = max(subnode_mondata_selmon) + val_delta
                } else if (nonexprob_main < min(eprob_sub_selmon)) {
                  extval1 = qgamma(nonexprob_main,parhat_sub[selmon,1],scale=parhat_sub[selmon,2])
                  extval2 = qgamma(eprob_sub_selmon[1],parhat_sub[selmon,1],scale=parhat_sub[selmon,2])                          			
                  val_delta = extval2 - extval1
                  quantile_val = min(subnode_mondata_selmon) - val_delta
                }
                
              } else {
                
                if (nonexprob_main > max(eprob_sub_selmon)) {
                  extval1 = qgamma(nonexprob_main,parhat_main[selmon,1],scale=parhat_main[selmon,2])
                  extval2 = qgamma(tail(eprob_sub_selmon,n=1),parhat_main[selmon,1],scale=parhat_main[selmon,2])                          			
                  val_delta = extval1 - extval2
                  quantile_val = (1 + val_delta/max(mainnode_mondata_selmon)) * max(subnode_mondata_selmon)
                } else if (nonexprob_main < min(eprob_sub_selmon)) {
                  extval1 = qgamma(nonexprob_main,parhat_main[selmon,1],scale=parhat_main[selmon,2])
                  extval2 = qgamma(eprob_sub_selmon[1],parhat_main[selmon,1],scale=parhat_main[selmon,2])                          			
                  val_delta = extval2 - extval1
                  if (min(subnode_mondata_selmon) >= 0) {
                    quantile_val = (1 - val_delta/min(mainnode_mondata_selmon)) * min(subnode_mondata_selmon)
                  } else {
                    quantile_val = (1 + val_delta/min(mainnode_mondata_selmon)) * min(subnode_mondata_selmon)
                  }
                }
              }
            }
          }
          
          #The timeseries of quantile mapped flows for subnode in 1 weather generator run
          quantile_val_subnode[j] <- quantile_val
                  
        } #the quantile mapped flows for subnode in 1 weather generator run, j
        
        q_val_subnode[[isub]] <- as.data.frame(quantile_val_subnode)
        colnames(q_val_subnode[[isub]]) <- as.character(subNodes[[imain]][isub])
    
        #new_main_sub_corr <- cor(quantile_val_subnode,mainnode_sim)
        #Have to be careful that the new data and old data have the same dimensions
        #old_main_sub_corr <- cor(subnode_data[-c(1,968:984)],mainnode_data)
        
        # figure
        #plot(1:length(subnode_data),subnode_data,type="l",xlim = c(1,(length(subnode_data)+length(quantile_val_subnode))))
        #lines((length(subnode_data)+1):(length(subnode_data)+length(quantile_val_subnode)),quantile_val_subnode,,col="red")
        
      } #the subnode number, isub
      
      q_val_subnode_mainnode[[imain]] <- bind_cols(q_val_subnode)
      
      setTxtProgressBar(pb,imain)

    } #the mainnode number, imain
    close(pb)
    q_val_subnode_mainnode_all <- bind_cols(q_val_subnode_mainnode)
    
    q_val_subnode_mainnode_all_out <- cbind(year=flowSimNode[[run]][,1],month=flowSimNode[[run]][,2],q_val_subnode_mainnode_all)
    
    dssin_qmapped[[run]] <- q_val_subnode_mainnode_all_out
    
  }
  

  return(dssin_qmapped)

}








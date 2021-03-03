library(openxlsx)
library(tidyr)
library(dplyr)
library(ggplot2)
library(zoo)
library(mvtnorm)
library(png)
library(gridExtra)
library(reshape2)

# settings 
filter_GCMs <- FALSE # filter by a GCM list (e.g. CCTAG-20)
temp_increment <- 0.5
precip_increment <- 10
sd_increment <- 10

# plotting probability
climate_period <- 55
prob_interval_count <- 100
prob_levels <- c(0.68,.95)
prob_plot_title <- paste0("Projected Range of Likely Climate Changes by ", climate_period+1995)

# directories
dir_GCM <- file.path("../data/gcm-projection-summary-worksheets/")
dir_GCM_bc <- file.path("../data/gcm-cvs-bc/")
families_GCM_list <- file.path("../data/gcm-families.csv")
filter_GCM_list <- file.path("../data/cctagGCM.csv")

# load files
gcm_summary_worksheets <- list.files(dir_GCM)
gcm_families <- read.csv(families_GCM_list)
gcm_families <- gcm_families %>% mutate(GCM = toupper(GCM)) # uppercase GCM names

# set increments
Precip <- as.numeric(seq(-30, 30, by=precip_increment))
Temp <- seq(0, 4, by=temp_increment) 
SD <- as.numeric(seq(-50, 50, by= sd_increment))


#############################################################################################################################
##################################### LOAD AND PROCESS DATA #################################################################
#############################################################################################################################


########### LOAD GCM PROCESSED FOR DWR VA - ONLY INCLUDES MEAN T and P CHANGES ##############################################
# Load historical
P_hist45 <- read.xlsx(paste0(dir_GCM, gcm_summary_worksheets[1]), sheet= "pr_hist", startRow= 1, cols=c(1:13)) # units are mm
T_hist45 <- read.xlsx(paste0(dir_GCM, gcm_summary_worksheets[1]), sheet= "tas_hist", startRow= 1, cols=c(1:13))
P_hist85 <- read.xlsx(paste0(dir_GCM, gcm_summary_worksheets[2]), sheet= "pr_hist", startRow= 1, cols=c(1:13)) # units are mm
T_hist85 <- read.xlsx(paste0(dir_GCM, gcm_summary_worksheets[2]), sheet= "tas_hist", startRow= 1, cols=c(1:13))
P_hist45$Total <- rowSums(P_hist45[,-1])
T_hist45$Total <- rowMeans(T_hist45[,-1])
P_hist85$Total <- rowSums(P_hist85[,-1])
T_hist85$Total <- rowMeans(T_hist85[,-1])
# read in changes in P and T for each year (30 year climate windows)
GCM_data <- data.frame()
for (i in 1:length(gcm_summary_worksheets)) {
  name <- strsplit(gcm_summary_worksheets[i], "_")[[1]][5]
  RCP <- sub(".xlsx", "", strsplit(gcm_summary_worksheets[i], "_")[[1]][6])
  PRdata <- read.xlsx(paste0(dir_GCM, gcm_summary_worksheets[i]), sheet= "pr_fut", startRow= 1, cols=c(1:13))
  TASdata <- read.xlsx(paste0(dir_GCM, gcm_summary_worksheets[i]), sheet= "tas_fut", startRow= 1, cols=c(1:13))
  PRdata$Total <- rowSums(PRdata[, -1])
  TASdata$Total <- rowMeans(TASdata[, -1])
  T_histdata <- if(RCP=="rcp45"){T_hist45}else{T_hist85}
  P_histdata <- if(RCP=="rcp45"){P_hist45}else{P_hist85}
  PRdata$D_pr <- (PRdata$Total-P_histdata$Total)/P_histdata$Total*100
  TASdata$D_tas <- TASdata$Total-T_histdata$Total
  deltas <- full_join(PRdata[, c("GCM", "D_pr")], TASdata[, c("GCM", "D_tas")], by="GCM")
  deltas$RCP <- RCP
  deltas$Year <- name
  GCM_data <- rbind(GCM_data, deltas)
}
GCM_data <- GCM_data[complete.cases(GCM_data),]
GCM_data <- GCM_data %>% mutate(GCM = toupper(GCM))



########### LOAD GCM BC DATASET - INCLUDES MONTHLY PR (AND TAS) AND CAN BE USED FOR CALCULATION OF STD.DEV. ################
pr <- read.csv(file.path(dir_GCM_bc,"pr_SpatialStat_mean.csv"), header= F)
t <- read.csv(file.path(dir_GCM_bc,"tas_SpatialStat_mean.csv"), header= F)
models <- read.table(file.path(dir_GCM_bc,"Projections5.txt"), header= F)
# hardcoded list of models to include - these are complete sets of RCP 4.5 and 8.5 for the 1st GCM realization
modelCols <- c(seq(1,8),9,14,19,24,29,30,31,34,37,38,39,40,45,55,66,69,
            seq(71,74),76,79,seq(82,87),seq(96,99),100,104,
            108,109,110,114,seq(118,127),128,131,seq(134,141))
rcp45 <- seq(1,length(modelCols),by=2)
rcp85 <- seq(2,length(modelCols),by=2)
models <- models[modelCols,]
pr <- pr[,c(1,2,modelCols+2)]
colnames(pr) <- c('y','m',as.character(models))
t <- t[,c(1,2,modelCols+2)]
colnames(t) <- c('y','m',as.character(models))
# calc baseline historical climate means and sd
pr_hist <- pr %>% filter(y>=1981 & y<=2010) %>% group_by(y) %>% summarise_all("sum") %>% summarise_all("mean") 
pr_clim <- as.data.frame(t(as.vector(pr_hist[1,c(seq(3,length(models)+2))])))
colnames(pr_clim) <- 'HIST(1981-2010)'
pr_sd_hist <- pr %>% filter(y>=1981 & y<=2010) %>% group_by(y) %>% summarise_all("sum") %>% summarise_all("sd")
pr_sd_clim <- as.data.frame(t(as.vector(pr_sd_hist[1,c(seq(3,length(models)+2))])))
colnames(pr_sd_clim) <- 'HIST(1981-2010)'
t_hist <- t %>% filter(y>=1981 & y<=2010) %>% group_by(y) %>% summarise_all("mean") %>% summarise_all("mean")
t_clim <- as.data.frame(t(as.vector(t_hist[1,c(seq(3,length(models)+2))])))
colnames(t_clim) <- 'HIST(1981-2010)'
# calc climate period Tmean, Pmean, and Psd
for (year in seq(1982,2070)) {
    pr_fut_clim <- pr %>% filter(y>=year & y<=year+29) %>% group_by(y) %>% summarise_all("sum") %>% summarise_all("mean") 
    pr_fut_clim <- as.data.frame(t(as.vector(pr_fut_clim[1,c(seq(3,length(models)+2))])))
    colnames(pr_fut_clim) <- paste0('FUT(',year,'-',year+29,')')
    pr_clim <- cbind(pr_clim,pr_fut_clim)
    
    pr_sd_fut_clim <- pr %>% filter(y>=year & y<=year+29) %>% group_by(y) %>% summarise_all("sum") %>% summarise_all("sd")
    pr_sd_fut_clim <- as.data.frame(t(as.vector(pr_sd_fut_clim[1,c(seq(3,length(models)+2))])))
    colnames(pr_sd_fut_clim) <- paste0('FUT(',year,'-',year+29,')')
    pr_sd_clim <- cbind(pr_sd_clim,pr_sd_fut_clim)
    
    t_fut_clim <- t %>% filter(y>=year & y<=year+29) %>% group_by(y) %>% summarise_all("mean") %>% summarise_all("mean") 
    t_fut_clim <- as.data.frame(t(as.vector(t_fut_clim[1,c(seq(3,length(models)+2))])))
    colnames(t_fut_clim) <- paste0('FUT(',year,'-',year+29,')')
    t_clim <- cbind(t_clim,t_fut_clim)
}
# calc coefficient of variation
pr_cv_clim <- (pr_sd_clim / pr_clim) * 100
# calc deltas between climate periods and historical (whis is in the first column of the dataframes)
pr_fut_clim_delta <- ((pr_clim - pr_clim[,1]) / pr_clim[,1]) *100
pr_cv_fut_clim_delta <- ((pr_cv_clim - pr_cv_clim[,1]) / pr_cv_clim[,1]) *100
t_fut_clim_delta <- t_clim - t_clim[,1]
# calc statistics (used in plotting)
rcp_clim_trends <- function(climate_delta_dataframe,rcp_columns) {
    climate_trend_rcp <- cbind(seq(1996,2085),
      as.data.frame(apply(t(climate_delta_dataframe)[,c(rcp_columns)],1,mean)),
      as.data.frame(apply(t(climate_delta_dataframe)[,c(rcp_columns)],1,sd)),
      as.data.frame(apply(t(climate_delta_dataframe)[,c(rcp_columns)],1,max)),
      as.data.frame(apply(t(climate_delta_dataframe)[,c(rcp_columns)],1,min)))
    colnames(climate_trend_rcp) <- c('period','mean','sd','max','min')
    return(climate_trend_rcp)
  }
pr_cv_fut_clim_delta_trend_rcp45 <- rcp_clim_trends(pr_cv_fut_clim_delta,rcp45)
pr_cv_fut_clim_delta_trend_rcp85 <- rcp_clim_trends(pr_cv_fut_clim_delta,rcp85)
pr_fut_clim_delta_trend_rcp45 <- rcp_clim_trends(pr_fut_clim_delta,rcp45)
pr_fut_clim_delta_trend_rcp85 <- rcp_clim_trends(pr_fut_clim_delta,rcp85)
t_fut_clim_delta_trend_rcp45 <- rcp_clim_trends(t_fut_clim_delta,rcp45)
t_fut_clim_delta_trend_rcp85 <- rcp_clim_trends(t_fut_clim_delta,rcp85)
# melt dataframes to flatten results
pr_cv_fut_clim_delta.rcp45.melt <- cbind(rep(seq(1996,2085), times = 33),melt(t(pr_cv_fut_clim_delta)[,c(rcp45)]))
pr_cv_fut_clim_delta.rcp85.melt <- cbind(rep(seq(1996,2085), times = 33),melt(t(pr_cv_fut_clim_delta)[,c(rcp85)]))
colnames(pr_cv_fut_clim_delta.rcp45.melt) <- c('period','period.name','model','value')
colnames(pr_cv_fut_clim_delta.rcp85.melt) <- c('period','period.name','model','value')
pr_fut_clim_delta.rcp45.melt <- cbind(rep(seq(1996,2085), times = 33),melt(t(pr_fut_clim_delta)[,c(rcp45)]))
pr_fut_clim_delta.rcp85.melt <- cbind(rep(seq(1996,2085), times = 33),melt(t(pr_fut_clim_delta)[,c(rcp85)]))
colnames(pr_fut_clim_delta.rcp45.melt) <- c('period','period.name','model','value')
colnames(pr_fut_clim_delta.rcp85.melt) <- c('period','period.name','model','value')
t_fut_clim_delta.rcp45.melt <- cbind(rep(seq(1996,2085), times = 33),melt(t(t_fut_clim_delta)[,c(rcp45)]))
t_fut_clim_delta.rcp85.melt <- cbind(rep(seq(1996,2085), times = 33),melt(t(t_fut_clim_delta)[,c(rcp85)]))
colnames(t_fut_clim_delta.rcp45.melt) <- c('period','period.name','model','value')
colnames(t_fut_clim_delta.rcp85.melt) <- c('period','period.name','model','value')
# prepare pr-cv results for joining on original VA GCM trends
pr_cv_fut_clim_delta.rcp45.melt <- pr_cv_fut_clim_delta.rcp45.melt %>% separate(model, c("GCM", "r", "RCP"),sep = "([\\.])")
pr_cv_fut_clim_delta.rcp85.melt <- pr_cv_fut_clim_delta.rcp85.melt %>% separate(model, c("GCM", "r", "RCP"),sep = "([\\.])")
pr_cv_fut_clim_delta.melt <- rbind(pr_cv_fut_clim_delta.rcp45.melt,pr_cv_fut_clim_delta.rcp85.melt)
pr_cv_fut_clim_delta.melt <- pr_cv_fut_clim_delta.melt %>% mutate(GCM = toupper(GCM))
colnames(pr_cv_fut_clim_delta.melt) <- c("period","Year","GCM","r","RCP","D_cv")



########### JOIN GCM DATASETS ##############################################################################################
# join pr-cv data to GCM_data (from VA)
GCM_data <- inner_join(GCM_data, subset(pr_cv_fut_clim_delta.melt,r ==1), by=c("GCM","RCP","Year"))
# filter down to GCMs in list if specified
if (filter_GCMs==TRUE) {
  filter_GCMs <- read.csv(filter_GCM_list, header= F)
  GCM_data <- subset(GCM_data, GCM %in% filter_GCMs$V1)
}
# calculate mean of GCM families to remove model family bias
GCMs_dataframe_family <- GCM_data %>% 
  left_join(y=gcm_families, by="GCM") %>%
  group_by(GCM.Family, RCP, Year) %>%
  summarise(DT=mean(D_tas), DP=mean(D_pr), DSD=mean(D_cv))




#############################################################################################################################
##################################### PROBABILITIES #########################################################################
#############################################################################################################################

# dt dp dv
gcm_mean <- GCMs_dataframe_family[,3:6] %>%
  group_by(Year) %>%
  summarise_all(.,funs(mean))
sigs <- list()
Years <- list()
for(i in 1: length(unique(GCMs_dataframe_family$Year)))
{
  Years[[i]] = subset(GCMs_dataframe_family, Year==unique(GCMs_dataframe_family$Year[i]))
  sigs[[i]]=cov(cbind("DTsig"= Years[[i]]["DT"],"DPsig"= Years[[i]]["DP"],"DSDsig"= Years[[i]]["DSD"]))
}
stress_test_grid <- expand.grid(Temp,Precip,SD)
norm_probs_normalize.dtdpdsd <- list()
for(j in 1:length(unique(GCMs_dataframe_family$Year)))
{
  norm_probs <- dmvnorm(stress_test_grid,mean=as.numeric(gcm_mean[j,2:4]),sigma=sigs[[j]])
  norm_probs_normalize.dtdpdsd[[j]] <- cbind(stress_test_grid,norm_probs/sum(norm_probs))
  colnames(norm_probs_normalize.dtdpdsd[[j]]) <- c("T_lev","P_lev","SD_lev","Biv_Norm_Prob")
  norm_probs_normalize.dtdpdsd[[j]]$period<- GCMs_dataframe_family$Year[[j]]
}
saveRDS(norm_probs_normalize.dtdpdsd, file= "../processed/biv_norm_values_dt-dp-dsd.RDS")

## DT DSD ##
gcm_mean <- GCMs_dataframe_family[,c(3,4,6)] %>%
  group_by(Year) %>%
  summarise_all(.,funs(mean))
sigs <- list()
Years <- list()
for(i in 1: length(unique(GCMs_dataframe_family$Year)))
{
  Years[[i]] = subset(GCMs_dataframe_family, Year==unique(GCMs_dataframe_family$Year[i]))
  sigs[[i]]=cov(cbind("DTsig"= Years[[i]]["DT"],"DSDsig"= Years[[i]]["DSD"]))
}
stress_test_grid <- expand.grid(Temp,SD)
norm_probs_normalize.dtdsd <- list()
for(j in 1:length(unique(GCMs_dataframe_family$Year)))
{
  norm_probs <- dmvnorm(stress_test_grid,mean=as.numeric(gcm_mean[j,2:3]),sigma=sigs[[j]])
  norm_probs_normalize.dtdsd[[j]] <- cbind(stress_test_grid,norm_probs/sum(norm_probs))
  colnames(norm_probs_normalize.dtdsd[[j]]) <- c("T_lev","SD_lev","Biv_Norm_Prob")
  norm_probs_normalize.dtdsd[[j]]$period<- GCMs_dataframe_family$Year[[j]]
}
saveRDS(norm_probs_normalize.dtdsd, file= "../processed/biv_norm_values_dt-dsd.RDS")

## DT DP ##
gcm_mean <- GCMs_dataframe_family[,c(3:5)] %>%
  group_by(Year) %>%
  summarise_all(.,funs(mean))
sigs <- list()
Years <- list()
for(i in 1: length(unique(GCMs_dataframe_family$Year)))
{
  Years[[i]] = subset(GCMs_dataframe_family, Year==unique(GCMs_dataframe_family$Year[i]))
  sigs[[i]]=cov(cbind("DTsig"= Years[[i]]["DT"],"DPsig"= Years[[i]]["DP"]))
}
stress_test_grid <- expand.grid(Temp,Precip)
norm_probs_normalize.dtdp <- list()
for(j in 1:length(unique(GCMs_dataframe_family$Year)))
{
  norm_probs <- dmvnorm(stress_test_grid,mean=as.numeric(gcm_mean[j,2:3]),sigma=sigs[[j]])
  norm_probs_normalize.dtdp[[j]] <- cbind(stress_test_grid,norm_probs/sum(norm_probs))
  colnames(norm_probs_normalize.dtdp[[j]]) <- c("T_lev","P_lev","Biv_Norm_Prob")
  norm_probs_normalize.dtdp[[j]]$period<- GCMs_dataframe_family$Year[[j]]
}
saveRDS(norm_probs_normalize.dtdp, file= "../processed/biv_norm_values_dt-dp.RDS")



#############################################################################################################################
##################################### PLOTTING  PROBABILITIES ###############################################################
#############################################################################################################################
mylevel <- seq(0,1, length.out = prob_interval_count)^5
mycolors <- c(colorRampPalette(c("white", "#c5e2f0","dark blue"))(prob_interval_count)[-prob_interval_count])

plotRCP45 <- subset(GCM_data, RCP=="rcp45" & period==1995+climate_period)
plotRCP85 <- subset(GCM_data, RCP=="rcp85" & period==1995+climate_period)

my.filled.contour <- function (x = seq(0, 1, length.out = nrow(z)), 
                               y = seq(0, 1, length.out = ncol(z)), z, 
                               xlim = range(x, finite = TRUE),
                               ylim = range(y, finite = TRUE),
                               zlim = range(z, finite = TRUE),
                               levels = pretty(zlim, nlevels), nlevels = 20, 
                               color.palette = cm.colors,
                               col = color.palette(length(levels) - 1), 
                               plot.title, plot.axes, key.title, key.axes, 
                               asp = NA, xaxs = "i", yaxs = "i", las = 1,
                               axes = TRUE, frame.plot = axes, ...){
  if (missing(z)) { if (!missing(x)) { if (is.list(x)) {
    z <- x$z
    y <- x$y
    x <- x$x }
    else {
      z <- x
      x <- seq.int(0, 1, length.out = nrow(z))
    } } else stop("no 'z' matrix specified")}
  else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  
  if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
    stop("increasing 'x' and 'y' values expected")
  
  mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
  on.exit(par(par.orig))
  par(las = las)
  mar <- mar.orig
  mar[4L] <- mar[2L]
  mar[2L] <- 1
  par(mar = mar)
  plot.new()
  plot.window(xlim = c(0,1), ylim = range(levels), xaxs = "i", 
              yaxs = "i")
  if (!missing(key.title)) 
    key.title
  mar <- mar.orig
  mar[4L] <- 1
  par(mar = mar)
  plot.new()
  plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
  .filled.contour(x, y, z, levels, col)
  if (missing(plot.axes)) {
    if (axes) {
      title(main = "", xlab = "", ylab = "")
      Axis(x, side = 1)
      Axis(y, side = 2)
    }
  }
  else plot.axes
  if (frame.plot) 
    box()
  if (missing(plot.title)) 
    title(...)
  else plot.title
  invisible()
}

# dt dsd
norm_probs_normalize.dtdsd.period <- norm_probs_normalize.dtdsd[[climate_period]]
norm_probs_normalize.dtdsd.period <- norm_probs_normalize.dtdsd.period[order(norm_probs_normalize.dtdsd.period$Biv_Norm_Prob),]
norm_probs_normalize.dtdsd.period$area <- cumsum(norm_probs_normalize.dtdsd.period$Biv_Norm_Prob)

plotGCMs_dataframe_family <- subset(GCMs_dataframe_family, Year==norm_probs_normalize.dtdsd[[climate_period]]$period[1])

EvalData_stress_matrix.dtdsd <- norm_probs_normalize.dtdsd.period[c("T_lev", "SD_lev", "area")] %>% spread(key=T_lev,value="area") 
rownames(EvalData_stress_matrix.dtdsd) <- EvalData_stress_matrix.dtdsd$SD_lev
EvalData_stress_matrix.dtdsd <- EvalData_stress_matrix.dtdsd[,-1]

png(paste0("../figures/biv-norm-prob-dt-dsd.png"), width = 4500,height = 3800, res=600)
prob_plot <- my.filled.contour(x=SD, y=Temp, z=as.matrix(EvalData_stress_matrix.dtdsd),
   levels=mylevel, col=mycolors, plot.axes={
     points(y=plotRCP45$D_tas, x=plotRCP45$D_cv, pch=20, col='blue');
     points(y=plotRCP85$D_tas, x=plotRCP85$D_cv, pch=20, col='red');
     # points(y=plotGCMs_dataframe_family$DT, x=plotGCMs_dataframe_family$DSD, pch=10, col='green');
     contour(x=SD, y=Temp, z=as.matrix(1-EvalData_stress_matrix.dtdsd), 
             add=TRUE, lwd=2.5, labcex=1,
             levels=prob_levels, col='dark blue',
             plot.axes= contour(SD, Temp, z=1-EvalData_stress_matrix.dtdsd));
     {axis(1);axis(2)}},
   main=prob_plot_title, 
   xlab="Change in CV of Precipitation (Percent Change from Historical Average)", 
   ylab="Change in Temperature (C)")
print(prob_plot)
dev.off()

# dt dp
norm_probs_normalize.dtdp.period <- norm_probs_normalize.dtdp[[climate_period]]
norm_probs_normalize.dtdp.period <- norm_probs_normalize.dtdp.period[order(norm_probs_normalize.dtdp.period$Biv_Norm_Prob),]
norm_probs_normalize.dtdp.period$area <- cumsum(norm_probs_normalize.dtdp.period$Biv_Norm_Prob)

EvalData_stress_matrix.dtdp <- norm_probs_normalize.dtdp.period[c("T_lev", "P_lev", "area")] %>% spread(key=T_lev,value="area") 
rownames(EvalData_stress_matrix.dtdp) <- EvalData_stress_matrix.dtdp$P_lev
EvalData_stress_matrix.dtdp<- EvalData_stress_matrix.dtdp[,-1]

png(paste0("../figures/biv-norm-prob-dt-dp.png"), width = 4500,height = 3800, res=600)
prob_plot <- my.filled.contour(x=Precip, y=Temp, z=as.matrix(EvalData_stress_matrix.dtdp),
   levels=mylevel, col=mycolors, plot.axes={
     points(y=plotRCP45$D_tas, x=plotRCP45$D_pr, pch=20);
     points(y=plotRCP85$D_tas, x=plotRCP85$D_pr, pch=20);
     # points(y=plotGCMs_dataframe_family$DT, x=plotGCMs_dataframe_family$DP, pch=10, col='green');
     contour(x=Precip, y=Temp, z=as.matrix(1-EvalData_stress_matrix.dtdp), 
             add=TRUE, lwd=2.5, labcex=1,
             levels=prob_levels, col='dark blue',
             plot.axes= contour(Precip, Temp, z=1-EvalData_stress_matrix.dtdp));
     {axis(1);axis(2)}},
   main=prob_plot_title, 
   xlab="Change in Precipitation (Percent Change from Historical Average)", 
   ylab="Change in Temperature (C)",xlim=c(-30, 30))
print(prob_plot)
dev.off()


#############################################################################################################################
##################################### PLOTTING  DT-DP-DSD TRENDS ############################################################
#############################################################################################################################
# plot climate trends from GCM BC dataset
ggplot() +
    geom_point(data=pr_cv_fut_clim_delta.rcp45.melt, aes(x=period,y=value,color='blue'),alpha=0.3,size=0.3) +
    geom_line(data=pr_cv_fut_clim_delta_trend_rcp45, aes(x=period,y=mean,color='blue')) +
    geom_ribbon(data=pr_cv_fut_clim_delta_trend_rcp45, aes(x=period, ymin=mean-2*sd, ymax=mean+2*sd), fill='blue', alpha=0.3) +
    geom_line(data=pr_cv_fut_clim_delta_trend_rcp85, aes(x=period,y=mean,color='red')) +
    geom_point(data=pr_cv_fut_clim_delta.rcp85.melt, aes(x=period,y=value,color='red'),alpha=0.3,size=0.3) +
    geom_ribbon(data=pr_cv_fut_clim_delta_trend_rcp85, aes(x=period, ymin=mean-2*sd, ymax=mean+2*sd), fill='red', alpha=0.2) +
    geom_hline(yintercept=0) +
    ggtitle('Percent Change in Coefficient of Variation of Annual Precipitation\nin 32 GCMs Spatially Averaged over the Delta Catchment') +
    labs(colour="RCP Set",
         x='30-Year Period Center',
         y='Percent Change in Coefficient of Variation of Precipitation')+
    theme(legend.position = c(0, 1),legend.justification = c(0, 1))+
    scale_color_manual(labels = c("RCP4.5","RCP8.5"), values=c('blue','red'))  +
    scale_x_continuous(breaks=seq(1995,2085,10))
ggsave("../figures/gcm-1996-2070-pr-cv-change-trend.png",dpi=600,scale=0.7)

ggplot() +
    geom_point(data=pr_fut_clim_delta.rcp45.melt, aes(x=period,y=value,color='blue'),alpha=0.3,size=0.3) +
    geom_point(data=pr_fut_clim_delta.rcp85.melt, aes(x=period,y=value,color='red'),alpha=0.3,size=0.3) +
    geom_line(data=pr_fut_clim_delta_trend_rcp45, aes(x=period,y=mean,color='blue')) +
    geom_ribbon(data=pr_fut_clim_delta_trend_rcp45, aes(x=period, ymin=mean-2*sd, ymax=mean+2*sd), fill='blue', alpha=0.3) +
    geom_line(data=pr_fut_clim_delta_trend_rcp85, aes(x=period,y=mean,color='red')) +
    geom_point(data=pr_fut_clim_delta.rcp45.melt, aes(x=period,y=value,color='blue'),alpha=0.3,size=0.4) +
    geom_ribbon(data=pr_fut_clim_delta_trend_rcp85, aes(x=period, ymin=mean-2*sd, ymax=mean+2*sd), fill='red', alpha=0.2) +
    geom_hline(yintercept=0) +
    ggtitle('Percent Change in Annual Precipitation\nin 32 GCMs Spatially Averaged over the Delta Catchment') +
    labs(colour="RCP Set",
         x='30-Year Period Center',
         y='Percent Change in Average Annual Precipitation')+
    theme(legend.position = c(0, 1),legend.justification = c(0, 1))+
    scale_color_manual(labels = c("RCP4.5","RCP8.5"), values=c('blue','red')) +
    scale_x_continuous(breaks=seq(1995,2085,10))
ggsave("../figures/gcm-1996-2070-pr-change-trend.png",dpi=600,scale=0.7)

ggplot() +
    geom_point(data=t_fut_clim_delta.rcp45.melt, aes(x=period,y=value,color='blue'),alpha=0.3,size=0.3) +
    geom_point(data=t_fut_clim_delta.rcp85.melt, aes(x=period,y=value,color='red'),alpha=0.3,size=0.3) +
    geom_line(data=t_fut_clim_delta_trend_rcp45, aes(x=period,y=mean,color='blue')) +
    geom_ribbon(data=t_fut_clim_delta_trend_rcp45, aes(x=period, ymin=mean-sd, ymax=mean+sd), fill='blue', alpha=0.3) +
    geom_line(data=t_fut_clim_delta_trend_rcp85, aes(x=period,y=mean,color='red')) +
    geom_ribbon(data=t_fut_clim_delta_trend_rcp85, aes(x=period, ymin=mean-sd, ymax=mean+sd), fill='red', alpha=0.2) +
    ggtitle('Change in Average Annual Temperature\nin 32 GCMs Spatially Averaged over the Delta Catchment') +
    labs(colour="RCP Set",
         x='30-Year Period Center',
         y='Temperature Change (C)')+
    theme(legend.position = c(0, 1),legend.justification = c(0, 1))+
    scale_color_manual(labels = c("RCP4.5","RCP8.5"), values=c('blue','red')) +
    ylim(-0.15,6) +
    scale_x_continuous(breaks=seq(1995,2085,10))
ggsave("../figures/gcm-1996-2070-tas-change-trend.png",dpi=600,scale=0.7)


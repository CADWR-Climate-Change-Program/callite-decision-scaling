getMetricAnnualData <- function(dbCon,metricsList,chooseMetric,dbTable,perturbY,perturbX) {
	metricDBVarName <- as.character(metricsList$dbTableVar[chooseMetric])
	dbPerturbX <- if(perturbX=='P_lev') {'(dp-1)*100 AS P_lev'} else if (perturbX=='SD_lev') {'(dsd-1)*100 AS SD_lev'}
	sql <- paste0('SELECT Water_Year, dt AS T_lev, ',dbPerturbX, ',',
		                metricDBVarName, ' AS Metric',
		                ' FROM ',dbTable,' WHERE S_OROVL_oct <> 0 ORDER BY T_lev, ', perturbX) 
	res <- dbSendQuery(dbCon,sql)
	data <- dbFetch(res, n=-1)
	return(data)
}

metricCDF <- function(evalData,bivNormVal,perturbX,perturbY,compYear=NULL) {

	evalDataCDF <- merge(evalData, bivNormVal[1:3], by=c(perturbY, perturbX))
	if (is.null(compYear)) {
		evalDataCDF$PerfPeriod <- "Current Conditions"
	} else {
		evalDataCDF$PerfPeriod <- paste0(compYear, " Conditions")
	}
	evalDataCDF <- group_by(evalDataCDF, Bin, BinAve, PerfPeriod) %>% 
	  summarize("Joint_Prob"=sum(Biv_Norm_Prob)/length(unique(evalData$Water_Year)))
	evalDataCDF$CumProb <- cumsum(evalDataCDF$Joint_Prob)
	evalDataCDF <- evalDataCDF[complete.cases(evalDataCDF),]

	return(evalDataCDF) 
}

plotCDFPDF <- function(evalDataFut,evalDataHist,chooseUnits,compYear,chooseMetric,metricsList) {

	if (chooseUnits==1) {
	  units <- metricsList$Units[chooseMetric]
	  unitName <- metricsList$UnitName[chooseMetric]
	} else {
	  units <- metricsList$UnitsI[chooseMetric]
	  unitName <- metricsList$UnitNameI[chooseMetric]
	}

	# CDF
	PlotCDF <- rbind(evalDataFut, evalDataHist)
	PlotCDF[c("Bin", "BinAve")] <- PlotCDF[c("Bin", "BinAve")]/units
	PlotCDF$PerfPeriod <- factor(PlotCDF$PerfPeriod, 
	                             levels= c("Current Conditions", 
	                             	paste0(compYear, " Conditions")))
	Probs <- data.frame("hist25"= numeric(18), "fut25"=numeric(18),
	                    "hist50"= numeric(18), "fut50"=numeric(18),
	                    "hist75"= numeric(18), "fut75"=numeric(18))
	row.names(Probs)[chooseMetric]<- as.character(metricsList$PerformanceMetric[chooseMetric])
	Probs$hist25[chooseMetric] <- signif(mean(evalDataHist$BinAve[
	  which(abs(evalDataHist$CumProb-.25)==min(abs(evalDataHist$CumProb-.25)))]/units), 3)
	Probs$hist50[chooseMetric] <- signif(mean(evalDataHist$BinAve[
	  which(abs(evalDataHist$CumProb-.5)==min(abs(evalDataHist$CumProb-.5)))]/units), 3)
	Probs$hist75[chooseMetric] <- signif(mean(evalDataHist$BinAve[
	  which(abs(evalDataHist$CumProb-.75)==min(abs(evalDataHist$CumProb-.75)))]/units), 3)
	Probs$fut25[chooseMetric] <- signif(mean(evalDataFut$BinAve[
	  which(abs(evalDataFut$CumProb-.25)==min(abs(evalDataFut$CumProb-.25)))]/units), 3)
	Probs$fut50[chooseMetric] <- signif(mean(evalDataFut$BinAve[
	  which(abs(evalDataFut$CumProb-.5)==min(abs(evalDataFut$CumProb-.5)))]/units), 3)
	Probs$fut75[chooseMetric] <- signif(mean(evalDataFut$BinAve[
	  which(abs(evalDataFut$CumProb-.75)==min(abs(evalDataFut$CumProb-.75)))]/units), 3)

	cdfTable=data.frame("P25"=c(Probs$hist25[chooseMetric], Probs$fut25[chooseMetric]),
	                 "P50"=c(Probs$hist50[chooseMetric], Probs$fut50[chooseMetric]),
	                 "P75"=c(Probs$hist75[chooseMetric], Probs$fut75[chooseMetric]))
	rownames(cdfTable)= c("Current Conditions", paste0(compYear, " Conditions"))

	plot.cdf <- (ggplot(PlotCDF, aes(BinAve, CumProb, color=PerfPeriod)) +
	               geom_line() + 
	               theme(plot.title = element_text(size = 14, face = "bold"),
	               			axis.text.x = element_text(angle = 60, hjust = 1)) +
	               xlab(paste0(metricsList$PerformanceMetric[chooseMetric], " (",unitName,")" )) + 
	               ylab("Non-Exceedance Probability") + 
	               scale_x_continuous(breaks = signif(seq(min(binAve/units), max(binAve/units), length.out = 10 ),3)) + 
	               scale_colour_manual(values=c("Dark Turquoise", "Red"), guide=guide_legend(title= NULL)) + 
	               geom_segment(x=min(binAve/units), xend=Probs$hist25[chooseMetric], y=.25, yend=.25, linetype="dashed", color= "black",size=0.3) +
	               geom_segment(x=min(binAve/units), xend=Probs$hist50[chooseMetric], y=.50, yend=.50, linetype="dashed", color= "black",size=0.3) +
	               geom_segment(x=min(binAve/units), xend=Probs$hist75[chooseMetric], y=.75, yend=.75, linetype="dashed", color= "black",size=0.3) +
	               geom_segment(x=Probs$hist25[chooseMetric], xend=Probs$hist25[chooseMetric], y=0, yend=.25, linetype="dashed",size=0.5) +
	               geom_segment(x=Probs$hist50[chooseMetric], xend=Probs$hist50[chooseMetric], y=.0, yend=.50, linetype="dashed",size=0.5) +
	               geom_segment(x=Probs$hist75[chooseMetric], xend=Probs$hist75[chooseMetric], y=0, yend=.75, linetype="dashed",size=0.5) +
	               geom_segment(x=Probs$fut25[chooseMetric], xend=Probs$fut25[chooseMetric], y=0, yend=.25, linetype="dashed", color= "red",size=0.3) +
	               geom_segment(x=Probs$fut50[chooseMetric], xend=Probs$fut50[chooseMetric], y=.0, yend=.50, linetype="dashed", color= "red",size=0.3) +
	               geom_segment(x=Probs$fut75[chooseMetric], xend=Probs$fut75[chooseMetric], y=0, yend=.75, linetype="dashed", color= "red",size=0.3))
	# PDF
	PlotPDF <- subset(PlotCDF, Joint_Prob>1e-6) %>%   
	   select(PerfPeriod, Bin, BinAve, CumProb, Joint_Prob) %>% 
	   group_by(PerfPeriod) %>% 
	   mutate("mins"= min(Joint_Prob), "maxs"= max(Joint_Prob))
	PlotPDF$factor<- as.integer(PlotPDF$Joint_Prob/PlotPDF$mins)

	evalDataFut.pdf <- subset(PlotPDF, PerfPeriod==paste0(compYear, " Conditions")) 
	evalDataFut.pdf <- as.data.frame(rep(evalDataFut.pdf$BinAve, evalDataFut.pdf$factor))
	colnames(evalDataFut.pdf) <- "BinAve"
	evalDataFut.pdf$Period <- paste0(compYear, " Conditions")

	evalDataHist.pdf <- subset(PlotPDF, PerfPeriod=="Current Conditions")
	evalDataHist.pdf <- as.data.frame(rep(evalDataHist.pdf$BinAve, evalDataHist.pdf$factor))
	colnames(evalDataHist.pdf) <- "BinAve"
	evalDataHist.pdf$Period <- "Current Conditions"

	evalData.pdf <- rbind(evalDataHist.pdf, evalDataFut.pdf)
	evalData.pdf$Period <-factor(evalData.pdf$Period, c("Current Conditions", paste0(compYear, " Conditions")))

	BW <- if (chooseUnits==1) {metricsList$BW[chooseMetric]} else {metricsList$BWI[chooseMetric]}
	BKDE <- as.data.frame(bkde(evalDataFut.pdf$BinAve, bandwidth=BW, gridsize=10000)) 
	BKDE$Period <- paste(compYear, "Conditions")
	BKDEhist <- as.data.frame(bkde(evalDataHist.pdf$BinAve, bandwidth=BW, gridsize=10000), 
	                          "Period"= "Current Conditions")
	BKDEhist$Period <- "Current Conditions"
	maxHist <- signif(BKDEhist$x[which(BKDEhist$y==max(BKDEhist$y))],3)
	maxFut <- signif(BKDE$x[which(BKDE$y==max(BKDE$y))],3)
	BKDEcomp <- bind_rows(BKDEhist, BKDE)
	BKDEcomp$Period <- factor(BKDEcomp$Period, levels= c("Current Conditions", paste0(compYear, " Conditions")))
	if (chooseMetric %in% c(1,2)) {BKDEcomp <- subset(BKDEcomp, x<=metrics[chooseMetric, 24+chooseUnits])}

	plot.pdf = (ggplot(BKDEcomp, aes(x,y, color= Period)) +
	              geom_line() + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
	              ylab("Probability Density") + 
	              scale_x_continuous(paste0(metricsList$PerformanceMetric[chooseMetric]," (", unitName,")"),
	              	breaks=signif(seq(min(binAve/units), max(binAve/units), length.out=10), 3)) +
	              scale_colour_manual(values=c("Dark Turquoise", "Red"), guide=guide_legend(title= NULL)) +
	              ggtitle(paste0(metricsList$PerformanceMetric[chooseMetric], "\nProbability Density")) +
	              geom_segment(x=maxHist, xend=maxHist, y=0, yend=max(BKDEhist$y), linetype="dashed", color= "turquoise") +
	              geom_segment(x=maxFut, xend=maxFut, y=0, yend=max(BKDE$y), linetype="dashed", color= "red"))
	plot.legend <- get_legend(plot.pdf)

	pdfTable = data.frame("Mode"=c(maxHist, maxFut))
	rownames(pdfTable) = c("Current Conditions", paste0(compYear, " Conditions"))

	return(list(plot.cdf, cdfTable, plot.pdf, pdfTable, plot.legend))
}

calcResponseMatrix <- function(evalData,perturbY,perturbX,statistic,percentile=NULL,count_threshold=0,year_count=1100) {

	if (statistic=='avg') {
		evalDataStat <- evalData %>% 
		  	group_by(get(perturbY),get(perturbX)) %>% 
		  	summarise(Metric=mean(Metric))
		colnames(evalDataStat) <- c(perturbY,perturbX,'Metric')
		evalDataStat <- as.data.frame(evalDataStat)
	} else if (statistic=='cv') {
		evalDataStat <- evalData %>% 
		  	group_by(get(perturbY),get(perturbX)) %>% 
		  	summarise(Metric=(sd(Metric)/mean(Metric))*100)
		colnames(evalDataStat) <- c(perturbY,perturbX,'Metric')
		evalDataStat <- as.data.frame(evalDataStat)
	} else if (statistic=='p') {
		evalDataStat <- evalData %>% 
		  	group_by(get(perturbY),get(perturbX)) %>% 
		  	summarise(Metric=quantile(Metric,percentile))
		colnames(evalDataStat) <- c(perturbY,perturbX,'Metric')
		evalDataStat <- as.data.frame(evalDataStat)
	} else if (statistic=='freq') {
		evalDataStat <- evalData %>% 
		  	group_by(get(perturbY),get(perturbX)) %>% 
		  	summarise(Metric=(sum(Metric>count_threshold)/year_count)*100)
		colnames(evalDataStat) <- c(perturbY,perturbX,'Metric')
		evalDataStat <- as.data.frame(evalDataStat)
	}

	# convert to matrix
	centerX <- ceiling(length(unique(evalData[,perturbX]))/2)
	metricMatrix <- evalDataStat %>% 
	    spread(key=get(perturbY), value=Metric) 
	metricMatrix <- as.matrix(metricMatrix[,-1])
	metricMatrix.AbsChange <- metricMatrix - metricMatrix[centerX,1]
	metricMatrix.PercentChange <- ((metricMatrix - metricMatrix[centerX,1]) / metricMatrix) * 100

	return(list(evalDataStat,metricMatrix,metricMatrix.AbsChange,metricMatrix.PercentChange))
}

plotResponseSurface <- function(responseMatrices,metricsList,chooseUnits,saveDir,
	contuorLevels,compYear,likelihoodSpace,xLevs,yLevs,statistic,perturbX) {

	absMatrix <- responseMatrices[[2]]
	absChangeMatrix <- responseMatrices[[3]]
	percentChangeMatrix <- responseMatrices[[4]]
	centerXlevs <- ceiling(length(xLevs)/2)

	plotTitle <- metricsList$PerformanceMetric[chooseMetric]
	legTitle<- metricsList$UnitName[chooseMetric]
	if (perturbX =='P_lev') {
		xlabel <- "Percent Change in Precipitation (%)"
	} else if (perturbX == 'SD_lev') {
		xlabel <- "Percent Change in Interannual Variance of Inflows (%)"
	}

	# Set density overlay params
	pLines <- 10
	probRange <- seq(0, .95, length.out = pLines)
	probColors <- c(colorRampPalette(c("blue","light blue"))(pLines)[-pLines])
	probLvls <- c(.68, .95)

	if (chooseUnits==1 & (statistic !='cv' & statistic !='freq')) {
	  units <- metricsList$Units[chooseMetric]
	} else if (chooseUnits==2 & (statistic !='cv' & statistic !='freq')) {
	  units <- metricsList$UnitsI[chooseMetric]
	} else {units <- 1}

	# absolute values
	if (statistic =='avg') {
		plotStat <- 'Average Annual'
	} else if (statistic =='cv') {
		plotStat <- 'Interannual Variance of'
		legTitle <- '\n\n%'
	} else if (statistic =='freq') {
		plotStat <- 'Frequency of'
		legTitle <- '\n\n%'
	} else {
		plotStat <- statistic
	}

	highest <- max(absMatrix/units, na.rm= TRUE)
	lowest <- min(absMatrix/units, na.rm= TRUE)
	center <- signif(absMatrix[centerXlevs,1]/units, 3)
	range <- max(abs(lowest-center), abs(highest-center))
	lowlvls <- seq(center + range * -1, center, length.out = ceiling(contuorLevels/2))
	highlvls <- seq(center, center + range, length.out = ceiling(contuorLevels/2))
	lvls <- round(c(lowlvls, highlvls[-1]),3)
	if (statistic != 'cv' & metricsList$Var[chooseMetric]!="SHORTAGE") {
		cols <- colorRampPalette(c("orangered4","darkorange",
			"floralwhite","deepskyblue", "royalblue4")) (contuorLevels - 1)
	} else {
		cols <- colorRampPalette(c("royalblue4","deepskyblue",
			"floralwhite","darkorange","orangered4")) (contuorLevels - 1)
	}
	graphData <- absMatrix/units

	png(file=file.path(saveDir,'Response',paste0(metricsList$filename[chooseMetric],"-RS-abs-",statistic,".png")),
		width=6480, height = 4880, res=600)
	par(mar=c(5,5,5,4))
	filled.contour(xLevs, yLevs, z=graphData,
	        levels= lvls, col = cols,labcex=0.7,drawlabels=T,
	        key.title=title(legTitle, cex.main=1.1),
	        main=paste(plotStat,plotTitle),cex.main=1.6,
	        xlab=xlabel, ylab="Change in Temperature (C)", cex.lab=1.4,
	        plot.axes={
	         	contour(xLevs, yLevs, z=graphData,
	         		level=c(center),add=T,lwd=2,labcex=1.2);
	         	contour(xLevs, yLevs, z=graphData,
	         		level=lvls[-ceiling(contuorLevels/2)], add=T, lwd=0, labcex=0.8);
	         	{axis(1);axis(2)}
	        })
	dev.off()

	if (statistic!='cv') {
		# percent change
	    legTitle <- "Percent\nChange"
		center <- 0
	    lvls <- seq(-100,100, length.out = 41)
	    if (statistic != 'cv' & metricsList$Var[chooseMetric]!="SHORTAGE") {
			cols <- colorRampPalette(c("orangered4","darkorange",
				"floralwhite","deepskyblue", "royalblue4")) (41 - 1)
		} else {
			cols <- colorRampPalette(c("royalblue4","deepskyblue",
				"floralwhite","darkorange","orangered4")) (41 - 1)
		}
	    graphData <- percentChangeMatrix

		png(file=file.path(saveDir,'Response',paste0(metricsList$filename[chooseMetric],"-RS-pc-",statistic,".png")),
			width=6480, height = 4880, res=600)
		par(mar=c(5,5,5,4))
		filled.contour(xLevs, yLevs, z=graphData,
		        levels= lvls, col = cols,
		        key.title=title(legTitle, cex.main=1.1),
		        main=paste('Percent Change in',plotStat,plotTitle),cex.main=1.6,
		        xlab=xlabel, ylab="Change in Temperature (C)", cex.lab=1.4,
		        plot.axes={
		         	contour(xLevs, yLevs, z=graphData,
		         		level=c(center),add=T,lwd=2,labcex=1.1);
		         	contour(xLevs, yLevs, z=graphData,
		         		level=lvls[-ceiling(41/2)], add=T, lwd=0, labcex=0.8);
		         	{axis(1);axis(2)}
		        })
	}
	dev.off()

	# plot.abs.gcm <- filled.contour(xLevs,yLevs,z=graphData,
 #           	levels= lvls, col = cols,
 #           	key.title=title(legTitle, cex.main=0.8),
 #           	main=paste(plotStat,plotTitle,"\nwith", compYear, "Conditional Probability Density"),
 #           	xlab=xlabel, ylab="Change in Temperature (C)", cex.lab=1.3,
 #           	plot.axes={
 #           		contour(xLevs, yLevs,z=graphData, level=c(center), add=T, lwd=2, labcex=1.1);
 #           		contour(xLevs, yLevs,z= as.matrix(likelihoodSpace), add= TRUE,
 #             	levels=probRange, col=probColors, drawlabels = F,
 #             	plot.axes= contour(xLevs, yLevs, z=graphdata));
 #            	contour(xLevs, yLevs, z= as.matrix(likelihoodSpace), add= TRUE,
 #             	levels=probLvls, col='darkblue', lwd=2, labcex=1, plot.axes= contour(xLevs, yLevs,z=graphdata));
 #            	{axis(1);axis(2)}
 #            })

	
}

saveCDFPDF <- function(metricsList,chooseMetric,chooseUnits,binAve,compYear,plots_cdf_pdf,saveDir) {
	# units
	if (chooseUnits==1) {
	  units <- metricsList$Units[chooseMetric]
	} else {
	  units <- metricsList$UnitsI[chooseMetric]
	}

	# annotation tables
	if (chooseMetric %in% c(3:6)) {
	  ymin=0
	  ymax=.125
	  xmin=min(binAve[45])/units
	  xmax= binAve[70]/units
	} else {
	  ymin=.875
	  ymax=1
	  xmin=binAve[8]/units
	  xmax= binAve[22]/units 
	}

	Table1form <- tableGrob(plots_cdf_pdf[[2]], 
		theme=ttheme_minimal(core= list(bg_params= list(fill="aliceblue")), 
			colhead= list(bg_params=list(fill= "aliceblue")),
			rowhead=list(bg_params=list(fill="aliceblue"))))

	cdf.annotation <- annotation_custom(Table1form, 
		xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)

	Table2form <- tableGrob(plots_cdf_pdf[[4]], 
		theme=ttheme_minimal(core= list(bg_params= list(fill="aliceblue")),
			colhead= list(bg_params=list(fill= "aliceblue")), 
			rowhead=list(bg_params=list(fill="aliceblue"))))

	pdf.annotation = annotation_custom(Table2form,
		xmin=metricsList$xmin[chooseMetric], 
		xmax=metricsList$xmax[chooseMetric],
		ymin=metricsList$ymin[chooseMetric],
		ymax=metricsList$ymax[chooseMetric])

	# figures prints
	png(file=paste0(saveDir,"/", metricsList$filename[chooseMetric],"_Perf_", compYear, "_CDF.png"), 
    	width=3210, height=2410, units= "px", res=300)
	print(plots_cdf_pdf[[1]] + 
		ggtitle(paste0(metricsList$PerformanceMetric[chooseMetric], "- \nCumulative Distribution")) + 
		cdf.annotation)
	dev.off()

	png(file=paste0(saveDir,"/", metricsList$filename[chooseMetric],"_Perf_", compYear, "_PDF.png"), 
	    width=3210, height=2410, units= "px", res=300)
	print(plots_cdf_pdf[[3]] + 
		ggtitle(paste0(metricsList$PerformanceMetric[chooseMetric], "- \nProbability Density")) +
		pdf.annotation)
	dev.off()

	png(file=paste0(saveDir,"/", metricsList$filename[chooseMetric],"_Perf_", compYear, "_v_I.png"), 
	    width=3210, height=2410, units= "px", res=300)
	print(grid.arrange(plots_cdf_pdf[[1]]+theme(legend.position= "none") + 
	                     ggtitle("a)       Cumulative Distribution") + 
	                     theme(axis.text.x = element_text(angle = 60, hjust = 1)), 
	                   plots_cdf_pdf[[3]]+theme(legend.position= "none") + 
	                     ggtitle("b)       Probability Density") + 
	                     theme(axis.text.x = element_text(angle = 60, hjust = 1)), 
	                   plots_cdf_pdf[[5]], Table1form, Table2form, 
	                   ncol=3, nrow=2, widths= c(4.6,4.6,1.6 ), heights= c(5, 1), 
	                   top=textGrob(paste0("Shift in ", metricsList$PerformanceMetric[chooseMetric],
	                   					" Current to ",compYear," Conditions"), 
	                   gp=gpar(fontsize= 18, fontface="bold"))))
	dev.off()
}

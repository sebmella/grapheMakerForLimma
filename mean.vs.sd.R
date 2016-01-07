plotMeanVsSd <- function(Elist, graphe.dir=""){
	require(ggplot2)
	require(limma)
	require(grid)
	
	data.name <- substitute(Elist)	
	
	#creating a data frame containing the expression values
	expDF <- data.frame(Elist$E)
	nc <- ncol(expDF)

		
	if(class(Elist)== "EListRaw"){
		
		#Calculating overall mean and sd
		expDF$mean <- apply(expDF[,1:eval(nc)], 1, mean)
		expDF$sd <- apply(expDF[,1:eval(nc)], 1, sd)
		
		#linear regression
		lm <- lm(sd~mean, data=expDF)
		R2 <- round(summary(lm)$adj.r.squared,3)
		slope <- round(lm$coefficients[2],3)
		cat("R2 = ", R2, "\n")
		cat("slope = ", slope, "\n")
		
		#ploting mean vs sd
		grob <- grobTree(textGrob(eval(parse(text=paste("'R2 = ", R2, "\nSlope = ", slope, "'", sep="")))))
		plot <- ggplot(expDF, aes(x=mean, y=sd))+geom_point(size=1)+stat_smooth(method='lm', colour="red", linetype="dashed")+stat_smooth(method='auto')+theme_minimal()+annotation_custom(grob)		
		#+annotate("text", label=eval(parse(text=paste("'R2 = ", R2,"\nslople = ", slope, "'", sep=""))))	
		
		print(plot)		
		
		#saving the graphe if graphe.dir specified
		if(graphe.dir!=""){
			graphe.name <- paste("p", data.name,"MeanVsSd", "png", sep=".")
			setwd(graphe.dir)
			png(eval(parse(text=paste("'", graphe.name,"'", sep=""))), width=300*5, height=300*5, res=300)
			print(plot)
			dev.off()
			
		}
		
	}
	
	else{
		cat("Sorry, this function can only works with 'EListRaw' type of file.")
	}
	
}
plotGroupMeanVsOverall <- function(Elist, graphe.dir=""){
	require(ggplot2)
	require(limma)
	
	data.name <- substitute(Elist)
	
	WTindex <- vector()
	KOindex <- vector()
	
	#creating a data frame containing the expression values
	expDF <- data.frame(Elist$E)
	nc <- ncol(expDF)
	
	if(class(Elist)== "EListRaw"){
		
		
		
		#finding index for the WT and KO in vector Elist$targets$Genotype
		WTindex <- which(Elist$targets$Genotype %in% "WT")
		KOindex <- which(Elist$targets$Genotype %in% "KO")
		
		cat("WTindex --> ", WTindex ,"\n")
		cat("KOindex --> ", KOindex ,"\n")
		
		#Calculating overall mean, WT mean, KO mean
		expDF$mean <- apply(expDF[,1:eval(nc)], 1, mean)
		expDF$meanWT <- apply(expDF[,WTindex], 1, mean)
		expDF$meanKO <- apply(expDF[,KOindex], 1, mean)		
		
		#ploting WT mean vs overall mean in black and KOmean vs overall mean in blue
		p.WT.KO.vs.overall <- ggplot(expDF, aes(x=mean, y=meanWT))+geom_point(size=1)+geom_point(aes(y=meanKO), size=1, colour="blue")+theme_minimal()+ylab("WTmean(black) - KOmean(blue)")	
		
		print(p.WT.KO.vs.overall)		
		
		#saving the graphe if graphe.dir specified
		if(graphe.dir!=""){
			graphe.name <- paste("p", data.name,"groupVSoverallMean", "png", sep=".")
			setwd(graphe.dir)
			png(eval(parse(text=paste("'", graphe.name,"'", sep=""))), width=300*5, height=300*5, res=300)
			print(p.WT.KO.vs.overall)
			dev.off()
			
		}
		
	}
	
	else{
		cat("Sorry, this function can only works with 'EListRaw' type of file.")
	}
	
}
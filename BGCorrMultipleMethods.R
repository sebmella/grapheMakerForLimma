BGCorrMultipleMethods <- function(data, graphe.dir=""){
	require(limma)
	require(ggplot2)
	require(plyr)
	
	methods <- c("none", "normexp", "subtract", "half", "movingmin", "minimum", "edwards")
	methods.mean <- paste(methods, "mean", sep=".")
	
	ppi <- 300
	
	BGCorrDataList <- list()
	BGCorrDataMeanList <- list()
	
	if(graphe.dir==""){cat('graphe.dir = NULL; graphe will not be saved\n')}
	
	
	if(class(data)== "EListRaw"){
		for(i in 1:length(methods)){
			cat("Method used for background correction = ", methods[i], "\n")
			BGCorrDataList[[i]] <- backgroundCorrect(data, method=eval(parse(text=paste("'", methods[i], "'", sep=""))))
			cat("Data corrected.\n\n")
			cat("\t------------ Calculating the mean... ------------\n")
			
			BGCorrDataMeanList[[i]] <- apply(data.frame(BGCorrDataList[[i]]$E)[,1:8],1,mean)
			
			cat("calcul done.\n\n")
		}
		
		names(BGCorrDataList) <- methods
		names(BGCorrDataMeanList) <- methods
		tempdf <- ldply(BGCorrDataMeanList, data.frame)
		names(tempdf) <- c("methods", "values")
		p <- ggplot(tempdf, aes(x=values, colour=as.factor(methods)))+geom_line(stat="density")+xlim(c(-40, 150))+theme_minimal()+scale_colour_discrete(name="Methods")
		print(p)
		
		if(graphe.dir!=""){
		cat("***********	saving graphe 'p.BGCorrMultpiple.png' there --> ", graphe.dir, "\t***********\n")
		setwd(graphe.dir)
		png("p.BGCorrMultpiple.png", width=8*ppi, height=5*ppi, res=ppi)
		print(p)
		dev.off()
		}
			
	}
	
	else cat("this function only works with class 'EListRaw' type of data.\n Your class of data is : '", class(data), "'\n", sep="")
	
}

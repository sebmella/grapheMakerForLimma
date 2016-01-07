 BGCorrMultipleNormexpMethods <- function(data, graphe.dir=""){
	require(limma)
	require(ggplot2)
	require(plyr)
	
	normexp.methods <- c( "saddle",  "mle",  "rma")
	normexp.methods.mean <- paste(methods, "mean", sep=".")
	
	normexpBGCorrDataList <- list()
	normexpBGCorrDataMeanList <- list()
	
	for(i in 1:length(normexp.methods)){
		cat("Background correction type 'normexp' with normexp.method= '", normexp.methods[i], "'\n", sep="")
		
		normexpBGCorrDataList[[i]] <- backgroundCorrect(data,method="normexp", normexp.method=eval(parse(text=paste("'", normexp.methods[i], "'", sep=""))))
		
		normexpBGCorrDataMeanList[[i]] <- apply(data.frame(normexpBGCorrDataList[[i]]$E)[,1:8],1,mean)		
	}
	
	names(normexpBGCorrDataList) <- normexp.methods
	names(normexpBGCorrDataMeanList) <- normexp.methods
	
	#return(normexpBGCorrDataMeanList)
	
	tempdf <- ldply(normexpBGCorrDataMeanList, data.frame)
	names(tempdf) <- c("methods", "values")
	p <- ggplot(tempdf, aes(x=values, colour=as.factor(methods)))+geom_line(stat="density")+xlim(c(-40, 150))+theme_minimal()+scale_colour_discrete(name="Methods")+scale_colour_brewer(palette=3)
	print(p)
	
	if(graphe.dir!=""){
		cat("***********	saving graphe 'p.BGCorrMultpipleNormexp.png' there --> ", graphe.dir, "\t***********\n")
		setwd(graphe.dir)
		png("p.BGCorrMultpipleNormexp.png", width=8*ppi, height=5*ppi, res=ppi)
		print(p)
		dev.off()
		}
}
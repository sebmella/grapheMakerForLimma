multipleColumnsBoxplot <- function(data, ncol=0, graphe.dir="", ppi=300, return=F){
	#loading required libraries
	require(ggplot2)
	
	if(class(data)!="data.frame") {
		cat('transforming data to data.frame\n')
		data <- data.frame(data)
		cat("class data = ", class(data), "\n")
		}
	
	#grabbing name of the data
	cat(quote(data), "\n")
	
	#
	colnames <- colnames(data)
	cat("\tcolnames =\n", colnames, "\n")
	
	#dealing with argument 'ncol'
	if(ncol==0) {
		cat("argument ncol=0\n...")
		ncol <- ncol(data)
		cat("now ncol= ", ncol, "\n")
		df <- data
		}
	else{
		cat("argument ncol!=0\t ncol = ", ncol, "\n")
		df <- eval(parse(text=paste("data[,c(1:",ncol,")]")))
	}
	
	#plotting the data	
	p <- ggplot(df, eval(parse(text=paste('aes(x=1, y=', colnames[1], ')'))))+geom_boxplot()
		
	for(i in 2: ncol){		
		cat(i, "\n")
		p <- p+geom_boxplot(eval(parse(text=paste('aes(x=',i,', y=', colnames[i], ')'))))
	}
	
	#removing unwanted texts on the graphe	
	p <- p+ylab("")+xlab("")+theme_minimal()
	
	#printing the plot
	print(p)
	
	#argument return
	if(return) return (p)
	
	#argument graphe.dir --> saving the file in the choosen directory
	if(graphe.dir!=""){
		namePlot <- paste("p", quote(data) ,"boxplot.png", sep=".")#pb with pasting 'data'!!!!
		cat("Saving plot '", namePlot ,"' in the following directory --> ", graphe.dir, "\n", sep="")
		setwd(graphe.dir)
		
		eval(parse(text=paste("png('" , namePlot , "', width=5*" , ppi , ",height=5*" , ppi , ",res=" , ppi , ")",sep="")))
		
		print(p)
		dev.off()
	}
	
	cat("end\n\n")
}
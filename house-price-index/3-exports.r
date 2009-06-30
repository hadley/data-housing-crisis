#Graphics


# Housing price index
	library(ggplot2)
	options(stringsAsFactors= FALSE)

hpi <- read.csv("HPI-metro-areas.csv")
time <- hpi[,"year"] + (hpi[,"quarter"] - 1) / 4
hpi <- cbind(hpi,time)
hpi$city_state <- paste(hpi$city, hpi$state, sep= ", ")


#Facets comparing all states
	statesAll <- qplot(x= time, y= hpi, data= hpi, colour = city, geom= "line", facets=~ state) + ylim(c(0,400)) + opts(legend.position = "none") 

#unique(each[each[,"hpi"] == max(each[,"hpi"]), "time"])

#Facets comparing states with apararent rise and fall
	extreme <- subset(hpi, state %in% c("FL", "CA", "CA  MSAD", "OR", "NV"))
	riseAndFall <- qplot(x= time, y= hpi, data= extreme, colour = city, geom= "line", facets=~ state) + ylim(c(0,400)) + opts(legend.position = "none") 
	
#Facets comparing states with significant rise and little fall

	constant <- subset(hpi, state %in% c("NY","NY  MSAD","NJ","NJ-PA  MSAD","MA","WA"))
	bigRise <- qplot(x= time, y= hpi, data= constant, colour = city, geom= "line", facets=~ state) + ylim(c(0,400)) + opts(legend.position = "none") 
	
	




#looking at highest and  more current HPI for every city

#unique(each[ each[,"hpi"] == max(each[,"hpi"]), "hpi"])


returnMaxTimeHPI <- function(d, maxcolumn)
	unique(d[d[,maxcolumn] == max(d[,maxcolumn]), c("hpi","time")])


MaxHPI <- ddply(hpi, .(state,city) , returnMaxTimeHPI, maxcolumn = "hpi")


HPI09 <- ddply(hpi[ hpi[,"year"] >= 2009 ,], .(state,city) , returnMaxTimeHPI, maxcolumn = "hpi")


names(HPI09)[3] <- "hpi_2009" 


maximum_hpi <- merge(MaxHPI, HPI09[,1:3])




maximum_hpi$percent_change <- maximum_hpi$hpi / maximum_hpi$hpi_2009 * 100 - 100

#findings: the housing bubble- cities whoes HPI hit a peak in 2006,changed the most by 2009

#ggplot(data= maximum_hpi, aes(x=time, y=change, label= state, colour=state)) + geom_point() + geom_text(angle=45, hjust= 0, size= 5) + opts(legend.position="none")

findmerced <- ggplot(data= maximum_hpi, aes(x=time, y=percent_change, label= substr(state, 1, 2), colour=state)) + geom_text( hjust = .5, vjust = .5, size= 5) + opts(legend.position="none", title = "Percent Change vs. Peak HPI Time")+ ylim(c(0,140))





#looking into merced

#subset(maximum_hpi, city %in% c("Merced"))

#subset(maximum_hpi, state %in% c("CA"))


	merced <- subset(hpi, city %in% c("Merced"))
	mercedMax <- data.frame(x = 2006.25, y = 321.29, label = "321.29")
	merced09 <- data.frame(x = 2009, y = 140.4, label = "140.4")
	mercedgraph <- qplot(x= time, y= hpi, data= merced, geom= "line", main = "Merced, CA") + 
	  ylim(c(100,350)) + 
	  opts(legend.position = "none") + 
	  geom_point(data = mercedMax, aes(x, y), size= 2) + 
	  geom_text(data = mercedMax, aes(x, y+5), label = "321.29", vjust = 0) + 
	  geom_point(data = merced09, aes(x, y), size= 2) + 
	  geom_text(data = merced09, aes(x, y+5), label = "140.4", vjust = 0)

CA <- subset(maximum_hpi, state %in% c("CA"))
	CAgraph <- ggplot(data= CA, aes(x=time, y=percent_change, label= city)) + geom_point(size = 2) + geom_text( hjust = 0, vjust = 0, size= 5) + opts(title = "State of California",legend.position="none") + xlim(c(2005,2009)) + ylim(c(0,140))
	


if(TRUE)
{
  print("printing")
  dir.create("exports")
  
#  pdf("exports/My Pretty Plots.pdf", width = 8, height = 6)
#  	print(statesAll)
#  	print(riseAndFall)
#  	print(bigRise)
#  dev.off()

  pdf("exports/Hpi Peak Time per State.pdf", width = 8, height = 6)
	 print(findmerced)
	dev.off()  
  
  pdf("exports/Merced.pdf", width = 8, height = 6)
	 print(mercedgraph)
	dev.off()	
	
  pdf("exports/California.pdf", width = 8, height = 6)
	 print(CAgraph)
	dev.off()	 
	
	pdf("exports/Messy Plot.pdf", width = 8, height = 6)
	 print(qplot(time, hpi, data = hpi, colour = city, label = city_state,  geom = c("path","point","text"), main = "HPI vs. Time") + opts(legend.position = "none"))
	dev.off()

	 
}
	 
		

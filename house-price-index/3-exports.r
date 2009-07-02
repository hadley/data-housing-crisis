#Graphics


# Housing price index
	library(ggplot2)
	options(stringsAsFactors= FALSE)

hpi <- read.csv("HPI-metro-areas.csv")
time <- hpi[,"year"] + hpi[,"quarter"] / 4 -.5
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
  write.csv(maximum_hpi, gzfile("Max HPI.csv.gz"), row.names = FALSE)
  closeAllConnections()

#findings: the housing bubble- cities whoes HPI hit a peak in 2006,changed the most by 2009

#ggplot(data= maximum_hpi, aes(x=time, y=change, label= state, colour=state)) + geom_point() + geom_text(angle=45, hjust= 0, size= 5) + opts(legend.position="none")

findmerced <- ggplot(data= maximum_hpi, aes(x=time, y=percent_change, label= substr(state, 1, 2), colour=state)) + geom_text( hjust = .5, vjust = .5, size= 5) + opts(legend.position="none", title = "Percent Change vs. Peak HPI Time")+ ylim(c(0,140))





#looking into merced

#subset(maximum_hpi, city %in% c("Merced"))

#subset(maximum_hpi, state %in% c("CA"))
	merced <- subset(hpi, city %in% c("Merced"))
	mercedMax <- data.frame(x = 2006.375, y = 321.29, label = "321.29")
	merced09 <- data.frame(x = 2009.125, y = 140.4, label = "140.4")


makeExplanation <- function(data )
{

  dummy <- data
  
  minHeight <- min(c(100,dummy$hpi))
  
  dummyMax <- data.frame(
    x = dummy[dummy$hpi == max(dummy$hpi), "time"], 
    y = max(dummy$hpi), 
    label = as.character(dummy$hpi),
    colour = I("blue")
  )
	
  dummy09 <- data.frame(
    x = max(dummy$time), 
    y = dummy[dummy$time == max(dummy$time), "hpi"], 
    label = as.character(dummy[dummy$time == max(dummy$time), "hpi"]),
    colour = I("red")
  )
  
  ratio <- format(max(dummyMax$y) / max(dummy09$y), digits = 3)
  
  dummyDiff <- data.frame(x = 2000, xtext = 2000.125, y1 = max(dummyMax$y), y2 = max(dummy09$y), text = paste("Peak HPI / Current HPI = ", ratio ,"\n(ratio) \n\n\nMeans that in it's maximum HPI year,\nit was ", ratio," times bigger\nthan it is today.", sep = "", collapse = ""), colour = I("darkgreen") )
  
	dummygraph <- qplot(x= time, y= hpi, data= dummy, geom= "blank", main = paste("Explanation Graph for ", dummy[1,"city"],", ",dummy[1,"state"], sep = "", collapse = ""), xlab = "Time", ylab = "HPI") + 
    geom_line(size = 2) +
	  xlim(c(2000, 2009)) + 
	  ylim(c(minHeight,350)) + 
	  opts(legend.position = "none") + 
	  geom_segment(data = dummyMax, aes(x = 2000, xend = x, y = y, yend = y), colour = dummyMax$colour, size= 1) + 
	  geom_segment(data = dummyMax, aes(x = x, xend = x, y = minHeight, yend = y), colour = dummyMax$colour, size= 1) + 
	  geom_text(data = dummyMax, aes((x - 2000) / 2 + 2000, y+2), colour = dummyMax$colour, label = "Peak HPI Value", vjust = 0, size = 3) + 
	  geom_text(data = dummyMax, aes(x - 1/8, (y - minHeight) * 3/4 + minHeight), colour = dummyMax$colour, label = "Peak HPI Time", vjust = 0, angle = 90, size = 3) + 

	  geom_text(data = dummy09, aes((x - 2000) / 3 + 2000, y+2), colour = dummy09$colour, label = "Current HPI Value", vjust = 0, size = 3) +
	  geom_text(data = dummy09, aes(x - 1/8, (y - minHeight)/ 2 + minHeight), colour = dummy09$colour, label = "Current Time", vjust = 0, angle = 90, size = 3) +
	  geom_segment(data = dummy09, aes(x = 2000, xend = x, y = y, yend = y), colour = dummy09$colour, size= 1) + 
	  geom_segment(data = dummy09, aes(x = x, xend = x, y = minHeight, yend = y), colour = dummy09$colour, size= 1) +
	  
#	  geom_segment(data = dummyDiff, aes(x = x, xend = x, y = y1, yend = y2), colour = dummyDiff$colour, size = 1) + 
	  geom_text(data = dummyDiff, aes(x = xtext , y = (y1 + y2) / 2, label = text), size = 3, hjust = 0)

  dummygraph
}


  StocktonExplain <- makeExplanation(subset(hpi, city %in% c("Stockton")))
  MercedExplain <- makeExplanation(subset(hpi, city %in% c("Merced")))
  HonoluluExplain <- makeExplanation(subset(hpi, city %in% c("Honolulu")))

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

	pdf("exports/Explanations.pdf", width = 8, height = 6)
	 print(StocktonExplain)
	 print(MercedExplain)
	 print(HonoluluExplain)
	dev.off()
	 
}
	 
		

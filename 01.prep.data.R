#drafted by Jeremy VanDerWal ( jjvanderwal@gmail.com ... www.jjvanderwal.com )
#GNU General Public License .. feel free to use / distribute ... no warranties

################################################################################
library(SDMTools); library(igraph) #load the library
wd = '/home/jc165798/working/Jessica.Cheok/'; setwd(wd) #set the workign directory

################################################################################
###do some trial works
################################################################################
wd = '/home/jc165798/working/Jessica.Cheok/Testing/'; setwd(wd) #set the workign directory
baseasc = as.asc(matrix(0,nrow=1000,ncol=1000)) #create an empty dataframe
pos = as.data.frame(which(is.finite(baseasc),arr.ind=TRUE)) #create the position matrix
pos$GUID = 1:nrow(pos) #create the grid ID
tasc = baseasc #create a copy of the ascii
counter = 1 #define the planning unit counter
for (ii in seq(2,999,by=3)) { cat(ii,'- ')
	for (jj in seq(2,999,by=3)) { 
		tasc[as.matrix(expand.grid((ii-1):(ii+1),(jj-1):(jj+1)))] = counter #setup the grid planning units
		counter = counter + 1 #increment the counter
	}; cat('\n')
}
pos$PU = tasc[cbind(pos$row,pos$col)] #extract the planning unit
pos = as.matrix(pos) #convert to a position matrix

write.asc(tasc,'base.asc')
write.csv(pos,'pos.csv',row.names=FALSE)


#now create the features & management units
areas = abs(round(rnorm(1000,4,2))) + 1 #make up some areas
baseasc[cbind(pos[,'row'],pos[,'col'])] = pos[,'GUID']
d1=nrow(baseasc); d2=ncol(baseasc) #get the dimensions
##get a neighbor list with weightings for the 8 neighbors
nblist = na.omit(cbind(start=as.vector(baseasc[,-d2]),end=as.vector(baseasc[,-1]),weight=1)) #+ 1 column
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[,-1]),end=as.vector(baseasc[,-d2]),weight=1))) #- 1 column
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[-d1,]),end=as.vector(baseasc[-1,]),weight=1))) #+1 row
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[-1,]),end=as.vector(baseasc[-d1,]),weight=1))) #- 1 row
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[-d1,-d2]),end=as.vector(baseasc[-1,-1]),weight=1.414214))) #+ 1 column, + 1 row
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[-d1,-1]),end=as.vector(baseasc[-1,-d2]),weight=1.414214))) #- 1 column, + 1 row
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[-1,-d2]),end=as.vector(baseasc[-d1,-1]),weight=1.414214))) #+ 1 column, - 1 row 
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[-1,-1]),end=as.vector(baseasc[-d1,-d2]),weight=1.414214))) #- 1 column, - 1 row
g <- graph.data.frame(as.data.frame(nblist), directed = FALSE); g = simplify(g) #create the simplified graph
save(g, file='graph.RData')


pos = cbind(pos,MU=0) #append the unique id's
counter = 1; pos[runif(1,1,nrow(pos)),'MU'] = 1 #setup the counter and the starting point
while (any(pos[,'MU']==0)) { cat(length(which(pos[,'MU']==0)),'\n') #cat('.') #keep doing this until ALL 0's are filled
	EXIT = 0
	tarea = areas[round(runif(1,1,length(areas)))]
	vois = pos[which(pos[,'MU']==counter),'GUID'] #get the uids for patch 1
	while(length(vois)<tarea) {
		tvois = unique(unlist(neighborhood(g,1,vois))) #get the unique neighbors for 1 pixel buffer -- 80 m -- 0.11 seconds
		tt = which(pos[tvois,'MU']==0) #get the positions that are still 0
		if (length(tt)==0) {
			if (length(which(pos[,'MU']==0))>0) {
				break()
			} else {
				EXIT=1;break()
			} #break if no more data
		}
		tt = sample(tt,ceiling(0.5*length(tt)))
		if (length(tt)+length(which(pos[,'MU']==counter))>tarea) {
			tt = sample(tt,tarea-length(which(pos[,'MU']==counter))) #keep only a subset to add upto the area
		}
		pos[tvois[tt],'MU'] = counter #set the data
		vois = pos[which(pos[,'MU']==counter),'UID'] #get the full vois
	}
	counter = counter + 1
	pos[sample(which(pos[,'MU']==0),1),'MU'] = counter #set up the next starting point
	if (EXIT==1) break()
} ; cat('\n')

pdf('start.fixed.pdf')
ttasc = tasc; ttasc[cbind(pos[,'row'],pos[,'col'])] = pos[,'MU']
image(ttasc)
dev.off()


################################################################################
################################################################################






### generate some trial data
tasc = read.asc('trial.asc'); tasc[which(is.finite(tasc))] = 0 #load in some trial data
areas = abs(round(rnorm(1000,length(which(is.finite(tasc)))/1000,length(which(is.finite(tasc)))/2500)))

#start populating data
pos = as.matrix(which(is.finite(tasc),arr.ind=TRUE)) #define the pos dataframe
pos = cbind(UID=(1:nrow(pos)),pos) #append the unique id's
baseasc = tasc; baseasc[pos[,c('row','col')]] = pos[,'UID'] #create a copy of the ascii matrix with the unique ids
d1=nrow(baseasc); d2=ncol(baseasc) #get the dimensions
##get a neighbor list with weightings for the 8 neighbors
nblist = na.omit(cbind(start=as.vector(baseasc[,-d2]),end=as.vector(baseasc[,-1]),weight=1)) #+ 1 column
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[,-1]),end=as.vector(baseasc[,-d2]),weight=1))) #- 1 column
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[-d1,]),end=as.vector(baseasc[-1,]),weight=1))) #+1 row
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[-1,]),end=as.vector(baseasc[-d1,]),weight=1))) #- 1 row
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[-d1,-d2]),end=as.vector(baseasc[-1,-1]),weight=1.414214))) #+ 1 column, + 1 row
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[-d1,-1]),end=as.vector(baseasc[-1,-d2]),weight=1.414214))) #- 1 column, + 1 row
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[-1,-d2]),end=as.vector(baseasc[-d1,-1]),weight=1.414214))) #+ 1 column, - 1 row 
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[-1,-1]),end=as.vector(baseasc[-d1,-d2]),weight=1.414214))) #- 1 column, - 1 row
g <- graph.data.frame(as.data.frame(nblist), directed = FALSE); g = simplify(g) #create the simplified graph

pos = cbind(pos,MID=0) #append the unique id's
counter = 1; pos[1,'MID'] = 1 #setup the counter and the starting point
while (any(pos[,'MID']==0)) { cat('.') #keep doing this until ALL 0's are filled
	EXIT = 0
	tarea = areas[round(runif(1,1,length(areas)))]
	vois = pos[which(pos[,'MID']==counter),'UID'] #get the uids for patch 1
	while(length(vois)<tarea) {
		tvois = unique(unlist(neighborhood(g,1,vois))) #get the unique neighbors for 1 pixel buffer -- 80 m -- 0.11 seconds
		tt = which(pos[tvois,'MID']==0) #get the positions that are still 0
		if (length(tt)==0) {
			if (length(which(pos[,'MID']==0))>0) {
				break()
			} else {
				EXIT=1;break()
			} #break if no more data
		}
		if (length(tt)+length(which(pos[,'MID']==counter))>tarea) {
			tt = sample(tt,tarea-length(which(pos[,'MID']==counter))) #keep only a subset to add upto the area
		}
		pos[tvois[tt],'MID'] = counter #set the data
		vois = pos[which(pos[,'MID']==counter),'UID'] #get the full vois
	}
	counter = counter + 1
	pos[which(pos[,'MID']==0)[1],'MID'] = counter #set up the next starting point
	if (EXIT==1) break()
} ; cat('\n')

pdf('start.fixed.pdf')
ttasc = tasc; ttasc[cbind(pos[,'row'],pos[,'col'])] = pos[,'MID']
image(ttasc)
dev.off()

#start populating data
pos = as.matrix(which(is.finite(tasc),arr.ind=TRUE)) #define the pos dataframe
pos = cbind(UID=(1:nrow(pos)),pos) #append the unique id's
baseasc = tasc; baseasc[pos[,c('row','col')]] = pos[,'UID'] #create a copy of the ascii matrix with the unique ids
d1=nrow(baseasc); d2=ncol(baseasc) #get the dimensions
##get a neighbor list with weightings for the 8 neighbors
nblist = na.omit(cbind(start=as.vector(baseasc[,-d2]),end=as.vector(baseasc[,-1]),weight=1)) #+ 1 column
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[,-1]),end=as.vector(baseasc[,-d2]),weight=1))) #- 1 column
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[-d1,]),end=as.vector(baseasc[-1,]),weight=1))) #+1 row
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[-1,]),end=as.vector(baseasc[-d1,]),weight=1))) #- 1 row
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[-d1,-d2]),end=as.vector(baseasc[-1,-1]),weight=1.414214))) #+ 1 column, + 1 row
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[-d1,-1]),end=as.vector(baseasc[-1,-d2]),weight=1.414214))) #- 1 column, + 1 row
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[-1,-d2]),end=as.vector(baseasc[-d1,-1]),weight=1.414214))) #+ 1 column, - 1 row 
nblist = rbind(nblist,na.omit(cbind(start=as.vector(baseasc[-1,-1]),end=as.vector(baseasc[-d1,-d2]),weight=1.414214))) #- 1 column, - 1 row
g <- graph.data.frame(as.data.frame(nblist), directed = FALSE); g = simplify(g) #create the simplified graph

pos = cbind(pos,MID=0) #append the unique id's
counter = 1; pos[sample(pos[1,'MID'],1),'MID'] = 1 #setup the counter and the starting point
while (any(pos[,'MID']==0)) { cat('.') #keep doing this until ALL 0's are filled
	EXIT = 0
	tarea = areas[round(runif(1,1,length(areas)))]
	vois = pos[which(pos[,'MID']==counter),'UID'] #get the uids for patch 1
	while(length(vois)<tarea) {
		tvois = unique(unlist(neighborhood(g,1,vois))) #get the unique neighbors for 1 pixel buffer -- 80 m -- 0.11 seconds
		tt = which(pos[tvois,'MID']==0) #get the positions that are still 0
		if (length(tt)==0) {
			if (length(which(pos[,'MID']==0))>0) {
				break()
			} else {
				EXIT=1;break()
			} #break if no more data
		}
		if (length(tt)+length(which(pos[,'MID']==counter))>tarea) {
			tt = sample(tt,tarea-length(which(pos[,'MID']==counter))) #keep only a subset to add upto the area
		}
		pos[tvois[tt],'MID'] = counter #set the data
		vois = pos[which(pos[,'MID']==counter),'UID'] #get the full vois
	}
	counter = counter + 1
	pos[sample(which(pos[,'MID']==0),1),'MID'] = counter #set up the next starting point
	if (EXIT==1) break()
} ; cat('\n')

pdf('start.random.pdf')
ttasc = tasc; ttasc[cbind(pos[,'row'],pos[,'col'])] = pos[,'MID']
image(ttasc)
dev.off()



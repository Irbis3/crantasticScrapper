
makeRegions = function(fileIN="multiChIPmixHMM-results.txt", fileOUT="regions.txt", gap=1)
{

data=read.table(fileIN,header=TRUE)

x=which(diff(which(data$status==1))<(gap+2))
y=which(data$status==1)[x]
z=which(data$status==1)[x+1]

a=unique(sort(c(y,z)))
b=diff(a)

pos.start=pos.stop=c()
pos=0
i=1
while(i <= length(b))
{
   if(b[i]==1)
   {
	pos=pos+1
	pos.start[pos]=a[i]
	i=i+1
	while(b[i]<(gap+1) & i<=length(b) )
	{ i= i+1}
	pos.stop[pos]=a[i]
   }
   i = i + 1
}

regions = data.frame(start=data$ID[pos.start], stop=data$ID[pos.stop])
write.table(regions, file=fileOUT, row.names=FALSE, sep="\t")
}



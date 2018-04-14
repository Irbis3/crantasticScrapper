txt = readLines('rosalind_1d.txt')
nums = as.numeric(strsplit(txt[2],' ')[[1]])
txt = txt[1]
k = nums[1]
L = nums[2]
t = nums[3]

n = nchar(txt)
kmer = substring(txt,1:(n-k+1),k:n)

dat = data.frame(kmer,ind=1:length(kmer),stringsAsFactors = F)
dat = dat[order(kmer),]

ans = NULL
head = 1
tail = 1
for (i in 2:length(kmer))
{
    if (dat[i,1]==dat[i-1,1])
        tail = tail+1
    if (dat[i,1]!=dat[i-1,1])
    {
        ind = dat[head:tail,2]
        if (length(ind)>=t)
        {
            flag = FALSE
            for (j in 1:(length(ind)-t+1))
                if (ind[j+t-1]-ind[j]<=L)
                    flag=TRUE
            if (flag)
                ans = c(ans,dat[head,1])
        }
        head = i
        tail = i   
    }
}

ans = paste(ans,collapse=' ')
writeLines(ans,'1d.txt')

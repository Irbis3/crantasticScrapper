hierClust =
  #
  #
  # The return value is a list containing
  #  the final clusters
  #  a sequence of the intermediate clusters from the beginning until the end of the process
  #  the distance used to merge the observation to the nearest cluster at each step
  #  a vector of observation identifiers giving which observation was merged at each step.
  #
function(D, linkage = min, maxSteps = Inf)
{
   if(length(dimnames(D)) == 0)
      dimnames(D) = list(1:nrow(D), 1:nrow(D))
   
#   clusters = lapply(1:nrow(D), function(x) x)

   nsteps = min(nrow(D) - 2, maxSteps)
   clusters = as.list(1:nrow(D))
   delta = numeric(nsteps)
   step = 1L
   groupSeq = vector("list", )
   who = character()
   while(step <= nsteps) {
      tmp = agglomerate(D, clusters, linkage)
      D = tmp$D
      clusters = tmp$clusters
      groupSeq[[step]] = clusters
      delta[step] = tmp$delta
      who[step] = tmp$observation
      names(who)[step] = tmp$clusterID
      step = step + 1L
   }
   list(clusters = clusters, sequence = groupSeq, height = delta, who = who) 
}


agglomerate =
  #
  # This is the function that does a single agglomeration
  # i.e. moving one observation into the nearest cluster.
  #
function(D, clusters, linkage = singleLinkage)
{
  m = min(D[D > 0])
  pos = which( D == m, arr.ind = TRUE)[1,]  # could do this directly w/o arr.ind

#  pos = sort(pos)
    # get the distances for the row
  d = D[pos[1], ]
  
  clusters[[pos[1]]] = c(  clusters[[pos[1]]], colnames(D)[pos[2]] )
  clusters = clusters[ - pos[2] ]

  id = colnames(D)[pos[2]]
  to = colnames(D)[pos[1]]
  tmp = applyLinkage(pos[1], pos[2], D, clusters, linkage) [ - pos ]
  i = setdiff(1:nrow(D), pos)
  D[pos[1], i] = tmp
  D[i, pos[1]] = tmp
  D = D[-pos[2], - pos[2] ]

  l = sapply(clusters, clusterLabel)
  dimnames(D) = list(l, l)

  list(D = D, clusters = clusters, delta = m,  observation = id, clusterID = to)
}

applyLinkage =
  #
  #  Given the row identifying the cluster to which the elemented identified by col
  # will be added, compute the new distances from this new cluster to all of the other 
  # elements in the distance matrix.
  #
  #  clusters isn't used.
  #
  # linkage is a function to compare collections of distances, e.g. min, max, mean
  #
function(row, col, ds, clusters, linkage)
{
   apply(ds[c(row, col), ], 2, linkage)
}

clusterLabel =
  # Compute the identifier for a cluster using its members labels
function(x)
  paste(sort(as.integer(x)), collapse = ",")



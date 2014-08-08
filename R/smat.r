# Make scoring matrices

calc_dists_dotprods<-function(querynl, targetnl, subset, ignoreSelf=TRUE, ...){
  if(missing(targetnl)) targetnl=querynl
  if(missing(subset)) {
    subset=expand.grid(query=names(targetnl), target=names(targetnl), stringsAsFactors = FALSE)
  } else {
    if(!is.data.frame(subset) || !all(sapply(s,is.character)))
      stop("subset must be a data.frame with two character columns specifying",
           " query and target neurons by name, with one row for each pair")
  }
  if(ignoreSelf)
    subset=subset(subset, target!=query)
  collect_one_pair<-function(query, target, ...)
    NeuriteBlast(querynl[[query]], targetnl[target], NNDistFun=list, ...)
  mlply(subset, collect_one_pair, ...)
}

makeprobmat<-function(nndists, dotprods, distbreaks, dotprodbreaks=seq(0, 1, by=0.1),
                      ReturnCounts=FALSE){
  if(missing(distbreaks)) distbreaks=c(0,0.75,1.5,2,2.5,3,3.5,4,5,6,7,8,9,10,12,14,16,20,25,30,40,500)
  if(missing(dotprods)){
    if(is.list(nndists) && length(nndists[[1]])==2){
      dotprods=sapply(nndists,"[[",2)
      nndists=sapply(nndists,"[[",1)
    } else
      stop("dotprods missing and unable to parse nndists as combined list")
  }

  countmat=table(cut(unlist(nndists),br=distbreaks),
                 cut(unlist(dotprods),dotprodbreaks))
  attr(countmat,"distbreaks")=distbreaks
  attr(countmat,"dotprodbreaks")=dotprodbreaks
  if(!ReturnCounts) countmat/sum(countmat)
  else countmat
}

scorematrix<-function(matchmat, randmat, logbase=2, fudgefac=1e-6, ...){

  distbreaks=attr(matchmat,"distbreaks")
  ndistbreaks=length(distbreaks)
  dotprodbreaks=attr(matchmat,"dotprodbreaks")
  ndpbins=length(dotprodbreaks)-1

  if(!isTRUE(all.equal(dim(randmat),dim(matchmat))))
    stop("Mismatch between match and mismatch dimensions")
  if(!isTRUE(all.equal(
    distbreaks[-ndistbreaks],distbreaks[-ndistbreaks],check.attributes=F)))
    stop("mismatch between distance breaks used for match and null models")

  log((matchmat+fudgefac)/(randmat+fudgefac),logbase)
}

#' Produce similarity score for neuron morphologies
#'
#' An updated version of the NBLAST algorithm that compares the morphology of a
#' neuron with those of a list of other neurons.
#' @param query the query neuron.
#' @param target a \code{\link[nat]{neuronlist}} to compare neuron against.
#'   Defaults to \code{options(nat.default.neuronlist)}.
#' @param targetBinds numeric indices or names with which to subset \code{target}.
#' @param Reverse whether to use \code{query} as the target neuron rather than query
#'   (default=FALSE).
#' @param ... extra arguments to pass to the distance function.
#' @return Named list of similarity scores.
#' @export
#' @seealso \code{\link{WeightedNNBasedLinesetMatching}}
nblast2 <- function(query, target, targetBinds=NULL, Reverse=FALSE, ...) UseMethod("nblast2")


nblast2.dotprops <- function(query, target=option(nat.default.neuronlist), targetBinds=NULL, Reverse=FALSE, ...){
  if(is.null(targetBinds))
    targetBinds=seq_along(target)
  else if(is.character(targetBinds))
    targetBinds=charmatch(targetBinds,names(target))
  scores=rep(NA,length(targetBinds))

  for(i in seq_along(targetBinds)){
    # targetBinds[i] is numeric index in target of current target neuron
    dbn=target[[targetBinds[i]]]
    if(!is.null(dbn)){
      if(Reverse)
        score=try(WeightedNNBasedLinesetMatching.dotprops(query,dbn,...))
      else
        score=try(WeightedNNBasedLinesetMatching.dotprops(dbn,query,...))
      if(!inherits(score,'try-error'))
        scores[i]=score
    }
  }
  names(scores)=names(target)[targetBinds]
  scores
}


WeightedNNBasedLinesetDistFun<-function(nndists,dotproducts,sd=3,...){
  summaryfun=function(x) 1-mean(sqrt(x),na.rm=T)
  sapply(sd,function(sd) summaryfun(dnorm(nndists,sd=sd)*dotproducts/dnorm(0,sd=sd)))
}

#' @rdname WeightedNNBasedLinesetMatching
#' @importFrom RANN nn2
WeightedNNBasedLinesetMatching <- function(...) UseMethod("WeightedNNBasedLinesetMatching")

#' Compute point & tangent vector similarity score between two dotprops objects
#'
#' UseAlpha determines whether the alpha values (eig1-eig2)/sum(eig1:3)
#' are passed on to WeightedNNBasedLinesetMatching. These will be used to scale
#' the dot products of the direction vectors for nearest neighbour pairs.
#' @param dp1,dp2 dotprops objects to compare.
#' @param UseAlpha Whether to scale dot product of tangent vectors (default=F)
#' @param ... extra arguments to pass to the distance function.
#' @return Value of NNDistFun passd to WeightedNNBasedLinesetMatching
#' @export
#' @seealso \code{\link[nat]{dotprops}}
#' @rdname WeightedNNBasedLinesetMatching
WeightedNNBasedLinesetMatching.dotprops<-function(dp1,dp2,UseAlpha=FALSE,...){
  if(UseAlpha)
    WeightedNNBasedLinesetMatching(dp1$points,dp2$points,dvs1=dp1$vect,dvs2=dp2$vect,
                                   alphas1=dp1$alpha,alphas2=dp2$alpha,...)
  else
    WeightedNNBasedLinesetMatching(dp1$points,dp2$points,dvs1=dp1$vect,dvs2=dp2$vect,...)
}


WeightedNNBasedLinesetMatching.default<-function(n1,n2,dvs1=NULL,dvs2=NULL,alphas1=NULL,
                                        alphas2=NULL,NNDistFun=WeightedNNBasedLinesetDistFun,Verbose=FALSE,
                                        BothDirections=FALSE,BothDirectionsFun=list,OnlyClosestPoints=FALSE,...){
  # my hybrid version
  # returns a score based on the similarity of nearest neighbour location
  # and the dot product of the direction vectors

  # accept either neurons or just the point dataframes
  if(is.list(n1) & !is.data.frame(n1))
    n1=data.matrix(n1$d[,c("X","Y","Z","Parent")])
  if(is.list(n2) & !is.data.frame(n2))
    n2=data.matrix(n2$d[,c("X","Y","Z","Parent")])

  NNDistFun=match.fun(NNDistFun)
  BothDirectionsFun=match.fun(BothDirectionsFun)
  if(BothDirections){
    f=WeightedNNBasedLinesetMatching(n1,n2,dvs1,dvs2,alphas1,alphas2,
                                     NNDistFun=NNDistFun,Verbose=Verbose,BothDirections=FALSE,...)
    b=WeightedNNBasedLinesetMatching(n2,n1,dvs1,dvs2,alphas1,alphas2,
                                     NNDistFun=NNDistFun,Verbose=Verbose,BothDirections=FALSE,...)
    if(length(f)==1 && length(b)==1) return (BothDirectionsFun(f,b))
    if(length(dim(f))==1 && length(f)==length(b)) return (cbind(f,b))
    return(BothDirectionsFun(f,b))
  }

  a=n1[,c("X","Y","Z")]
  b=n2[,c("X","Y","Z")]

  nnn1=RANN::nn2(a,b,k=1)


  idxArray=cbind(nnn1$nn.idx,seq(length(nnn1$nn.idx)))

  # Need to supply a set of pairs of points.
  # will use the parent of each chosen point.
  # if parent undefined, then ignore that point

  if(is.null(dvs1) || is.null(dvs2)){
    if(OnlyClosestPoints==TRUE)
      stop("OnlyClosestPoints is not yet implemented for neurons")
    # Calculate the direction vectors
    dvs=findDirectionVectorsFromParents(n1,n2,idxArray,ReturnAllIndices=TRUE,Verbose=Verbose)

    # Calculate segment lengths
    l1.seglengths=normbyrow(dvs[,1:3])
    l2.seglengths=normbyrow(dvs[,4:6])
    # normalise the direction vectors
    dvs[,1:3]=dvs[,1:3]/l1.seglengths
    dvs[,4:6]=dvs[,4:6]/l2.seglengths
    # Calculate absolute dot products
    # nb absolute, because we don't really care about directionality here
    dps=abs(dotprod(dvs[,1:3],dvs[,4:6]))
  } else {
    # OnlyClosestPoints prunes the list of query-target pairs so that no
    # points in the target are duplicated (points in query are already unique)
    if(OnlyClosestPoints){
      # sort by increasing distance between pairs
      # remove duplicates in target
      targetdupes=duplicated(nnn1$nn.idx[order(nnn1$nn.dist)])
      idxArray=idxArray[!targetdupes,,drop=FALSE]
      nnn1$nn.dists=nnn1$nn.dists[!targetdupes]
    }
    dps=abs(dotprod(dvs1[idxArray[,1],],dvs2[idxArray[,2],]))
    if(!is.null(alphas1) && !is.null(alphas2)){
      # for perfectly aligned points, alpha = 1, at worst alpha = 0
      # sqrt seems reasonable since if alpha1=alpha2=0.5 then
      # the scalefac will be 0.5
      # zapsmall to make sure there are no tiny negative numbers etc
      scalefac=sqrt(zapsmall(alphas1[idxArray[,1]]*alphas2[idxArray[,2]]))
      dps=dps*scalefac
    }
  }

  NNDistFun(nnn1$nn.dists,dps,...)
}

dotprod=function(a,b){
  # expects 2 matrices with n cols each
  c=a*b
  if(length(dim(c))>1) 	rowSums(c)
  else sum(c)
}

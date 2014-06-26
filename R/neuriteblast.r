#' Produce similarity score for neuron morphologies
#'
#' Uses the NBLAST algorithm that compares the morphology of two neurons. For
#' more control over the parameters of the algorithm, see the arguments of
#' \code{\link{NeuriteBlast}}.
#'
#' @param query the query neuron.
#' @param target a \code{\link[nat]{neuronlist}} to compare neuron against.
#'   Defaults to \code{options("nat.default.neuronlist")}.
#' @param smat the scoring matrix to use.
#' @param sd Standard deviation to use in distance dependence of nblast v1
#'   algorithm. Ignored when \code{version=2}.
#' @param version the version of the algorithm to use (the default, 2, is the
#'   latest).
#' @param normalised whether to divide scores by the self-match score of the
#'   query
#' @param UseAlpha whether to consider local directions in the similarity
#'   calculation (default: FALSE).
#' @return Named list of similarity scores.
#' @export
#' @importFrom nat neuronlist
#' @examples
#' data(kcs20,package='nat')
#' nblast(kcs20[[1]],kcs20)
nblast <- function(query, target, smat=NULL, sd=3, version=c(2, 1), normalised=FALSE,
                   UseAlpha=FALSE) {
  version <- as.character(version)
  version <- match.arg(version, c('2', '1'))

  # Convert target to neuronlist if passed a single object
  if("dotprops" %in% class(target)) target <- neuronlist(target)

  if(version == '2') {
    if(is.null(smat)) {
      smat=getOption("nat.nblast.defaultsmat")
      if(is.null(smat)) {
        if(UseAlpha) smat=smat_alpha.fcwb
        else smat=smat.fcwb
      }
    }
    if(is.character(smat)) smat=get(smat)
    NeuriteBlast(query=query, target=target, NNDistFun=lodsby2dhist, smat=smat, UseAlpha=UseAlpha,
                 normalised=normalised)
  } else if(version == '1') {
    NeuriteBlast(query=query, target=target, NNDistFun=WeightedNNBasedLinesetDistFun, UseAlpha=UseAlpha,
                 sd=sd, normalised=normalised)
  } else {
    stop("Only NBLAST versions 1 and 2 are currently implemented. For more advanced control, see NeuriteBlast.")
  }
}

#' Produce similarity score for neuron morphologies
#'
#' A low-level version of the NBLAST algorithm that compares the morphology of a
#' neuron with those of a list of other neurons. For most use cases, one would
#' probably wish to use \code{\link{nblast}} instead.
#' @param query the query neuron.
#' @param target a \code{\link[nat]{neuronlist}} to compare neuron against.
#'   Defaults to \code{options("nat.default.neuronlist")}.
#' @param targetBinds numeric indices or names with which to subset
#'   \code{target}.
#' @param Reverse whether to use \code{query} as the target neuron rather than
#'   query (default=FALSE).
#' @param normalised whether to divide scores by the self-match score of the
#'   query
#' @param ... extra arguments to pass to the distance function.
#' @return Named list of similarity scores.
#' @importFrom nat is.neuronlist
#' @export
#' @seealso \code{\link{WeightedNNBasedLinesetMatching}}
NeuriteBlast <- function(query, target=getOption("nat.default.neuronlist"),
                         targetBinds=NULL, Reverse=FALSE, normalised=FALSE, ...){
  if(nat::is.neuronlist(query)) {
    scores <- sapply(query, NeuriteBlast, target=target, targetBinds=targetBinds, Reverse=FALSE, normalised=FALSE, ...=...)
  } else {
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
          score=try(WeightedNNBasedLinesetMatching(query,dbn,...))
        else
          score=try(WeightedNNBasedLinesetMatching(dbn,query,...))
        if(!inherits(score,'try-error'))
          scores[i]=score
      }
    }
    names(scores)=names(target)[targetBinds]
  }
  if(normalised){
    scores=scores/NeuriteBlast(query, query, ...)
  }
  scores
}


WeightedNNBasedLinesetDistFun<-function(nndists,dotproducts,sd,...){
  summaryfun=function(x) 1-mean(sqrt(x),na.rm=T)
  sapply(sd,function(sd) summaryfun(dnorm(nndists,sd=sd)*dotproducts/dnorm(0,sd=sd)))
}


#' @rdname WeightedNNBasedLinesetMatching
#' @importFrom RANN nn2
#' @export
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

# Originally from AnalysisSuite PotentialSynapases.R
normbyrow=function(a){
  # returns euclidean norm (by row if reqd)
  c=a*a
  if(length(dim(c))>1) 	sqrt(rowSums(c))
  else sqrt(sum(c))
}

findDirectionVectorsFromParents<-function(d1,d2,idxArray,ReturnAllIndices=FALSE,Verbose=FALSE){
  # rather than dropping root points, just use the vector from them rather than to them
  if(Verbose) cat(".")
  p1=.CleanupParentArray(d1[,"Parent"])
  p2=.CleanupParentArray(d2[,"Parent"])
  parentPointsArray=cbind(p1[idxArray[,1]],p2[idxArray[,2]])
  # find any points with bad parents and instead use their offspring
  if(any(parentPointsArray[,1]<1 | parentPointsArray[,2]<1)){
    stop ("Some points do not have a parent: therefore impossible to calculate direction vector")
  }
  dvs=cbind(d1[idxArray[,1],c("X","Y","Z"),drop=FALSE]-d1[parentPointsArray[,1],c("X","Y","Z"),drop=FALSE],
            d2[idxArray[,2],c("X","Y","Z"),drop=FALSE]-d2[parentPointsArray[,2],c("X","Y","Z"),drop=FALSE])

  if(ReturnAllIndices){
    attr(dvs,"idxArray")=idxArray
    attr(dvs,"parentPointsArray")=parentPointsArray
  }
  dvs
}

.CleanupParentArray<-function(pa){
  # takes a list of parents for points and replaces any <1
  # with the first offspring of that point
  #if(length(dim(pa))>1) apply(pa,2,.CleanupParentArray)
  pointsNeedingWork<-which(pa<1)
  if(length(pointsNeedingWork)<1) return( pa )
  for(p in pointsNeedingWork){
    wp=which(pa==p)
    if(length(wp)>1){
      warning(cat("more than 1 point in .CleanupParentArray, choosing first from:",wp))
      pa[p]=wp[1]
    } else if(length(wp)<1){
      warning("no points to choose in .CleanupParentArray using original value")
    } else pa[p]=wp
  }
  pa
}


lodsby2dhist <- function(nndists, dotprods, smat, Return=c('sum', 'weightedlodtable', 'elements')) {
  Return <- match.arg(Return)

  if(missing(dotprods)) {
    if(!is.list(nndists))
      stop("must provide nndists and dotprods or a list with both")
    dotprods <- nndists[[2]]
    nndists <- nndists[[1]]
  }
  if(length(nndists)!= length(dotprods))
    stop("nndists and dotprods must have the same length.")

  c1 <- findInterval(nndists, attr(smat,"distbreaks"), all.inside=TRUE)
  # NB use of all.inside fixes NAs that would otherwise result
  # when dot product falls outside (0,1) due to floating point errors
  c2 <- findInterval(dotprods, attr(smat,"dotprodbreaks"), all.inside=TRUE)

  if(Return == 'elements') return(smat[cbind(c1,c2)])

  nlc1 <- length(attr(smat, "distbreaks")) - 1
  nlc2 <- length(attr(smat, "dotprodbreaks")) - 1

  this.countstable <- fast2dintegertable(c1, c2, nlc1, nlc2)

  weightedlodtable <- smat*this.countstable
  if(Return == 'sum') return(sum(weightedlodtable))
  weightedlodtable
}


fast2dintegertable <- function(a, b, nlevelsa=max(a), nlevelsb=max(b)) {
  # Fast 2D cross-tabulation (joint histogram) for two integer inputs

  nlevelsab <- nlevelsa*nlevelsb
  if(nlevelsab > .Machine$integer.max) stop("Number of levels exceeds integer type.")
  ab <- (a - 1) * nlevelsb + b
  matrix(tabulate(ab, nlevelsab), ncol=nlevelsb, nrow=nlevelsa, byrow=T)
}

#' @title sampleDist
#'
#' @description Generate information for questions involving a single sample or population
#'
#' @slot alpha Alpha for significance testing
#' @slot ci Confidence Interval (Two Number Vector)
#' @slot ci.lb Confidence Interval: Lower Bound
#' @slot cu.ub Confidence Interval: Upper Bound
#' @slot df Population Degrees of Freedom
#' @slot greater.than TRUE for Greater Than One-Tailed Testing, FALSE for Less Than (Defaults to FALSE)
#' @slot iqr Population IQR
#' @slot level Level for confidence interval calculation
#' @slot mean Population Mean
#' @slot median Population Median
#' @slot min Population Min
#' @slot max Population Max
#' @slot outliers Population Outliers
#' @slot population Population Data (Vector of Numbers)
#' @slot q1 Population Q1
#' @slot q3 Population Q3
#' @slot range Population Range
#' @slot samp.dist.sd Sampling Distribution Standard Deviation
#' @slot samp.dist.var Sampling Distribution Variance
#' @slot samp.df Sample Degrees of Freedom
#' @slot samp.mean Sample Mean
#' @slot samp.sd Sample Standard Deviation
#' @slot samp.se Sample Standard Error
#' @slot samp.size Sample Size
#' @slot samp.t Hypothesis Test t-value
#' @slot samp.t.p Hypothesis Test p-value (for t-method)
#' @slot samp.t.sig Hypothesis Test significance (for t-method)
#' @slot samp.t.sig.sign Hypothesis Test sign, > or < alpha (for t-method)
#' @slot samp.t.sig.verb Hypothesis Test verbose significance (for t-method)
#' @slot samp.var Sample Variance
#' @slot samp.z Hypothesis Test z-value
#' @slot samp.z.p Hypothesis Test p-value (for z method)
#' @slot samp.z.sig Hypothesis Test significance (for z-method)
#' @slot samp.z.sig.sign Hypothesis Test sign, > or < alpha (for z-method)
#' @slot samp.z.sig.verb Hypothesis Test verbose significance (for z-method)
#' @slot sd Population Standard Deviation
#' @slot se Population Standard Error
#' @slot sign Testing sign (>,<,≠)
#' @slot size Population Size
#' @slot two.tail TRUE for Two-Tailed testing, FALSE for One-Tailed (Defaults to FALSE)
#' @slot var Population Variance
#' @slot z Population Probability Test z-value
#' @slot z.p Population Probability Test p-value
#' @slot z.p.sig Population Probability Test significance
#' @slot z.p.sig.sign Population Probability Test sign, > or < alpha
#' @slot z.p.sig.verb Population Probability Test verbose significance
#'
#' @exportClass sampleDist
setClass("sampleDist",
  slots=c(
    alpha="numeric",
    ci="numeric",
    ci.lb="numeric",
    ci.ub="numeric",
    df="numeric",
    greater.than="logical",
    iqr="numeric",
    level="numeric",
    mean="numeric",
    median="numeric",
    min="numeric",
    max="numeric",
    outliers="numeric",
    population="numeric",
    q1="numeric",
    q3="numeric",
    range="numeric",
    samp.dist.sd="numeric",
    samp.dist.var="numeric",
    samp.df="numeric",
    samp.mean="numeric",
    samp.sd="numeric",
    samp.se="numeric",
    samp.size="numeric",
    samp.t="numeric",
    samp.t.p="numeric",
    samp.t.sig="logical",
    samp.t.sig.sign="character",
    samp.t.sig.verb="character",
    samp.var="numeric",
    samp.z="numeric",
    samp.z.p="numeric",
    samp.z.sig="logical",
    samp.z.sig.sign="character",
    samp.z.sig.verb="character",
    sd="numeric",
    se="numeric",
    sign="character",
    size="numeric",
    two.tail="logical",
    var="numeric",
    z="numeric",
    z.p="numeric",
    z.sig="logical",
    z.sig.sign="character",
    z.sig.verb="character"
  ),
  prototype=list(
    alpha=as.numeric(NA),
    ci=as.numeric(NA),
    ci.lb=as.numeric(NA),
    ci.ub=as.numeric(NA),
    df=as.numeric(NA),
    greater.than=NA,
    iqr=as.numeric(NA),
    level=as.numeric(NA),
    mean=as.numeric(NA),
    median=as.numeric(NA),
    min=as.numeric(NA),
    max=as.numeric(NA),
    outliers=as.numeric(NA),
    population=as.numeric(NA),
    q1=as.numeric(NA),
    q3=as.numeric(NA),
    range=as.numeric(NA),
    samp.dist.sd=as.numeric(NA),
    samp.dist.var=as.numeric(NA),
    samp.df=as.numeric(NA),
    samp.mean=as.numeric(NA),
    samp.sd=as.numeric(NA),
    samp.se=as.numeric(NA),
    samp.size=as.numeric(NA),
    samp.t=as.numeric(NA),
    samp.t.p=as.numeric(NA),
    samp.t.sig=NA,
    samp.t.sig.sign=as.character(NA),
    samp.t.sig.verb=as.character(NA),
    samp.var=as.numeric(NA),
    samp.z=as.numeric(NA),
    samp.z.p=as.numeric(NA),
    samp.z.sig=NA,
    samp.z.sig.sign=as.character(NA),
    samp.z.sig.verb=as.character(NA),
    sd=as.numeric(NA),
    se=as.numeric(NA),
    sign=as.character(NA),
    size=as.numeric(NA),
    two.tail=NA,
    var=as.numeric(NA),
    z=as.numeric(NA),
    z.p=as.numeric(NA),
    z.sig=NA,
    z.sig.sign=as.character(NA),
    z.sig.verb=as.character(NA)
  )
)

#' @title sampleDist
#'
#' @description Create a "sampeDist" object to generate information for questions involving a single sample or population
#'
#' @param sample Previous sampleDist Object
#' @param population Vector of numbers denoting the population of interest
#' @param size Size of the population
#' @param mean Mean of the population
#' @param sd Standard Deviation of the population
#' @param se Standard Error of the population
#' @param samp.size Size of the sample
#' @param samp.mean Mean of the sample
#' @param samp.sd Standard Deviation of the sample
#' @param samp.se Standard Error of the sample
#' @param alpha Alpha for signficance testing
#' @param level Level for confidence intervals
#' @param two.tail TRUE for Two-Tailed testing, FALSE for One-Tailed (Defaults to FALSE)
#' @param greater.than TRUE for Greater Than One-Tailed Testing, FALSE for Less Than (Defaults to FALSE)
#'
#' @return sampleDist
#'
#' @examples sampleDist(sd = 1, mean = 0, size = 100)
#'
#' @export sampleDist
sampleDist<-function(sample=NA, population=NA, size=NA_real_, mean=NA_real_, sd=NA_real_, se=NA_real_, samp.size=NA_real_, samp.mean=NA_real_, samp.sd=NA_real_, samp.se=NA_real_, alpha=NA_real_, level=NA_real_, two.tail=NA_real_, greater.than=NA_real_){
  ## sd when true value, se when estimated
  if(isS4(sample)){
    mys<-sample
  }
  else{
    mys<-new("sampleDist")
    mys@alpha<-0.05
    mys@level<-0.95
    mys@two.tail<-F
    mys@greater.than<-F
  }
  if(!is.na(population)){
    mys@population<-population
    mys@size<-length(population)
    mys@mean<-mean(population)
    mys@sd<-sd(population)
  }
  else{
    if(!is.na(size)){
      mys@size<-size
    }
    if(!is.na(mean)){
      mys@mean<-mean
    }
    if(!is.na(sd)){
      mys@sd<-sd
    }
    else if(!is.na(se)){
      mys@se<-se
      mys@sd<-mys@se*sqrt(mys@size)
    }
    if(!is.na(mys@size) && !is.na(mys@mean) && is.na(mys@sd) && (!is.na(size) || !is.na(mean) || !is.na(sd))){
      mys@population<-as.vector(mys@mean+mys@sd*scale(rnorm(mys@size)))
    }
  }
  if(!is.na(alpha)){
    mys@alpha<-alpha
  }
  if(!is.na(level)){
    mys@level<-level
  }
  mys@sign<-"<"
  if(!is.na(greater.than)){
    mys@greater.than<-greater.than
    mys@sign<-">"
  }
  if(!is.na(two.tail)){
    mys@two.tail<-two.tail
    mys@sign<-"\neq"
  }
  if(!is.na(samp.mean)){
    mys@samp.mean<-samp.mean
  }
  if(!is.na(samp.size)){
    mys@samp.size<-samp.size
  }
  if(!is.na(samp.sd)){
    mys@samp.sd<-samp.sd
  }
  else if(!is.na(samp.se)){
    mys@samp.se<-samp.se
    mys@samp.sd<-mys@samp.se*sqrt(mys@samp.size)
  }
  if(!is.na(mys@population)){
    mys@median<-median(mys@population)
    mys@q1<-as.numeric(quantile(mys@population, 0.25))
    mys@q3<-as.numeric(quantile(mys@population, 0.75))
    mys@iqr<-mys@q3-mys@q1
    mys@outliers<-mys@population[mys@population<mys@q1-1.5*mys@iqr | mys@population>mys@q3+1.5*mys@iqr]
    mys@min<-min(mys@population)
    mys@max<-max(mys@population)
    mys@range<-mys@max-mys@min
  }
  mys@df<-mys@size-1
  mys@var<-mys@sd^2
  if(is.na(se)){
    mys@se<-mys@sd/sqrt(mys@size)
  }
  if(!is.na(mys@mean) && !is.na(mys@samp.mean)){
    if(!is.na(mys@sd)){
      mys@z<-(mys@samp.mean-mys@mean)/mys@sd
      if(mys@two.tail==1){
        mys@z.p<-c(2*pnorm(floor(-abs(mys@z)*100)/100),2*pnorm(ceiling(-abs(mys@z)*100)/100))
      }
      else if(mys@greater.than==1){
        mys@z.p<-c(pnorm(floor(-mys@z*100)/100),pnorm(ceiling(-mys@z*100)/100))
      }
      else{
        mys@z.p<-c(pnorm(floor(mys@z*100)/100),pnorm(ceiling(mys@z*100)/100))
      }
      if(!is.na(mys@alpha)){
        if(mys@z.p[1] < mys@alpha){
          mys@z.sig<-T
          mys@z.sig.sign<-"<"
          mys@z.sig.verb<-"significant"
        }else{
          mys@z.sig<-F
          mys@z.sig.sign<-">"
          mys@z.sig.verb<-"not significant"
        }
      }
    }
    if(!is.na(mys@samp.size)){
      mys@samp.df<-mys@samp.size-1
      if(!is.na(mys@sd)){
        mys@samp.dist.sd<-mys@sd/sqrt(mys@samp.size)
        mys@samp.dist.var<-mys@samp.dist.sd^2
        mys@samp.z<-(mys@samp.mean-mys@mean)/mys@samp.dist.sd
        if(mys@two.tail==1){
          mys@samp.z.p<-c(2*pnorm(floor(-abs(mys@samp.z)*100)/100),2*pnorm(ceiling(-abs(mys@samp.z)*100)/100))
        }
        else if(mys@greater.than==1){
          mys@samp.z.p<-c(pnorm(floor(-mys@samp.z*100)/100),pnorm(ceiling(-mys@samp.z*100)/100))
        }
        else{
          mys@samp.z.p<-c(pnorm(floor(mys@samp.z*100)/100),pnorm(ceiling(mys@samp.z*100)/100))
        }
        mys@ci<-qnorm(c(0.5-mys@level/2,0.5+mys@level/2))*mys@samp.dist.sd+mys@samp.mean
        mys@ci.lb<-mys@ci[1]
        mys@ci.ub<-mys@ci[2]
        if(!is.na(mys@alpha)){
          if(mys@samp.z.p[1] < mys@alpha){
            mys@samp.z.sig<-T
            mys@samp.z.sig.sign<-"<"
            mys@samp.z.sig.verb<-"significant"
          }else{
            mys@samp.z.sig<-F
            mys@samp.z.sig.sign<-">"
            mys@samp.z.sig.verb<-"not significant"
          }
        }
      }
      if(!is.na(mys@samp.sd)){
        mys@samp.var<-mys@samp.sd^2
        if(is.na(samp.se)){
          mys@samp.se<-mys@samp.sd/sqrt(mys@samp.size)
        }
        ftb<-c(1000,100,80,60,50,40,30:1)
        ptb<-c(100,(5:1)*5,2.5,2,1,0.5,0.25,0.1,0.05)/100
        mys@samp.t<-(mys@samp.mean-mys@mean)/mys@samp.se
        if(mys@two.tail==1){
          mys@samp.t.p<-pt(-abs(mys@samp.t), df=ftb[(ftb<=mys@samp.df)][1])
        }
        else if(mys@greater.than==1){
          mys@samp.t.p<-pt(-mys@samp.t, df=ftb[(ftb<=mys@samp.df)][1])
        }
        else{
          mys@samp.t.p<-pt(mys@samp.t, df=ftb[(ftb<=mys@samp.df)][1])
        }
        mys@samp.t.p<-c(ptb[(ptb<=mys@samp.t.p)][1],tail(ptb[(ptb>=0.03)],n=1))*(1+mys@two.tail)
        if(!is.na(mys@alpha)){
          if(mys@samp.t.p[1] < mys@alpha){
            mys@samp.t.sig<-T
            mys@samp.t.sig.sign<-"<"
            mys@samp.t.sig.verb<-"significant"
          }else{
            mys@samp.t.sig<-F
            mys@samp.t.sig.sign<-">"
            mys@samp.t.sig.verb<-"not significant"
          }
        }
      }
    }
  }
  mys@ci<-round(mys@ci,4)
  mys@ci.lb<-round(mys@ci.lb,4)
  mys@ci.ub<-round(mys@ci.ub,4)
  mys@mean<-round(mys@mean,4)
  mys@samp.dist.sd<-round(mys@samp.dist.sd,4)
  mys@samp.dist.var<-round(mys@samp.dist.var,4)
  mys@samp.mean<-round(mys@samp.mean,4)
  mys@samp.sd<-round(mys@samp.sd,4)
  mys@samp.se<-round(mys@samp.se,4)
  mys@samp.t<-round(mys@samp.t,4)
  mys@samp.t.p<-round(mys@samp.t.p,4)
  mys@samp.var<-round(mys@samp.var,4)
  mys@samp.z<-round(mys@samp.z,4)
  mys@samp.z.p<-round(mys@samp.z.p,4)
  mys@sd<-round(mys@sd,4)
  mys@se<-round(mys@se,4)
  mys@var<-round(mys@var,4)
  mys@z<-round(mys@z,4)
  mys@z.p<-round(mys@z.p,4)
  return(mys)
}

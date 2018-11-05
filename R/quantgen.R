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
#' @param samp.size Size of the sample
#' @param samp.mean Mean of the sample
#' @param samp.sd Standard Deviation of the sample
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
sampleDist<-function(sample=NA, population=NA, size=NA_real_, mean=NA_real_, sd=NA_real_, samp.size=NA_real_, samp.mean=NA_real_, samp.sd=NA_real_, alpha=NA_real_, level=NA_real_, two.tail=NA_real_, greater.than=NA_real_){
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
  mys@se<-mys@sd/sqrt(mys@size)
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
        mys@samp.se<-mys@samp.sd/sqrt(mys@samp.size)
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


#' @title sample2Dist
#'
#' @description Generate information for questions involving a two samples
#'
#' @slot alpha Alpha for significance testing
#' @slot ci Confidence Interval (Two Number Vector)
#' @slot ci.lb Confidence Interval: Lower Bound
#' @slot ci.ub Confidence Interval: Upper Bound
#' @slot conclusion
#' @slot confidence.level Level for confidence interval calculation in percentage points
#' @slot df Degrees of freedom
#' @slot df.D Degrees of freedom Table D
#' @slot estimator The value of the estimator (sample mean 1 - sample mean 0)
#' @slot equal.sd TRUE for equal standard deviations, FALSE for different standard deviations (Defaults to FALSE)
#' @slot greater.than TRUE for Greater Than One-Tailed Testing, FALSE for Less Than (Defaults to FALSE)
#' @slot margin Margin of error for confidence interval
#' @slot null level for Null Hypothesis
#' @slot numerator Numerator for the statistic
#' @slot pop.sd.0 Population 0 Standard Deviation
#' @slot pop.sd.1 Population 1 Standard Deviation
#' @slot pooled.s Pooled Standard Deviation
#' @slot samp.mean.0 Sample 0 Mean
#' @slot samp.mean.1 Sample 1 Mean
#' @slot samp.sd.0 Sample 0 Standard Deviation
#' @slot samp.sd.1 Sample 1 Standard Deviation
#' @slot samp.size.0 Sample 0 Size
#' @slot samp.size.1 Sample 1 Size
#' @slot samp.t Hypothesis Test t-value
#' @slot samp.t.p Hypothesis Test p-value (for t-method) (Two Number Vector, sometimes)
#' @slot samp.t.pvalue The verbose statement on p-value (for t-method)
#' @slot samp.t.sig Hypothesis Test significance (for t-method)
#' @slot samp.t.sig.sign Hypothesis Test sign, > or < alpha (for t-method)
#' @slot samp.t.sig.verb Hypothesis Test verbose significance (for t-method)
#' @slot samp.z Hypothesis Test z-value
#' @slot samp.z.p Hypothesis Test p-value (for z method)
#' @slot samp.z.pvalue The verbose statement on p-value (for z method)
#' @slot samp.z.sig Hypothesis Test significance (for z-method)
#' @slot samp.z.sig.sign Hypothesis Test sign, > or < alpha (for z-method)
#' @slot samp.z.sig.verb Hypothesis Test verbose significance (for z-method)
#' @slot sd.estimator The standard deviation of the estimator
#' @slot se.estimator The standard error of the estimator
#' @slot sign Testing sign (>,<,≠)
#' @slot t.star Critical value for confidence interval from the t-distribution (Table D version)
#' @slot two.tail TRUE for Two-Tailed testing, FALSE for One-Tailed (Defaults to FALSE)
#' @slot z.star Critical value for confidence interval from the Standard Normal distribution (Table A version)
#' @exportClass sample2Dist
setClass("sample2Dist",
         slots=c(
           alpha="numeric",
           ci="numeric",
           ci.lb="numeric",
           ci.ub="numeric",
           conclusion="character",
           confidence.level="numeric",
           df="numeric",
           df.D="numeric",
           greater.than="logical",
           equal.sd="logical",
           estimator="numeric",
           margin="numeric",
           null="numeric",
           numerator="numeric",
           pop.sd.0="numeric",
           pop.sd.1="numeric",
           pooled.s="numeric",
           samp.mean.0="numeric",
           samp.mean.1="numeric",
           samp.sd.0="numeric",
           samp.sd.1="numeric",
           samp.size.0="numeric",
           samp.size.1="numeric",
           samp.t="numeric",
           samp.t.p="numeric",
           samp.t.pvalue="character"
           samp.t.sig="logical",
           samp.t.sig.sign="character",
           samp.t.sig.verb="character",
           samp.z="numeric",
           samp.z.p="numeric",
           samp.z.pvalue="character",
           samp.z.sig="logical",
           samp.z.sig.sign="character",
           samp.z.sig.verb="character",
           sd.estimator="numeric",
           se.estimator="numeric",
           sign="character",
           t.star="numeric"
           two.tail="logical",
           z.star="numeric"
         ),
         prototype=list(
           alpha=as.numeric(NA),
           ci=as.numeric(NA),
           ci.lb=as.numeric(NA),
           ci.ub=as.numeric(NA),
           conclusion=as.character(NA),
           confidence.level=as.numeric(NA),
           df=as.numeric(NA),
           df.D=as.numeric(NA),
           greater.than=NA,
           equal.sd=NA,
           estimator=as.numeric(NA),
           margin=as.numeric(NA),
           null=as.numeric(NA),
           numerator=as.numeric(NA),
           pop.sd.0=as.numeric(NA),
           pop.sd.1=as.numeric(NA),
           pooled.s=as.numeric(NA),
           samp.mean.0=as.numeric(NA),
           samp.mean.1=as.numeric(NA),
           samp.sd.0=as.numeric(NA),
           samp.sd.1=as.numeric(NA),
           samp.size.0=as.numeric(NA),
           samp.size.1=as.numeric(NA),
           samp.t=as.numeric(NA),
           samp.t.p=as.numeric(NA),
           samp.t.pvalue=as.character(NA),
           samp.t.sig=NA,
           samp.t.sig.sign=as.character(NA),
           samp.t.sig.verb=as.character(NA),
           samp.z=as.numeric(NA),
           samp.z.p=as.numeric(NA),
           samp.z.pvalue=as.character(NA),
           samp.z.sig=NA,
           samp.z.sig.sign=as.character(NA),
           samp.z.sig.verb=as.character(NA),
           sd.estimator=as.numeric(NA),
           se.estimator=as.numeric(NA),
           sign=as.character(NA),
           t.star=as.numeric(NA)
           two.tail=NA,
           z.star=as.numeric(NA)
         )
)

#' @title sample2Dist
#'
#' @description Create a "sampe2Dist" object to generate information for questions involving two samples
#'
#' @param samples Previous sample2Dist Object
#' @param eqal.sd The standard deviations of the two populations are equal
#' @param samp.size.0 Size of the sample 0
#' @param samp.size.1 Size of the sample 1
#' @param samp.mean.0 Mean of the sample 0
#' @param samp.mean.1 Mean of the sample 1
#' @param samp.sd.0 Standard deviation of the sample 0
#' @param samp.sd.1 Standard deviation of the sample 1
#' @param two.tail TRUE for Two-Tailed testing, FALSE for One-Tailed (Defaults to FALSE)
#' @param greater.than TRUE for Greater Than One-Tailed Testing, FALSE for Less Than (Defaults to FALSE)
#' @param alpha Alpha for signficance testing
#' @param pop.sd.0 Population 0 Standard Deviation
#' @param pop.sd.1 Population 1 Standard Deviation
#' @param confidence.level Population 1 Standard Deviation
#'
#' @return sample2Dist
#'
#' @examples sample2Dist(equal.sd=FALSE,samp.size.0=100,samp.size.1=50,samp.mean.0=2,samp.mean.1=3,samp.sd.0=3,samp.sd.1=4,two.tail=TRUE,alpha=0.05)
#'
#' @export sample2Dist
sample2Dist<-function(samples=NA, equal.sd=NA_real_, samp.size.0=NA_real_, samp.size.1=NA_real_, samp.mean.0=NA_real_, samp.mean.1=NA_real_, samp.sd.0=NA_real_, samp.sd.1=NA_real_, two.tail=NA_real_, greater.than=NA_real_, alpha=NA_real_, confidence.level=NA_real_, pop.sd.0=NA_real_, pop.sd.1=NA_real_){
  if(isS4(samples)) {
    mys<-samples
  }
  else {
    mys<-new("sample2Dist")
  }








  mys@estimator<-mys@samp.mean.1-mys@samp.mean.0



  if(!is.na(mys@pop.sd.0) & !.isna(mys@pop.sd.1)){
    # Z distribution
    # Confidence interval
    mys@sd.estimator<-ceiling(1000*sqrt(mys@pop.sd.0^2/mys@samp.size.0+mys@pop.sd.1^2/mys@samp.size.1))/1000
    mys@zstar<-round(abs(qnorm(1-mys@confidence.level/100)),3)
    mys@margin<-ceiling(1000*mys@zstar*mys@sd.estimator)/1000
    mys@ci<-c(mys@estimator-mys@margin,mys@estimator+mys@margin)
    mys@ci.lb<-mys@ci[1]
    mys@ci.ub<-mys@ci[2]
    mys@verbose.confidence.interval<-paste("The confidence interval is centered at the value of the estimator is the difference between sample means, that is $\bar x_1-\bar x_0=",mys@samp.mean.1,"-",mys@samp.mean.0,"=",mys@estimator,"$. The standard deviation of the estimator is $\sigma_{\bar X_1-\bar X_0}=\sqrt{\frac{\sigma_1^2}{n_1}+\frac{\sigma_0^2}{n_0}}=\sqrt{\frac{",mys@pop.sd.1,"^2}{",mys@samp.size.1,"}+\frac{"mys@pop.sd.1,"^2}{",mys@samp.size.1,"}}=",mys@sd.estimator,"$. The critical value for a $",mys@confidence.level,"$\% confidence interval is $z^*="mys@z.star,"$, so the margin of error is $m=z^* \cdot \sigma_{\bar X_1-\bar X_0}=",mys@z.star," \cdot ",mys@sd.estimator,"=",mys@margin,"$. The confidence interval is $[\bar x_1-\bar x_0-m,\bar x_1-\bar x_0+m]=[",mys@estimator,"-",mys@margin","mys@estimator,"+",mys@margin,"]=[",mys@ci.lb,",",mys@ci.ub,"]$.",sep="")
    # Hypothesis testing
    mys@numerator<-mys@estimator-mys@null
    mys@samp.z<-ceiling(100*mys@numerator/mys@sd.estimator)/100
    if(mys@two.tail==1) {
      mys@samp.z.p<-2*(1-pnorm(abs(mys@samp.z)))
      mys@samp.z.pvalue<-paste("The p-value is $2 \cdot P(Z>|z|)=2 \cdot P(Z>|",mys@samp.z,"|)=2 \cdot (1-P(Z<",abs(mys@samp.z),")=",mys@samp.z.p,"$",sep="")
    }else{
      if(mys@greater.than==1) {
        mys@samp.z.p<-pnorm(mys@samp.z)
        mys@samp.z.pvalue<-paste("The p-value is $P(Z>z)=P(Z>",mys@samp.z,")=1-P(Z<",mys@samp.z,")=",mys@samp.z.p,"$",sep="")
      }else {
        mys@samp.z.p<-1-pnorm(mys@samp.z)
        mys@samp.z.pvalue<-paste("The p-value is $P(Z<z)=P(Z<",mys@samp.z,")=",mys@samp.z.p,"$",sep="")
      }
    }
    if(mys@samp.z.p < mys@alpha){
      mys.samp.z.sig<-T
      mys.samp.z.sig.sign<-"<"
      mys.samp.z.verb<-"statistically significant"
      mys.conclusion<-"We reject the Null Hypothesis and accept the Alternative Hypothesis."
      mys.samp.z.verbose.testing<-paste("\begin{itemize}\item $H_0: \, \mu_1-\mu_0=",mys@null,"$\item $H_a: \, \mu_1-\mu_0 ",mys@samp.z.sig.sign,mys@null,"$\item $\alpha=",mys@alpha,"$\item $z=\frac{\bar x_1-\bar x_0-",mys@null,"}{\sigma_{\bar X_1-\bar X_0}}=\frac{",mys@estimator,"-",mys@null,"}{\sqrt{\frac{",mys@pop.sigma.1,"^2}{",mys@samp.size.1,"}+\frac{",mys@pop.sigma.0,"^2}{",mys@samp.size.0,"}}}=\frac{",mys@numerator,"}{",mys@sd.estimator,"}=",samp.z,"$\item ",mys@samp.z.pvalue,"\item ",mys@conclusion,"\end{itemize}",sep="")
    }else{
      mys.samp.z.sig<-F
      mys.samp.z.sig.sign<-">"
      mys.samp.z.verb<-"not statistically significant"
      mys.conclusion<-"We fail to reject the Null Hypothesis."
      mys.samp.z.verbose.testing<-paste("\begin{itemize}\item $H_0: \, \mu_1-\mu_0=",mys@null,"$\item $H_a: \, \mu_1-\mu_0 ",mys@samp.z.sig.sign,mys@null,"$\item $\alpha=",mys@alpha,"$\item $z=\frac{\bar x_1-\bar x_0-",mys@null,"}{\sigma_{\bar X_1-\bar X_0}}=\frac{",mys@estimator,"-",mys@null,"}{\sqrt{\frac{",mys@pop.sigma.1,"^2}{",mys@samp.size.1,"}+\frac{",mys@pop.sigma.0,"^2}{",mys@samp.size.0,"}}}=\frac{",mys@numerator,"}{",mys@sd.estimator,"}=",samp.z,"$\item ",mys@samp.z.pvalue,"\item ",mys@conclusion,"\end{itemize}",sep="")
    }





  }
  else {
    if(mys@equal.sd==1) {
      # T distribution equal standard deviations
      mys@df<-mys@samp.size.0+mys@samp.size.1-2
      mys@pooled.s<-ceiling(1000*sqrt((mys@samp.sd.0^2*(mys@samp.size.0-1)+mys@samp.sd.1^2*(mys@samp.size.1-1))/mys@df))/1000
      mys@se.estimator<-ceiling(1000*sqrt(mys@pooled.s^2/mys@samp.size.0+mys@pooled.s^2/mys@samp.size.1))/1000
      mys@df.D<-
      mys@tstar<-round(abs(qt(1-mys@confidence.level/100,mys@df.D)),3)
      mys@margin<-ceiling(1000*mys@tstar*mys@se.estimator)/1000
      mys@ci<-c(mys@estimator-mys@margin,mys@estimator+mys@margin)
      mys@ci.lb<-mys@ci[1]
      mys@ci.ub<-mys@ci[2]
      mys@verbose.confidence.interval<-paste("The confidence interval is centered at the value of the estimator, which is the difference between sample means, that is $\bar x_1-\bar x_0=",mys@samp.mean.1,"-",mys@samp.mean.0,"=",mys@estimator,"$. The pooled standard deviation is $\sqrt{\frac{s_1^2 \cdot (n_1-1)+s_0^2 \cdot (n_0-1)}{n_1+n_0-2}}=\sqrt{\frac{",mys@samp.sd.1,"^2 \cdot (",mys@samp.size.1,"-1)+",mys@samp.sd.0",^2 \cdot (",mys@samp.size.0,"-1)}{",samp.size.1,"+",samp.size.0,"-2}}=",mys@pooled.s,"$ The standard error of the estimator is $SE_{\bar X_1-\bar X_0}=\sqrt{\frac{s_p^2}{n_1}+\frac{s_p^2}{n_0}}=\sqrt{\frac{",mys@pooled.s,"^2}{",mys@samp.size.1,"}+\frac{"mys@pooled.s,"^2}{",mys@samp.size.1,"}}=",mys@se.estimator,"$. The number of degrees of freedom is the smaller number of degrees of freedom in each sample, which in this case is ",mys@df,". We will use a conservative number of degrees of freedom from Table D, that is ",mys.df.D,". The critical value for a $",mys@confidence.level,"$\% confidence interval is $z^*="mys@t.star,"$, so the margin of error is $m=t^* \cdot SE_{\bar X_1-\bar X_0}=",mys@t.star," \cdot ",mys@se.estimator,"=",mys@margin,"$. The confidence interval is $[\bar x_1-\bar x_0-m,\bar x_1-\bar x_0+m]=[",mys@estimator,"-",mys@margin","mys@estimator,"+",mys@margin,"]=[",mys@ci.lb,",",mys@ci.ub,"]$.",sep="")
      # Hypothesis testing
      mys@numerator<-mys@estimator-mys@null
      mys@samp.t<-ceiling(100*mys@numerator/mys@se.estimator)/100
      if(mys@two.tail==1) {
        mys@samp.t.p<-2*c(ptb[(ptb<abs(mys@samp.t))][1],ptb[(ptb>abs(mys@samp.t))][length(ptb[(ptb>abs(mys@samp.t))])])
        mys@samp.t.pvalue<-paste("The p-value is $2 \cdot P(T>|t|)=2 \cdot P(T>|",mys@samp.t,"|)$ in the t-distribution with ",mys@df," degrees of freedom. Using ",mys@df.D," degrees of freedom from Table D, $P(T>",abs(mys@samp.t),") \in (",mys@samp.t.p[1]/2,",",mys@samp.t.p[2]/2,")$ so the p-value is in the $(",mys@samp.t.p[1],",",mys@samp.t.p[2],")$ interval.",sep="")
      }else{
        if(mys@greater.than==1) {
          if(mys@samp.t>0) {
            mys@samp.t.p<-c(ptb[(ptb<abs(mys@samp.t))][1],ptb[(ptb>abs(mys@samp.t))][length(ptb[(ptb>abs(mys@samp.t))])])
            mys@samp.t.pvalue<-paste("The p-value is $P(T>t)=P(T>",mys@samp.t,")$ in the t-distribution with ",mys@df," degrees of freedom. Using ",mys@df.D," degrees of freedom from Table D, $P(T>",mys@samp.t,") \in (",mys@samp.t.p[1],",",mys@samp.t.p[2],")$.",sep="")
          }else{
            mys@samp.t.p<-1-c(ptb[(ptb>abs(mys@samp.t))][length(ptb[(ptb>abs(mys@samp.t))])],ptb[(ptb<abs(mys@samp.t))][1])
            mys@samp.t.pvalue<-paste("The p-value is $P(T<t)=P(T<",mys@samp.t,")$ in the t-distribution with ",mys@df," degrees of freedom. That is larger than 50\%. More precisely, using ",mys@df.D," degrees of freedom from Table D, $P(T>",mys@samp.t,")=P(T<",-mys@samp.t,")=1-P(T>",-mys@samp.t,") \in (",mys@samp.t.p[1],",",mys@samp.t.p[2],")$.",sep="")
          }
        }else {
          if(mys@samp.t<0) {
            mys@samp.t.p<-c(ptb[(ptb<abs(mys@samp.t))][1],ptb[(ptb>abs(mys@samp.t))][length(ptb[(ptb>abs(mys@samp.t))])])
            mys@samp.t.pvalue<-paste("The p-value is $P(T<t)=P(T<",mys@samp.t,")=P(T>",-mys@samp.t,")$ in the t-distribution with ",mys@df," degrees of freedom. Using ",mys@df.D," degrees of freedom from Table D, $P(T>",-mys@samp.t,") \in (",mys@samp.t.p[1],",",mys@samp.t.p[2],")$.",sep="")
          }else {
            mys@samp.t.p<-1-c(ptb[(ptb>abs(mys@samp.t))][length(ptb[(ptb>abs(mys@samp.t))])],ptb[(ptb<abs(mys@samp.t))][1])
            mys@samp.t.pvalue<-paste("The p-value is $P(T<t)=P(T<",mys@samp.t,")$ in the t-distribution with ",mys@df," degrees of freedom. That is larger than 50\%. More precisely, using ",mys@df.D," degrees of freedom from Table D, $P(T<",mys@samp.t,")=1-P(T>",mys@samp.t,") \in (",mys@samp.t.p[1],",",mys@samp.t.p[2],")$.",sep="")
          }
        }
      }
      if(mys@samp.t.p[1] < mys@alpha){
        mys.samp.t.sig<-T
        mys.samp.t.sig.sign<-"<"
        mys.samp.t.verb<-"statistically significant"
        mys.conclusion<-"We reject the Null Hypothesis and accept the Alternative Hypothesis."
        mys.samp.t.verbose.testing<-paste("\begin{itemize}\item $H_0: \, \mu_1-\mu_0=",mys@null,"$\item $H_a: \, \mu_1-\mu_0 ",mys@samp.t.sig.sign,mys@null,"$\item $\alpha=",mys@alpha,"$\item The pooled standard deviation is $s=\sqrt{\frac{s_1^2 \cdot (n_1-1)+s_0^2 \cdot (n_0-1)}{n_1+n_0-2}}=\sqrt{\frac{",mys@samp.sd.1,"^2 \cdot (",mys@samp.size.1,"-1)+",mys@samp.sd.0,"^2 \cdot (",mys@samp.size.0,"-1)}{",mys@samp.size.1,"+",mys@samp.size.0,"-2}}=",mys@pooled.s,"$ \item $t=\frac{\bar x_1-\bar x_0-",mys@null,"}{SE_{\bar X_1-\bar X_0}}=\frac{",mys@estimator,"-",mys@null,"}{\sqrt{\frac{",mys@pooled.s,"^2}{",mys@samp.size.1,"}+\frac{",mys@pooled.s,"^2}{",mys@samp.size.0,"}}}=\frac{",mys@numerator,"}{",mys@se.estimator,"}=",samp.t,"$\item ",mys@samp.t.pvalue,"\item ",mys@conclusion,"\end{itemize}",sep="")
      }else{
        mys.samp.t.sig<-F
        mys.samp.t.sig.sign<-">"
        mys.samp.t.verb<-"not statistically significant"
        mys.conclusion<-"We fail to reject the Null Hypothesis."
        mys.samp.t.verbose.testing<-paste("\begin{itemize}\item $H_0: \, \mu_1-\mu_0=",mys@null,"$\item $H_a: \, \mu_1-\mu_0 ",mys@samp.t.sig.sign,mys@null,"$\item $\alpha=",mys@alpha,"$\item The pooled standard deviation is $s=\sqrt{\frac{s_1^2 \cdot (n_1-1)+s_0^2 \cdot (n_0-1)}{n_1+n_0-2}}=\sqrt{\frac{",mys@samp.sd.1,"^2 \cdot (",mys@samp.size.1,"-1)+",mys@samp.sd.0,"^2 \cdot (",mys@samp.size.0,"-1)}{",mys@samp.size.1,"+",mys@samp.size.0,"-2}}=",mys@pooled.s,"$ \item $t=\frac{\bar x_1-\bar x_0-",mys@null,"}{SE_{\bar X_1-\bar X_0}}=\frac{",mys@estimator,"-",mys@null,"}{\sqrt{\frac{",mys@samp.sd.1,"^2}{",mys@samp.size.1,"}+\frac{",mys@samp.sd.0,"^2}{",mys@samp.size.0,"}}}=\frac{",mys@numerator,"}{",mys@se.estimator,"}=",samp.t,"$\item ",mys@samp.t.pvalue,"\item ",mys@conclusion,"\end{itemize}",sep="")
      }
    }
    else {
      # T distribution different standard deviations
      mys@df<-min(mys@samp.size.0,mys@samp.size.1)-1
      mys@se.estimator<-ceiling(1000*sqrt(mys@samp.sd.0^2/mys@samp.size.0+mys@samp.sd.1^2/mys@samp.size.1))/1000
      ftb<-c(1000,100,80,60,50,40,30:1)
      ptb<-c(50,(5:1)*5,2.5,2,1,0.5,0.25,0.1,0.05,0)/100
      mys@df.D<-ftb[(ftb<=mys@samp.df)][1]
      mys@tstar<-round(abs(qt(1-mys@confidence.level/100,mys@df.D)),3)
      mys@margin<-ceiling(1000*mys@tstar*mys@se.estimator)/1000
      mys@ci<-c(mys@estimator-mys@margin,mys@estimator+mys@margin)
      mys@ci.lb<-mys@ci[1]
      mys@ci.ub<-mys@ci[2]
      mys@verbose.confidence.interval<-paste("The confidence interval is centered at the value of the estimator is the difference between sample means, that is $\bar x_1-\bar x_0=",mys@samp.mean.1,"-",mys@samp.mean.0,"=",mys@estimator,"$. The standard error of the estimator is $SE_{\bar X_1-\bar X_0}=\sqrt{\frac{s_1^2}{n_1}+\frac{s_0^2}{n_0}}=\sqrt{\frac{",mys@samp.sd.1,"^2}{",mys@samp.size.1,"}+\frac{"mys@samp.sd.1,"^2}{",mys@samp.size.1,"}}=",mys@se.estimator,"$. The number of degrees of freedom is the smaller number of degrees of freedom in each sample, which in this case is ",mys@df,". We will use a conservative number of degrees of freedom from Table D, that is ",mys.df.D,". The critical value for a $",mys@confidence.level,"$\% confidence interval is $t^*="mys@t.star,"$, so the margin of error is $m=t^* \cdot SE_{\bar X_1-\bar X_0}=",mys@t.star," \cdot ",mys@se.estimator,"=",mys@margin,"$. The confidence interval is $[\bar x_1-\bar x_0-m,\bar x_1-\bar x_0+m]=[",mys@estimator,"-",mys@margin","mys@estimator,"+",mys@margin,"]=[",mys@ci.lb,",",mys@ci.ub,"]$.",sep="")
      # Hypothesis testing
      mys@numerator<-mys@estimator-mys@null
      mys@samp.t<-ceiling(100*mys@numerator/mys@se.estimator)/100
      if(mys@two.tail==1) {
        mys@samp.t.p<-2*c(ptb[(ptb<abs(mys@samp.t))][1],ptb[(ptb>abs(mys@samp.t))][length(ptb[(ptb>abs(mys@samp.t))])])
        mys@samp.t.pvalue<-paste("The p-value is $2 \cdot P(T>|t|)=2 \cdot P(T>|",mys@samp.t,"|)$ in the t-distribution with ",mys@df," degrees of freedom. Using ",mys@df.D," degrees of freedom from Table D, $P(T>",abs(mys@samp.t),") \in (",mys@samp.t.p[1]/2,",",mys@samp.t.p[2]/2,")$ so the p-value is in the $(",mys@samp.t.p[1],",",mys@samp.t.p[2],")$ interval.",sep="")
      }else{
        if(mys@greater.than==1) {
          if(mys@samp.t>0) {
            mys@samp.t.p<-c(ptb[(ptb<abs(mys@samp.t))][1],ptb[(ptb>abs(mys@samp.t))][length(ptb[(ptb>abs(mys@samp.t))])])
            mys@samp.t.pvalue<-paste("The p-value is $P(T>t)=P(T>",mys@samp.t,")$ in the t-distribution with ",mys@df," degrees of freedom. Using ",mys@df.D," degrees of freedom from Table D, $P(T>",mys@samp.t,") \in (",mys@samp.t.p[1],",",mys@samp.t.p[2],")$.",sep="")
          }else{
            mys@samp.t.p<-1-c(ptb[(ptb>abs(mys@samp.t))][length(ptb[(ptb>abs(mys@samp.t))])],ptb[(ptb<abs(mys@samp.t))][1])
            mys@samp.t.pvalue<-paste("The p-value is $P(T<t)=P(T<",mys@samp.t,")$ in the t-distribution with ",mys@df," degrees of freedom. That is larger than 50\%. More precisely, using ",mys@df.D," degrees of freedom from Table D, $P(T>",mys@samp.t,")=P(T<",-mys@samp.t,")=1-P(T>",-mys@samp.t,") \in (",mys@samp.t.p[1],",",mys@samp.t.p[2],")$.",sep="")
          }
        }else {
          if(mys@samp.t<0) {
            mys@samp.t.p<-c(ptb[(ptb<abs(mys@samp.t))][1],ptb[(ptb>abs(mys@samp.t))][length(ptb[(ptb>abs(mys@samp.t))])])
            mys@samp.t.pvalue<-paste("The p-value is $P(T<t)=P(T<",mys@samp.t,")=P(T>",-mys@samp.t,")$ in the t-distribution with ",mys@df," degrees of freedom. Using ",mys@df.D," degrees of freedom from Table D, $P(T>",-mys@samp.t,") \in (",mys@samp.t.p[1],",",mys@samp.t.p[2],")$.",sep="")
          }else {
            mys@samp.t.p<-1-c(ptb[(ptb>abs(mys@samp.t))][length(ptb[(ptb>abs(mys@samp.t))])],ptb[(ptb<abs(mys@samp.t))][1])
            mys@samp.t.pvalue<-paste("The p-value is $P(T<t)=P(T<",mys@samp.t,")$ in the t-distribution with ",mys@df," degrees of freedom. That is larger than 50\%. More precisely, using ",mys@df.D," degrees of freedom from Table D, $P(T<",mys@samp.t,")=1-P(T>",mys@samp.t,") \in (",mys@samp.t.p[1],",",mys@samp.t.p[2],")$.",sep="")
          }
        }
      }
      if(mys@samp.t.p[1] < mys@alpha){
        mys.samp.t.sig<-T
        mys.samp.t.sig.sign<-"<"
        mys.samp.t.verb<-"statistically significant"
        mys.conclusion<-"We reject the Null Hypothesis and accept the Alternative Hypothesis."
        mys.samp.t.verbose.testing<-paste("\begin{itemize}\item $H_0: \, \mu_1-\mu_0=",mys@null,"$\item $H_a: \, \mu_1-\mu_0 ",mys@samp.t.sig.sign,mys@null,"$\item $\alpha=",mys@alpha,"$\item $t=\frac{\bar x_1-\bar x_0-",mys@null,"}{SE_{\bar X_1-\bar X_0}}=\frac{",mys@estimator,"-",mys@null,"}{\sqrt{\frac{",mys@samp.sd.1,"^2}{",mys@samp.size.1,"}+\frac{",mys@samp.sd.0,"^2}{",mys@samp.size.0,"}}}=\frac{",mys@numerator,"}{",mys@se.estimator,"}=",samp.t,"$\item ",mys@samp.t.pvalue,"\item ",mys@conclusion,"\end{itemize}",sep="")
      }else{
        mys.samp.t.sig<-F
        mys.samp.t.sig.sign<-">"
        mys.samp.t.verb<-"not statistically significant"
        mys.conclusion<-"We fail to reject the Null Hypothesis."
        mys.samp.t.verbose.testing<-paste("\begin{itemize}\item $H_0: \, \mu_1-\mu_0=",mys@null,"$\item $H_a: \, \mu_1-\mu_0 ",mys@samp.t.sig.sign,mys@null,"$\item $\alpha=",mys@alpha,"$\item $t=\frac{\bar x_1-\bar x_0-",mys@null,"}{SE_{\bar X_1-\bar X_0}}=\frac{",mys@estimator,"-",mys@null,"}{\sqrt{\frac{",mys@samp.sd.1,"^2}{",mys@samp.size.1,"}+\frac{",mys@samp.sd.0,"^2}{",mys@samp.size.0,"}}}=\frac{",mys@numerator,"}{",mys@se.estimator,"}=",samp.t,"$\item ",mys@samp.t.pvalue,"\item ",mys@conclusion,"\end{itemize}",sep="")
      }
    }
  }
}

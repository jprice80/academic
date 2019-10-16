#' Descriptive Statistics on variables within a data frame
#'
#' @description The  \code{descriptives} function is used to perform descriptive statistics on a set of variables in a \code{\link{data.frame}}.
#' This can include descriptive statistics by group when including a vector of categorical variables in the groupby= argument.
#'
#' @param data a \code{\link{data.frame}} containing columns of variables.
#' @param vars a \code{\link{vector}} of variable names existing in the dataframe to perform descriptive statistics on.
#' @param groupby an optional \code{\link{vector}} containing factor names within a dataframe used for by group processing.
#' @param conf.level the confidence level for mean and median confidence intervals.
#' @param medianCI a TRUE/FALSE boolean value indicating whether or not to produce bootstrapped confidence intervals on the median (computationally intensive).
#'
#' @return A \code{\link{list}} containing descriptive statistics for each variable listed in the \code{vars} statement.
#' @export
#'
#' @examples
#' descriptives(data=mtcars, vars=c("mpg","disp"), groupby=c("gear","am"))
descriptives<-function(data, vars, groupby=NULL, conf.level=0.95, medianCI=FALSE){

  #Check to insure data is a dataframe
  if(!is.data.frame(data)){
    stop(paste("The object", data, "is not a dataframe"));
  }

  # check to see if all vars are numeric
  temp <- data %>% select(vars)
  temp <- as.vector(sapply(temp,is.numeric))

  if(all(temp)==FALSE){
    stop(paste("All variables listed in the vars= statement are not numeric"));
  }


  #Start actual function
  out<-list()

  for(i in 1:length(vars)){

    varname<-vars[i]

    if(!is.null(groupby)){

      dat<-data %>% group_by(.dots=groupby) %>%
        summarise(ntotal = n(),
                  nmiss = sum(is.na(!!sym(vars[i]))),
                  mean = mean(!!sym(vars[i]), na.rm = TRUE),
                  sd = sd(!!sym(vars[i]), na.rm = TRUE),
                  stderr = sd/sqrt(n()),
                  median = median(!!sym(vars[i]), na.rm = TRUE),
                  min = min(!!sym(vars[i]), na.rm = TRUE),
                  max = max(!!sym(vars[i]), na.rm = TRUE),
                  pct25 = quantile(!!sym(vars[i]), probs=0.25, na.rm=TRUE),
                  pct75 = quantile(!!sym(vars[i]), probs=0.75, na.rm=TRUE),
                  IQR = pct75 - pct25)
      #LCL = mean - qt(1 - (0.05 / 2), ntotal - 1) * stderr,
      #UCL = mean + qt(1 - (0.05 / 2), ntotal - 1) * stderr,
      #LCLmed = MedianCI(!!sym(vars[i]), method="boot", na.rm=TRUE)[2],
      #UCLmed = MedianCI(!!sym(vars[i]), method="boot", na.rm=TRUE)[3])

      #CL mean calcs
      clm<-data %>% group_by(.dots=groupby) %>% summarise(list(DescTools::MeanCI(!!sym(vars[i]), method="classic", na.rm=TRUE, conf.level = conf.level)))
      clm<-clm[[length(clm)]]
      clm<-unlist(clm)
      nms<-names(clm)
      clm<-data.frame(names=nms, clm)

      lwrm<-clm %>% filter(names=="lwr.ci")
      uprm<-clm %>% filter(names=="upr.ci")

      quantile

      if(medianCI==TRUE){
      #CL median calcs
        clmed<-data %>% group_by(.dots=groupby) %>% summarise(list(DescTools::MedianCI(!!sym(vars[i]), method="boot", na.rm=TRUE, R=2000, conf.level = conf.level)))
        clmed<-clmed[[length(clmed)]]
        clmed<-unlist(clmed)
        nms<-names(clmed)
        clmed<-data.frame(names=nms, clmed)

        lwrmed<-clmed %>% filter(names=="lwr.ci")
        uprmed<-clmed %>% filter(names=="upr.ci")

        #combine CLmean and CLmed components
        confint<-data.frame(LCLmean=lwrm[,2], UCLmean=uprm[,2], LCLmed=lwrmed[,2], UCLmed=uprmed[,2])
        dat<-cbind(as.data.frame(dat), confint)
      } else {
        confint<-data.frame(LCLmean=lwrm[,2], UCLmean=uprm[,2])
        dat<-cbind(as.data.frame(dat), confint)
      }


    } else {

      dat<-data %>%
        summarise(ntoal = n(),
                  nmiss = sum(is.na(!!sym(vars[i]))),
                  mean = mean(!!sym(vars[i]), na.rm = TRUE),
                  sd = sd(!!sym(vars[i]), na.rm = TRUE),
                  stderr = sd/sqrt(n()),
                  median = median(!!sym(vars[i]), na.rm = TRUE),
                  min = min(!!sym(vars[i]), na.rm = TRUE),
                  max = max(!!sym(vars[i]), na.rm = TRUE),
                  pct25 = quantile(!!sym(vars[i]), probs=0.25, na.rm=TRUE),
                  pct75 = quantile(!!sym(vars[i]), probs=0.75, na.rm=TRUE),
                  IQR = pct75 - pct25)
      #LCL = mean - qt(1 - (0.05 / 2), n - 1) * stderr,
      #UCL = mean + qt(1 - (0.05 / 2), n - 1) * stderr,
      #LCLmed = MedianCI(!!sym(vars[i]), method="exact", na.rm=TRUE)[2],
      #UCLmed = MedianCI(!!sym(vars[i]), method="exact", na.rm=TRUE)[3])

      #CL mean calcs
      clm<-data %>% summarise(list(DescTools::MeanCI(!!sym(vars[i]), method="classic", na.rm=TRUE, conf.level = conf.level)))
      clm<-clm[[length(clm)]]
      clm<-unlist(clm)
      nms<-names(clm)
      clm<-data.frame(names=nms, clm)

      lwrm<-clm %>% filter(names=="lwr.ci")
      uprm<-clm %>% filter(names=="upr.ci")

      if(medianCI==TRUE){

        #CL median calcs
        clmed<-data %>% summarise(list(DescTools::MedianCI(!!sym(vars[i]), method="boot", na.rm=TRUE, R=2000, conf.level = conf.level)))
        clmed<-clmed[[length(clmed)]]
        clmed<-unlist(clmed)
        nms<-names(clmed)
        clmed<-data.frame(names=nms, clmed)

        lwrmed<-clmed %>% filter(names=="lwr.ci")
        uprmed<-clmed %>% filter(names=="upr.ci")

        #combine CLmean and CLmed components
        confint<-data.frame(LCLmean=lwrm[,2], UCLmean=uprm[,2], LCLmed=lwrmed[,2], UCLmed=uprmed[,2])
        dat<-cbind(as.data.frame(dat), confint)
      } else {
        confint<-data.frame(LCLmean=lwrm[,2], UCLmean=uprm[,2])
        dat<-cbind(as.data.frame(dat), confint)
      }
    }

    dat<-cbind(variable=varname, as.data.frame(dat))
    row.names(dat)<-NULL

    out[[varname]]<-dat
  }

  class(out)<-"aca_desc"

  return(out)
}



#' @export
summary.aca_desc<-function(x){
  if(class(x) != "aca_desc"){
    stop(paste(x, "not an aca_desc object"))
  }

  for(i in 1:length(x)){
    print(x[[i]], row.names=FALSE)
    cat("\n")
  }
}





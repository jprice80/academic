#' Normality Tests for variables within a data frame
#'
#' @description The \code{normality} function is used to perform normality tests on a set of variables in a \code{\link{data.frame}}.
#' This can include by group normality tests when including a vector of categorical variables in the groupby= argument.
#' The Shapiro-Wilk Test for normality will be performed in all cases. If the sample size of all groups >= 20,
#' then the D'Agostino Omnibus normality test will also be performed.
#'
#' @param data a \code{\link{data.frame}} containing columns of variables.
#' @param vars a \code{\link{vector}} of variable names existing in the dataframe to perform normality tests on.
#' @param groupby an optional \code{\link{vector}} containing factor names within a dataframe used for by group processing.
#' @param conf.level the confidence level boostrapped confidence intervals for the QQ plot. The default value is 0.95.
#'
#' @return A \code{\link{list}} containing normality tests tresults for each variable listed in the \code{vars} statement.
#' Histograms and QQ plots are also returned.
#' @export
#'
#' @examples
#' normality(data=mtcars, vars=c("mpg","disp"), groupby=c("vs"))
normality<-function(data, vars, groupby=NULL, conf.level=0.95){

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

  #Setup output template
  out<-list()
  out[["tabs"]]<-list()
  out[["plots"]]<-list()

  tabs<-list()
  plots<-list()
  tab<-data.frame()
  if(is.null(groupby)){

    for(i in 1:length(vars)){

      varname<-vars[i]

      #total number of complete cases
      tot<-sum(complete.cases(mtcars[[vars[i]]])==TRUE)

      if(tot >= 20){
        dat<-data %>%
          summarise(list(sw = fBasics::shapiroTest(!!sym(vars[i]))@test),
                    list(da = fBasics::dagoTest(!!sym(vars[i]))@test))

        W<-round(as.numeric(dat[[1]]$sw$statistic), 4)
        swpval<-round(as.numeric(dat[[1]]$sw$p.value), 4)
        dachisq<-round(as.numeric(dat[[2]]$da$statistic[1]), 4)
        dapval<-round(as.numeric(dat[[2]]$da$p.value[1]), 4)

      } else {
        dat<-data %>%
          summarise(list(sw = fBasics::shapiroTest(!!sym(vars[i]))@test))

        W<-round(as.numeric(dat[[1]]$sw$statistic), 4)
        swpval<-round(as.numeric(dat[[1]]$sw$p.value), 4)
        dachisq<-NA
        dapval<-NA
      }

      currow<-data.frame(variable = varname, group="Overall", Shapiro.Wilk.W = W, Shapiro.Wilk.Pvalue = swpval,
                         DAgostino.Chisq = dachisq, DAgostino.Pvalue = dapval)

      tab<-rbind(currow, tab)

      plt_hist<-ggplot(data, aes(x = !!sym(vars[i]))) +
        geom_histogram(color="darkblue", fill="lightblue",
                       binwidth = function(x) (max(x)-min(x))/grDevices::nclass.scott(x)) +
        ggtitle(paste0("Histogram of ", vars[[i]])) + theme_bw() + labs(y = "Frequency") +
        theme(plot.title = element_text(size=10, hjust = 0.5, face="bold"),
              axis.title.x = element_text(size=8, color="black", face="bold"),
              axis.title.y = element_text(size=8, color="black", face="bold"))

      #print(plt_hist)

      plt_qq<-ggplot(data = data, mapping = aes(sample = !!sym(vars[i]))) +
        qqplotr::stat_qq_band(alpha=0.5, conf=0.95, qtype=1, bandType = "boot", fill="red") +
        qqplotr::stat_qq_line(identity=TRUE) +
        qqplotr::stat_qq_point(col="black") +
        ggtitle(paste0("Normal QQ Plot of ", vars[[i]], " with ", conf.level*100,"% Boostrapped CIs")) +
        labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw() +
        theme(plot.title = element_text(size=10, hjust = 0.5, face="bold"),
              axis.title.x = element_text(size=8, color="black", face="bold"),
              axis.title.y = element_text(size=8, color="black", face="bold"))

      #print(plt_qq)

      plots[[varname]]<-list(plt_hist, plt_qq)
    }

   out$tabs$overall<-tab
   out$plots<-plots

  } else {

    for(i in 1:length(vars)){

      varname<-vars[i]

      ns<-data %>% group_by(.dots=groupby) %>%
        summarise(ntotal = n(),
                  nmiss = sum(is.na(!!sym(vars[i]))),
                  nvalid = ntotal-nmiss)

      tot<-min(ns$nvalid, na.rm = TRUE)

      if(tot >= 20){
        dat<-data %>% group_by(.dots=groupby) %>%
          summarise(list(sw = fBasics::shapiroTest(!!sym(vars[i]))@test),
                    list(da = fBasics::dagoTest(!!sym(vars[i]))@test))

        ns$ntotal<-NULL
        ns$nmiss<-NULL
        ns$nvalid<-NULL

        tab<-data.frame(varname, ns, Shapiro.Wilk.W = NA, Shapiro.Wilk.Pvalue = NA, DAgostino.Chisq = NA, DAgostino.Pvalue = NA)

        for(j in 1:nrow(ns)){
          tab$Shapiro.Wilk.W[j]<-round(as.numeric(dat[[j]]$sw$statistic), 4)
          tab$Shapiro.Wilk.Pvalue[j]<-round(as.numeric(dat[[j]]$sw$p.value), 4)
          tab$DAgostino.Chisq[j]<-round(as.numeric(dat[[j]]$da$statistic[1]), 4)
          tab$DAgostino.Pvalue[j]<-round(as.numeric(dat[[j]]$da$p.value[1]), 4)
        }

        out$tabs[[varname]]<-tab

      } else {
        dat<-data %>% group_by(.dots=groupby) %>%
          summarise(list(sw = fBasics::shapiroTest(!!sym(vars[i]))@test))

        dat<-dat[[length(dat)]]

        ns$ntotal<-NULL
        ns$nmiss<-NULL
        ns$nvalid<-NULL

        tab<-data.frame(varname, ns, Shapiro.Wilk.W = NA, Shapiro.Wilk.Pvalue = NA, DAgostino.Chisq = NA, DAgostino.Pvalue = NA)

        for(j in 1:nrow(ns)){
          tab$Shapiro.Wilk.W[j]<-round(as.numeric(dat[[j]]$statistic), 4)
          tab$Shapiro.Wilk.Pvalue[j]<-round(as.numeric(dat[[j]]$p.value), 4)
        }

        out$tabs[[varname]]<-tab
      }
    }

    #create plots for each outcome
    for(i in 1:length(vars)){

      plt_hist<-data %>% group_by(.dots=groupby) %>%
        do(plots=ggplot(data=.) + aes(x = !!sym(vars[i])) +
        geom_histogram(color="darkblue", fill="lightblue", binwidth = function(x) (max(x)-min(x))/grDevices::nclass.scott(x)) +
        ggtitle(paste0("Histogram of ", vars[[i]])) + theme_bw() + labs(y = "Frequency") +
        theme(plot.title = element_text(size=10, hjust = 0.5, face="bold"),
              axis.title.x = element_text(size=8, color="black", face="bold"),
              axis.title.y = element_text(size=8, color="black", face="bold")))

      plt_qq<-data %>% group_by(.dots=groupby) %>%
        do(plots=ggplot(data = ., mapping = aes(sample = !!sym(vars[i]))) +
             qqplotr::stat_qq_band(alpha=0.5, conf=conf.level, qtype=1, bandType = "boot", fill="red") +
             qqplotr::stat_qq_line(identity=TRUE) +
             qqplotr::stat_qq_point(col="black") +
             ggtitle(paste0("Normal QQ Plot of ", vars[[i]], " with ", conf.level*100,"% Boostrapped CIs")) +
             labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + theme_bw() +
             theme(plot.title = element_text(size=10, hjust = 0.5, face="bold"),
                   axis.title.x = element_text(size=8, color="black", face="bold"),
                   axis.title.y = element_text(size=8, color="black", face="bold")))

      #Reorder plots to correctly pair Histogram and QQs and identify levels
      for(i in 1:nrow(plt_hist)){
        lev <- apply(ns[i,], 1, paste, collapse = "-")

        one<-plt_hist[[ncol(plt_hist)]][[i]]
        one<-one+ggtitle(paste0("Histogram of ", one$labels$x, lev)) +
          theme(plot.title = element_text(size=10, hjust = 0.5, face="bold"))

        two<-plt_qq[[ncol(plt_qq)]][[i]]
        two<-two+ggtitle(paste0("Normal QQ Plot of ", two$labels$sample, " ", lev, " with ", conf.level*100,"% Boostrapped CIs")) +
          theme(plot.title = element_text(size=10, hjust = 0.5, face="bold"))

        varname<-one$labels$x
        out$plots[[varname]][[i]]<-list(one,two)
      }
    }
  }


  class(out)<-"aca_norm"
  return(out)
}


#' @export
summary.aca_norm<-function(object){
  if(class(object) != "aca_norm"){
    stop(paste(object, "not an aca_norm object"))
  }

  for(i in 1:length(object[["tabs"]])){
    print(object[["tabs"]][[i]], row.names=FALSE)
    cat("\n")
  }

  for(i in 1:length(object[["plots"]])){
    for(j in 1:length(object[["plots"]][[i]])){
      do.call(gridExtra::grid.arrange, c(object[["plots"]][[i]][[j]], ncol=2))
    }
  }
}


#' @export
print.aca_norm<-function(object){
  summary(object)
}

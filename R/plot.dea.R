#' @title Plot
#'   
#'   
#' @description Plot some attribute of a dea model
#' 
#' @param x An object of class \code{"dea"} obtained by a dea model function.
#' @param ... Ignored, for compatibility issues.
#' 
#'   
#' @return Depending on the model it returns a single data.frame containing: efficiencies, 
#' slacks, lambdas, targets, references or a list of data.frames with the cross-efficiencies computed 
#' with different methods (Arbitrary, Method II or Method III (see CITA)) or, in case the model is a
#'  malmquist index, a single data.frame with the coefficients for the different periods.       
#' @examples
#' data_example <- read_data(datadea = Fortune500, dmus = 1, inputs = 2:4, outputs = 5:6)
#' 
#' deamodel <- model_basic(data_example)
#' plot(deamodel)
#' @method plot dea
#' @importFrom igraph graph.adjacency degree "V<-" "V" plot.igraph
#' @importFrom ggplot2 ggplot geom_line geom_histogram geom_col facet_wrap scale_x_discrete theme_bw scale_fill_identity xlab ylab coord_flip aes
#' @importFrom methods show
#' @importFrom graphics plot
#' @importFrom stats runif
#' @importFrom gridExtra grid.arrange
#' @import plotly
#' @export
#' 

plot.dea <- function(x, ...){
  object <- x
  if(!is.dea(object)){
    stop("Input should be of class dea!")
  }

 modelname <- object$modelname
 # For check CRAN pass...
 Period <- value <- DMU <- Pos <- Count <- NULL
 `V<-` <- NULL
 
 if(modelname %in% c("malmquist","cross_efficiency")){
   if(modelname == "malmquist"){
     malmdata <- summary(object, exportExcel = FALSE)
     results <- malmdata$Results
     sumres <- malmdata$means_by_period
     colnames(results) <- c("Period","DMU","Efficiency change", "Technical change", "Productivity change","Scale change","Malmquist index")
     colnames(sumres) <- c("Period","Efficiency change", "Technical change", "Productivity change","Scale change","Malmquist index")
     results %>% gather(key= "index", value = "value", -c("Period","DMU")) %>% 
       ggplot(aes(x = Period, y = value, col = DMU)) + geom_line(aes(group = DMU)) + facet_wrap(~index, scales = "free") +
       ylab("") + theme_bw() + ggtitle("Change indices") -> resplot
     sumres %>% gather(key= "index", value = "value", -c("Period")) %>% 
       ggplot(aes(x = Period, y = value, col = index)) + geom_line(aes(group = index)) + theme_bw() +
       ylab("")  + ggtitle("Geometric means by period") -> sumplot
     
     invisible(readline(prompt="Press [enter] to continue"))
     ggplotly(resplot)
     invisible(readline(prompt="Press [enter] to continue"))
     ggplotly(sumplot)
   }else{
     results <- list(Arb = object$Arbitrary$cross_eff,
     M2_agg = object$M2_agg$cross_eff,
     M2_ben = object$M2_ben$cross_eff,
     M3_agg = object$M3_agg$cross_eff,
     M3_ben = object$M3_ben$cross_eff)
     titles <- list("Arbitrary Method", "Method II - Aggresive", "Method II - Benevolent",
                    "Method III - Aggresive", "Method III - Benevolent")
     titles[sapply(results, is.null)] <- NULL 
    results[sapply(results, is.null)] <- NULL
    
      for(i in seq_along(results)){
        invisible(readline(prompt="Press [enter] for next plot"))
      xlab <- ylab <- colnames(results[[i]])
      p <- plot_ly(x = xlab, y = rev(ylab), z  = results[[i]][nrow(results[[i]]):1,], type = "heatmap" ) %>% layout(title = titles[[i]])
      print(p)
      }
   }
 }else{
   # Efficiencies histogram ----------
   if(!modelname %in% c("nonradial", "deaps")){
   eff <- data.frame(DMU = object$data$dmunames, eff = efficiencies(object))
   eff <- eff[complete.cases(eff),]
   #eff$iseff <- ifelse(eff$eff<1, "Inefficient" , "Efficient")
   eff$iseff <- ifelse(eff$eff<1, 0 , 1)
   if(!modelname %in% c("supereff_basic", "sbmsupereff")){
     eff %>% filter(iseff == 0) %>% ggplot(aes(x = eff)) + 
       geom_histogram(breaks = c(seq(from = min(eff$eff), to = max(eff$eff[eff$eff<1]), 
                                     length.out = 10)),col = "white", 
                      aes(fill = ifelse(eff < 1, "red","lightgreen"))) + 
       theme_bw() + scale_fill_identity() + xlab("Efficiency") + ylab("Count") + 
       ggtitle("Non-efficient DMU distribution")-> p2
     
     eff %>% ggplot(aes(x = factor(iseff))) + geom_bar(aes(fill = ifelse(eff < 1, "red","lightgreen"))) + theme_bw() +
       scale_fill_identity() + xlab("") + scale_x_discrete(labels = c("Non-efficient","Efficient")) + ylab("Count") + 
       ggtitle("Efficient/Non Efficient DMUs") + geom_text(stat='count', aes(label=..count..), vjust=-0.5) -> p1
     grid.arrange(p1,p2,nrow = 1)
   }else{
     eff %>% ggplot(aes(x = eff)) + geom_histogram(bins = 10,
                                                   col = "white", 
                                                   aes(fill = ifelse(eff < 1, 
                                                                     "red",
                                                                     "lightgreen")
                                                   )
     ) + 
       theme_bw() + scale_fill_identity() + xlab("Efficiency") + ylab("Count") + 
       facet_wrap(~ifelse(iseff==0,"Inefficient","Efficient"), scales = "free") -> effplot
     show(effplot)
     }
   
   
   }else{
     eff <- data.frame(DMU = object$data$dmunames, eff = efficiencies(object))
     effmelted <- eff %>% gather(key = "Aspect", value = "eff", -DMU) %>% mutate(iseff = ifelse(eff<1, 0 , 1)) 
     effmelted$Aspect <- gsub("eff.", "", x = effmelted$Aspect)
     effmelted %>% 
       ggplot(aes(x = eff)) + geom_histogram(breaks = c(seq(from = min(effmelted$eff), to = max(effmelted$eff[effmelted$eff<1]), 
                                                            length.out = 10), 1.05),#bins = 10,
                                             col = "white", 
                                             aes(fill = ifelse(eff < 1, "red","lightgreen"))) +
       theme_bw() + scale_fill_identity() + xlab("Efficiency") + ylab("Count") + facet_wrap(~Aspect)
   }
   
   invisible(readline(prompt="Press [enter] to continue"))
   # Reference ranking --------------
   if(!modelname %in% c("supereff_basic","sbmsupereff")){
   ref <- references(object) 
   lmbd <- lambdas(object)
   dmunames <- object$data$dmunames
   refnames <- unique(unlist(lapply(ref, function (x) names(x))))
   urefnames <- names(ref)
   effdmus <- dmunames[which(! dmunames %in% urefnames)]
  # if(!modelname %in% c("nonradial", "deaps")){
   
   #efficient <- which(efficiencies(object) > (1-1e-6))
   #effdmus <- dmunames[efficient]
   
   RefMat <- matrix(0, nrow = length(urefnames), ncol = length(effdmus),dimnames = list( urefnames, sort(effdmus)))
   RefMat[urefnames,refnames] <- round(lmbd[urefnames, refnames],4)
   RefMatl <- RefMat>0
   ranking <- sort(colSums(RefMatl))
   realcount <- ranking
   ranking[ranking==0] <- min(ranking[ranking>0])*1e-2
   effnames <- names(ranking)
   
   rkdf <- data.frame(DMU = effdmus, Count = ranking, realcount = realcount, Pos = seq(length(effdmus)))
   
   rkdf %>% ggplot(aes(x = as.factor(Pos), y = Count)) + geom_col(aes(fill = Count)) + coord_flip() + 
     scale_x_discrete(labels = rkdf$DMU, drop = FALSE) + theme_bw() + xlab("Efficient DMUs") + 
     ylab("# times appearing in reference sets") + geom_text(aes(label = realcount), hjust=-0.5) +
     guides(fill=FALSE) -> refplot
   show(refplot)
   invisible(readline(prompt="Press [enter] to continue"))
   }
  # }else{
     
     
     
     
   #}
   
   # Reference Graph ------------------------
   
   if(!modelname %in% c("supereff_basic","sbmsupereff")){
   lmbd <- lmbd[complete.cases(lmbd),]
   adjmatrix <- lmbd>0
   G <- graph.adjacency(adjmatrix,diag = FALSE )
   
   efficient <- which(dmunames %in% effdmus)
   non_efficient <- which(dmunames %in% urefnames)
   
   
   rnd <- runif(1)
   nefflocX <- 2*cos(2*pi*(0:(length(non_efficient)-1))/length(non_efficient) + rnd)
   nefflocY <- 2*sin(2*pi*(0:(length(non_efficient)-1))/length(non_efficient)+ rnd)
   efflocX <- 4*cos(2*pi*(0:(length(efficient)-1))/length(efficient))
   efflocY <- 4*sin(2*pi*(0:(length(efficient)-1))/length(efficient))
   
   relations <- degree(G, mode = "in") - degree(G, mode = "out")
   
   
   locX <- numeric(length(relations))
   locY <- numeric(length(relations))
   locX[efficient] <- efflocX
   locX[non_efficient] <- nefflocX
   
   locY[efficient] <- efflocY
   locY[non_efficient] <- nefflocY
   locations <- cbind(locX ,locY)
   
   V(G)$color <- numeric(length(dmunames))
   V(G)$color[efficient] <- rep("green", length(efficient))
   V(G)$color[non_efficient] <- rep("red", length(non_efficient))
   lmbd2 <- lmbd / rowSums(lmbd) 
   V(G)$size <- colSums(lmbd2)^1.1+10#0.6*degree(G, mode = "in") + 10
   
   
   
   plot(G, layout = locations, xlim = c(-2,2), ylim = c(-.4,.4),
        edge.arrow.size=1, edge.arrow.width = 0.5,edge.curved=FALSE)
   }

 }
 
 
}
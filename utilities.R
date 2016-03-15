discretize <- function(values_cont, n_points) {

  maximal = max(values_cont)
  minimal = min(values_cont)
  
  step <- (maximal - minimal) / (n_points - 1)
  break_points <- seq(from = minimal, to = maximal, by = step)
  
  values_disc <- vector()
  delta <- (maximal - minimal)
  value <- vector()
  
  for (i in 1:length(values_cont)){
    delta <- (maximal - minimal)
    for (j in 1:length(break_points)){
      if (abs(values_cont[i] - break_points[j]) < delta){
        delta = abs(values_cont[i] - break_points[j])
        value = break_points[j]
      }
    }
    values_disc <- c(values_disc, value)
  }
  return(values_disc)
}


draw_plot_1 <- function(df, x1, y1){

  p <- ggplot(aes(df$fixed.acidity, df$fixed.acidity), data = df) + 
  geom_point(color = 'green4', alpha = 0.1) +
  geom_line(stat = 'summary', fun.y = mean) + 
  geom_smooth(method=lm) +
  theme(axis.text=element_text(size=12), axis.title = element_text(size=15))
  
  #return(list(p))
}  


find_lrt <- function(wf){
  lrt <- vector()
  for (i in names(wf)){
    s <- summary(wf[, i])
    if ((s[5] - s[3]) * 10 <= (s[6] - s[3])){
      lrt <- c(lrt,i)
    }
  }
  return (lrt)
}

split_train_test <- function(df, attrib, class, ratio_train){
  
  nr = nrow(df)
  nc = ncol(df)
  
  # Splitting dataset in training set (70%) and test set (30%)
  ntrain <- round(nr * ratio_train)
  tindex <- sample(nr,ntrain) # indices of training samples
  attribs_train <- attribs[tindex,]
  attribs_test <- attribs[-tindex,]
  class_train <- class[tindex]
  class_test <- class[-tindex]
  
  return(list(AttrTrain = attribs_train, ClassTrain = class_train, 
              AttrTest  = attribs_test,  ClassTest  = class_test))
}
  
metrics_1 <- function(tab){
  pc = 0
  den = 0
  for (i in c(1:dim(tab)[2])){
    pc <- pc + tab[i,i]
    den <- den + sum(tab[,i])
  }
  pc <- pc / den
  return(pc)
}

metrics_2 <- function(tab){
  pc = 0
  for (i in c(1:dim(tab)[2])){
    pc <- pc + tab[i,i] / sum(tab[,i])
  }
  pc <- pc / dim(tab)[1]
}
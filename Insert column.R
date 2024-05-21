insert <- function(dataset,inserted.vector,pos,col=TRUE) {  
  if (col==TRUE) {
    name<-deparse(substitute(dataset))
    vecname<-deparse(substitute(inserted.vector))
    df1<-dataset[,1:(pos-1)]
    df2<-dataset[,pos:ncol(dataset)]
    dataset<-cbind(df1,inserted.vector,df2)
    colnames(dataset)<-c(colnames(df1),vecname,colnames(df2))
    #  assign(name,dataset,envir=.GlobalEnv) } else {
    return(dataset) } else {
      name<-deparse(substitute(dataset))
      vecname<-deparse(substitute(inserted.vector))
      df1<-dataset[1:(pos-1),]
      df2<-dataset[pos:nrow(dataset),]
      dataset<-rbind(df1,inserted.vector,df2)
      rownames(dataset)<-c(rownames(df1),vecname,rownames(df2))
      #      assign(name,dataset,envir=.GlobalEnv) }              
      return(dataset)}
}
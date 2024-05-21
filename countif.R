countif<-function(vector) {
  df<-vector
  df<-as.data.frame(df)
  df$rownames<-as.numeric(rownames(df))
  df2<-merge(df, data.frame(table(df = df$df)), by = c("df"))
  df2 <- df2[order(df2$rownames),]
  out<-df2$Freq
  return(out)
}
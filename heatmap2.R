
load('sout_b.rda')
uind<- unique(indmat[,1:3])
for(k in 1:nrow(uind)){
  mid <- uind[k,'model']
  fid <- uind[k,'fixopt']
  sid <- uind[k,'scen']
  indmatTmp <- indmat[jobInd,]
  inds <- which(indmatTmp$model==mid & indmatTmp$fixopt==fid & indmatTmp$scen==sid)
  npx <- unique(unlist(lapply(x_flags[inds], length)))
  i <- inds[[1]]
  np <- length(nam <- x_nams[[i]])
  dim1 <- dim2 <- numeric();  foo <- character()
  for(j in 1:np){
    foo <- c(foo, rep(nam[j],  x_dims1[[i]][j]*x_dims2[[i]][j] ))
    dim1 <- c(dim1, rep(1:x_dims1[[i]][j], each=x_dims2[[i]][j]))
    dim2 <- c(dim2, rep(1:x_dims2[[i]][j], x_dims1[[i]][j]))
  }
  namtmp <- nam0 <- character()
  tmp <- as.data.frame(mat_x[inds, 1:npx])
  for(n in 1:length(foo)){
    if(foo[n]%in%c('log_Ru')) namtmp[n] <-'rec_u'
    if(foo[n]%in%c('dev_r')) namtmp[n] <-'rec_dev'
    if(foo[n]%in%c('log_N0')) namtmp[n] <-'N_u'
    if(foo[n]%in%c('dev_N0')) namtmp[n] <-'N_dev'
    if(foo[n]%in%c('log_sig_F')) namtmp[n] <-'q_sig'
    if(foo[n]%in%c('log_q1')) namtmp[n] <-'q_init'
    if(foo[n]%in%c('dev_F')) namtmp[n] <-'q_dev'
    if(foo[n]%in%c('tau')) namtmp[n] <-'sel_tau'
    if(foo[n]%in%c('log_eta')) namtmp[n] <-'sel_eta'
    if(foo[n]%in%c('H0')) namtmp[n] <-'move'
    if(foo[n]%in%c('Lamda0')) namtmp[n] <-'report'
    if(foo[n]%in%c('S_N')) namtmp[n] <-'natal'
    if(foo[n]%in%c('M')) namtmp[n] <-'M'
  }
  
  tmp0 <- data.frame(namtmp,dim1,dim2)
  QR<-ddply(tmp0, c("namtmp"), function(df)
    return(c(mean(df$dim1),mean(df$dim2),length(df$dim1))))
  dims<- character()
  for(n in 1:nrow(tmp0)){
    nam_n <- tmp0$namtmp[n]
    for(m in 1:nrow(QR)){
      nam_m <- QR$namtmp[m]
      if(nam_n==nam_m) {
        if(QR$V1[m]==1 & QR$V2[m]==1) dims[n]<-'' 
        else if(QR$V1[m]==1 & QR$V2[m]!=1) dims[n]<-paste0('_',dim2[n])
        else if(QR$V1[m]!=1 & QR$V2[m]==1) dims[n]<-paste0('_',dim1[n])
        else if(QR$V1[m]!=1 & QR$V2[m]!=1) dims[n]<-paste0('_',dim1[n],',',dim2[n])
      }
    }
  }
  
  nam0 <- paste0(namtmp,dims )
  names(tmp) <- nam0
  indNA <- which(apply(tmp, 2, function(x) all(is.na(x))|length(unique(x))==1))
  if(length(indNA)) {
    tmp <- tmp[,-indNA]
    namInd <- nam0[-indNA]
    namtmp <- namtmp[-indNA]
  }
  col1 <- brewer.pal(8, "Set3")
  color.map<-function(CATEGORY){
    if(CATEGORY=="rec_u" | CATEGORY=="rec_dev") col1[1] 
    else if(CATEGORY=="N_u" | CATEGORY=="N_dev") col1[2] 
    else if(CATEGORY=="q_sig" | CATEGORY=="q_init" | CATEGORY=="q_dev") col1[3]
    else if(CATEGORY=="sel_tau" | CATEGORY=="sel_eta") col1[4] 
    else if(CATEGORY=="move") col1[5]
    else if(CATEGORY=="report") col1[6]
    else if(CATEGORY=="natal") col1[7]
    else if(CATEGORY=="M") col1[8]
  }
  sidebarcolors <- unlist(lapply(namtmp, color.map))
  mapback<-function(CATEGORY){
    if(CATEGORY==col1[1]) c("rec_u,rec_dev")
    else if(CATEGORY==col1[2]) c("N_u,N_dev")
    else if(CATEGORY==col1[3]) c("q_sig,q_1,q_dev") 
    else if(CATEGORY==col1[4]) c("sel_tau,sel_eta")  
    else if(CATEGORY==col1[5]) c("move") 
    else if(CATEGORY==col1[6]) c("report") 
    else if(CATEGORY==col1[7]) c("natal") 
    else if(CATEGORY==col1[8]) c("M") 
  }
  leg<- unlist(lapply(sidebarcolors,mapback))
  corMat <- cor(tmp, use="complete.obs")
  
  #corrplot::corrplot(cor.dendlist(corMat), "pie", "lower")
  
  #corrplot(corMat, type = "lower")
  # image(corMat)
  #hmcol <- colorRampPalette(brewer.pal(9, "GnBu"))(100)
  pdf(paste0('case',k,'cor.pdf'),width=6,height=5,paper='special') 
  par(mfrow=c(1,1), mar=c(0,0,0,0)+.4, cex.axis=.9)
  wrapper <- function(x) rev(brewer.pal(x,'RdBu'))
  heatmap.2(corMat,
            cexRow=.8, 
            labCol=namInd, 
            labRow=namInd,
            breaks=10, 
            trace='none',
            col=wrapper,
            ColSideColors=sidebarcolors, 
            #RowSideColors=sidebarcolors, 
            key.title=NA,
            revC=TRUE,
            #xlab="parameters", ylab="parameters",
            dendrogram="col") #            Rowv=FALSE, Colv=TRUE
  legend(y=0.5, x=.01, xpd=TRUE,     
         legend = unique(leg),
         col = unique(sidebarcolors), 
         lty= 1,             
         lwd = 5,           
         cex=.7)
  dev.off()
}


# not run

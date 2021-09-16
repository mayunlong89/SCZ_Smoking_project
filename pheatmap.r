#@ 2021-09-16

#heatmap
#install.packages("pheatmap")
library(pheatmap)

pearson_new<- read.delim("data_for_brainSpan.txt",header = T)

pearson_new2<-pearson_new[,-1]  ####去除第一列

mat_pearson<-as.matrix(pearson_new2)

row.names(mat_pearson)<- pearson_new[,1]

###产生matrix

l <- length(mat_pearson[1,])

number <- seq (2, 15,1)

new_mat <- c()

for (j in 1:l) {
  
  for (i in number){
    
    stage <- paste("Stage",i,sep="_")
    
    new_mat <- c(new_mat,mean(mat_pearson[,j][which(row.names(mat_pearson) == stage)]))
    
    
  }
  
}

new_mat2<-matrix(new_mat,ncol=25,byrow=T) 

colnames(new_mat2)<-colnames(mat_pearson)

####定义stage rowname
stage_name <- c()
for (i in number){
  
  stage <- paste("Stage",i,sep="_")
  
  stage_name <- c(stage_name,stage)
  
}

row.names(new_mat2)<-stage_name


pearson_heatmap<-pheatmap(t(new_mat2),cellwidth =30, cellheight =20,fontsize=12,border_color="black",scale="row",
                          fontsize_row=12, color=colorRampPalette(rev(c("red","white","blue")))(102),
                          cluster_rows = T,cluster_cols = F,display_numbers = F)


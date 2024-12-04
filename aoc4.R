input<-readLines("C:\\Users\\Nathaniel.Steinlicht\\Downloads\\aoc4.txt")
input<-c(paste0(rep(".",nchar(input[1])),collapse = ""),paste0(rep(".",nchar(input[1])),collapse = ""),paste0(rep(".",nchar(input[1])),collapse = ""),paste0(rep(".",nchar(input[1])),collapse = ""),
         input,paste0(rep(".",nchar(input[1])),collapse = ""),paste0(rep(".",nchar(input[1])),collapse = ""),paste0(rep(".",nchar(input[1])),collapse = ""),paste0(rep(".",nchar(input[1])),collapse = ""),paste0(rep(".",nchar(input[1])),collapse = ""))
input<-sapply(1:length(input), function(x) return(paste0(c(".....",input[x],"....."),collapse = "")))
#part 1
xmas_count<-NULL
for(i in 2:nchar(input[1])-1){
  for(j in 2:length(input)-1){
    if(substr(input[i],j,j)=="X"){
      top.left<-paste0(c("X",substr(input[i-1],j-1,j-1),substr(input[i-2],j-2,j-2),substr(input[i-3],j-3,j-3)),collapse = "")
      top.middle<-paste0(c("X",substr(input[i-1],j,j),substr(input[i-2],j,j),substr(input[i-3],j,j)),collapse = "")
      top.right<-paste0(c("X",substr(input[i-1],j+1,j+1),substr(input[i-2],j+2,j+2),substr(input[i-3],j+3,j+3)),collapse = "")
      middle.left<-paste0(c("X",substr(input[i],j-1, j-1),substr(input[i],j-2, j-2),substr(input[i],j-3, j-3)),collapse = "")
      middle.right<-paste0(c("X",substr(input[i],j+1,j+1),substr(input[i],j+2,j+2),substr(input[i],j+3,j+3)),collapse = "")
      bottom.left<-paste0(c("X",substr(input[i+1],j-1,j-1),substr(input[i+2],j-2,j-2),substr(input[i+3],j-3,j-3)),collapse = "")
      bottom.middle<-paste0(c("X",substr(input[i+1],j,j),substr(input[i+2],j,j),substr(input[i+3],j,j)),collapse = "")
      bottom.right<-paste0(c("X",substr(input[i+1],j+1,j+1),substr(input[i+2],j+2,j+2),substr(input[i+3],j+3,j+3)),collapse = "")
      xmas_count<-append(xmas_count,c(top.left,top.middle,top.right,middle.left,middle.right,bottom.left,bottom.middle,bottom.right))
    }
  }
}
length(xmas_count[which(xmas_count=="XMAS"|xmas_count=="SAMX")])
#part 2
mas_count<-0
xmas_matrix<-matrix(c("M",".","S", ".","A",".","M",".","S"), nrow = 3, ncol = 3, byrow = TRUE)
rotate <- function(x) t(apply(x, 2, rev))
for(i in 2:nchar(input[1])-1){
  for(j in 2:length(input)-1){
    if(substr(input[i],j,j)=="A"){
      top.left<-substr(input[i-1],j-1,j-1)
      top.right<-substr(input[i-1],j+1,j+1)
      bottom.left<-substr(input[i+1],j-1,j-1)
      bottom.right<-substr(input[i+1],j+1,j+1)
      origional<-matrix(c(top.left,".",top.right, ".","A",".",bottom.left,".",bottom.right), nrow = 3, ncol = 3, byrow = TRUE)
      org90<-rotate(origional)
      org180<-rotate(rotate(origional))
      org270<-rotate(rotate(rotate(origional)))
      if(identical(origional,xmas_matrix)){
        mas_count<-mas_count+1
      }else if(identical(org90,xmas_matrix)){
        mas_count<-mas_count+1
      }else if(identical(org180,xmas_matrix)){
        mas_count<-mas_count+1
      }else if(identical(org270,xmas_matrix)){
        mas_count<-mas_count+1
      }
    }
  }
}
print(mas_count)
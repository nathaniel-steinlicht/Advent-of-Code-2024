data<-read.table("C:\\Users\\Black\\Downloads\\aoc1.txt" )
##part 1
v1<-sort(data$V1)
v2<-sort(data$V2)
distance<-sum(abs(v1-v2))
##part 2
left_list<-unique(v1)
right_list_counts<-dplyr::count(data,V2)
combinded_list<-merge(data.frame("V1"=left_list),right_list_counts,by.x = "V1",by.y="V2")
combinded_list$similarity_score<-combinded_list$V1*combinded_list$n
sum(combinded_list$similarity_score)

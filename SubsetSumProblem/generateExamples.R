GenerateExamples <- function(size,a,b,filename){
  tempList <- sample(a:b,size)
  write.table(tempList,filename,row.names=FALSE,col.names=FALSE,eol=" ")
}

GenerateExamples(10,1,100,"test2.txt")
GenerateExamples(15,100,1000,"test3.txt")
GenerateExamples(20,10,100,"test4.txt")
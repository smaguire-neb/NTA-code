library(stringr)
library(dplyr)
library(R.utils)
library(ggplot2)
library(ggthemes)
file<-"merged_01cap_psII_cut.fasta"
#file<-"test.fasta"
reads<-file(file,"r")
nta.list<-list()
line.num <- countLines(file)[[1]]
pb <- txtProgressBar(min = 0, max = line.num)
count<-0
while ( TRUE ) {
    line = readLines(reads, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    if(str_sub(line,1,1) == ">"){
      count <- count + 1
      setTxtProgressBar(pb,count)
      next
    }
    if(str_length(line) < 4){
      next
    } else if(str_length(line) == 4){
      nta <- 'no_add'
      rn1 <- str_sub(line, start = -4,end = -4)
    } else if(str_length(line) > 4){
      nta <- str_sub(line, end = -5)
      rn1 <- str_sub(line, start = -4,end = -4)
    }
    if(is.null(nta.list[[rn1]])){
      nta.list[[rn1]] <- list()
      nta.list[[rn1]][[nta]] <- 1
    } else if(is.null(nta.list[[rn1]][[nta]])){
      nta.list[[rn1]][[nta]] <- 1
    } else{
      nta.list[[rn1]][[nta]] <- nta.list[[rn1]][[nta]] + 1
    }
    count <- count + 1
    setTxtProgressBar(pb,count)
  }
  
close(reads)


convert.data.frame<-function(sublist){
  temp1<-as.data.frame(t(as.data.frame(sublist)))
  names(temp1)[1]<-"count"
  temp1$nta <- rownames(temp1)
  return(as_tibble(temp1))
}

nta.df<-lapply(nta.list,convert.data.frame)

nta.g<-nta.df[["G"]]
head(nta.g)

plot_length_histogram<-function(subdf){
  no_add <- filter(subdf,nta == "no_add")$count
  subdf %>%
    filter(nta != "no_add") %>%
    mutate(nta.length = str_length(nta)) %>%
    group_by(nta.length) %>%
    summarise(count.sum = sum(count)) %>%
    rbind(c(0,no_add),.) %>%
    mutate(percent = (count.sum/sum(count.sum))*100) %>%
    ggplot(.,aes(x = nta.length, y = percent))+geom_histogram(stat = 'identity',binwidth = 1) + 
    xlim(c(-1,6))
}
mutate(nta.g,nta.length = str_length(nta))

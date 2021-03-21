install.packages("sjmisc")
library(sjmisc)

jobs <- c (1,2,3,4)
processing_time <-c(10,10,13,4)
due_dates <- c(4,2,1,12)
weighted <- c(14,12,1,12)
tabu_search <-t(data.frame(jobs,processing_time,due_dates,weighted))


tabuSearchFunc <- function(tabu_search,solutionF,iSay){
  alternatif_solution <- function(x,i,j) {x[c(i,j)] <- x[c(j,i)]; x}
  solution1 <- solutionF
  TabuListLength <-c()
  for(k in 1:iSay){
    summaryTabu <-c()
    solutionNN <- c()
    tabuList <- c()

    for (i in 1:length(solution1)){
      solutionN <- alternatif_solution(solution1,i-1,i)
      if(i >= 2){
        tabuList <- c(solution1[i-1],solution1[i])
      } else{
        tabuList <- c("-","-")
      }

      cumulative_total <- 0
      FS <- 0
      summaryTN<-0
      for(j in solutionN){
        cumulative_total <- tabu_search["processing_time",j] + cumulative_total
        tardiness <- (cumulative_total-tabu_search["due_dates",j])
        if(tardiness < 0){
          tardiness = 0
        }
        summaryTN <- (tabu_search["weighted",j]*tardiness)+ FS
        FS <-summaryTN
      }
      summaryTabu <- append(summaryTabu,i)
      summaryTabu <- append(summaryTabu,tabuList)
      summaryTabu <- append(summaryTabu,solutionN)
      summaryTabu <- append(summaryTabu,summaryTN)
    }
    iterasyon<-t(matrix(summaryTabu,nrow = 8))
    colnames(iterasyon) <- c("SatirNo","C1","C2","F1","F2","F3","F4","FSolutions")
    iterasyon <- as.data.frame(iterasyon)
    iterasyonD <- iterasyon
    if(length(TabuListLength)>=4){
      TabuListLength<-TabuListLength[-1,]
      iterasyonD <- iterasyonD[-1,]
      iterasyonOrder <- iterasyonD[order(iterasyonD$FSolutions),]
      iterasyonDO <- iterasyonOrder
      tabuList <- as.numeric(iterasyonDO[1,2:3])
      if(str_contains(TabuListLength, tabuList, logic = "and")){
        iterasyonDO <- iterasyonDO[-1,]
        iterasyonOrderr <- iterasyonDO[order(iterasyonDO$FSolutions),]
        FBestSolution<-iterasyonOrderr[1,"FSolutions"]
        TabuListOrder <- iterasyonOrderr[1,2:3]
        SolutionOrder <- iterasyonOrderr[1,4:7]
        SolutionOrder<-as.numeric(SolutionOrder)
        solution1 <- SolutionOrder
        TabuListLength <- t(matrix(append(TabuListLength,TabuListOrder),nrow =2))
      } else{
        iterasyonOrderr <- iterasyonDO[order(iterasyonDO$FSolutions),]
        FBestSolution<-iterasyonOrderr[1,"FSolutions"]
        TabuListOrder <- iterasyonOrderr[1,2:3]
        SolutionOrder <- iterasyonOrderr[1,4:7]
        SolutionOrder<-as.numeric(SolutionOrder)
        solution1 <- SolutionOrder
        TabuListLength <- t(matrix(append(TabuListLength,TabuListOrder),nrow =2))
      }
    } else{
      iterasyonOrder <- iterasyonD[order(iterasyonD$FSolutions),]
      tabuList <- iterasyonOrder[1,2:3]
      if(str_contains(TabuListLength, tabuList, logic = "and")){
        iterasyonD <- iterasyonD[-1,]
        tabu <- iterasyonD[(iterasyonD[["C1"]][tabuList]),]
        iterasyonD <- iterasyonD[-tabu$SatirNo,]
        iterasyonOrder <- iterasyonD[order(iterasyonD$FSolutions),]
        FBestSolution<-iterasyonOrder[1,"FSolutions"]
        TabuListOrder <- iterasyonOrder[1,2:3]
        SolutionOrder <- iterasyonOrder[1,4:7]
        SolutionOrder<-as.numeric(SolutionOrder)
        solution1 <- SolutionOrder
        TabuListLength <- t(matrix(append(TabuListLength,TabuListOrder),nrow =2))
      } else{
        iterasyonD <- iterasyonD[-1,]
        iterasyonOrder <- iterasyonD[order(iterasyonD$FSolutions),]
        FBestSolution<-iterasyonOrder[1,"FSolutions"]
        TabuListOrder <- iterasyonOrder[1,2:3]
        SolutionOrder <- iterasyonOrder[1,4:7]
        SolutionOrder<-as.numeric(SolutionOrder)
        solution1 <- SolutionOrder
        TabuListLength <- t(matrix(append(TabuListLength,TabuListOrder),nrow =2))
      }
    }

    print("##########################################################")
    print(paste(k,".iterasyon"))
    print(iterasyon)
    print(TabuListLength)
    print(paste("En iyi Total Weight Tardiness: ",FBestSolution))
    print("##########################################################")
    print("                                                          ")

  }
}

solutionF <- c(2,1,4,3)

tabuSearchFunc(tabu_search,solutionF,4)




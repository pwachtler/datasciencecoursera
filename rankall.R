rankall <- function(outcome, num = "best") {

  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  

  ##Adding simpleCap function for letter capitaliztion
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  
  
  ## Read outcome data
  file<-read.csv("outcome-of-care-measures.csv")
  
  
  
  ## Checking invalid outcome
  outcome.list<-c("heart attack","heart failure","pneumonia")
  if (!outcome %in% outcome.list)
  {result<- "invalid outcome"
  return (result)}

  
  hosp.name<-file[,"Hospital.Name"]    ## set vector for hospitals
  state.name<-file[,"State"]  ## set vector for states
  outcome.values.init<-file[,paste("Hospital.30.Day.Death..Mortality..Rates.from", sub(" ",".",simpleCap(outcome)),sep=".")]  ## set vector for outcomeswrite.csv(result.table,"test.csv")
  outcome.values<- as.numeric(as.character(outcome.values.init))
   ##return(seq_along(unique(state.name)))
 
   ##combine columns together
  new.table<-cbind.data.frame(state.name,hosp.name,outcome.values)


  ##split table by state
  ##result.table=split(new.table,new.table$state.name)
##  write.csv(result.table,"test2.csv")
  ##return(head(result.table,n=5))
  
  
  ##options(warn=oldw)## turn back on warnings
  
  
  ##sort by outcome value and then by hospital name
  sort.table = with(new.table,order(outcome.values, hosp.name))
  final.table.init<-new.table[sort.table,]
  
  ##remove na values from the table
  final.table<-na.omit(final.table.init)
  
  
  ##split
  result.table<-split(final.table,final.table$state.name)
  ##return(as.vector(result.table$CA[1,2]))
  
  ##Converting num to integer
  rnum<-0
  
  
  ##initalize loop variables
  unistatename<-unique(state.name)
  rnum<-1
  answermatrix<-matrix(0,54,2)
  colnames(answermatrix)<-c("hospital","state")
 ## return(state.name[1])

  
 ## loop.state<-"NY"
 ## rnum<-as.numeric(as.character(num))
 ## answermatrix[1,1]<-as.character(get(loop.state,result.table)[rnum,2])
 ## return(answermatrix)
  ##return(get(loop.state,result.table)[rnum,2])
  ##return(result.table$NY[1,2])

  ##loops to create answer matrix  
  if(num=="best"){

    for (i in seq_along(unistatename)){
        loop.state<-unistatename[i]
        answermatrix[i,1]<-as.character(get(loop.state,result.table)[1,2])
        answermatrix[i,2]<-as.character(loop.state)
    }
    return (answermatrix)
  }
  
    else if(num=="worst"){
      
        for (i in seq_along(unistatename)){
          loop.state<-unistatename[i]
          answermatrix[i,1]<-as.character(get(loop.state,result.table)[nrow(hosp.name),2])
          answermatrix[i,2]<-as.character(loop.state)
          }
      return (answermatrix)
    }
      
    else {
      
        rnum<-as.numeric(as.character(num))

        for (i in seq_along(unistatename)){
          loop.state<-unistatename[i]
          
        ## return(as.character(get(as.vector(loop.state),result.table)[rnum,2]))
          
          answermatrix[i,1]<-as.character(get(as.vector(loop.state),result.table)[rnum,2])
          answermatrix[i,2]<-as.character(loop.state)
        }
        ##answermatrix<- answermatrix
        return (answermatrix[sort.list(answermatrix[,2]),])
    
    
    }
 
  
  ##return requested value
 ## as.vector(final.table[rnum,2])
  
}
rankhospital <- function(state, outcome, num="best") {
  
  
  ##Adding simpleCap function for letter capitaliztion
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
  }
  

  ## Read outcome data
  file<-read.csv("outcome-of-care-measures.csv")
  
  
  
  ## Check that state and outcome are valid
  
  ## Checking invalid state
  state.list<-file[,"State"]
  if (!state %in% state.list)
    {result<-"invalid state"
  return (result)}
  
  ## Checking invalid outcome
  outcome.list<-c("heart attack","heart failure","pneumonia")
  if (!outcome %in% outcome.list)
    {result<- "invalid outcome"
  return (result)}
  

  hosp.name<-file[,"Hospital.Name"]    ## set vector for hospitals
  outcome.values.init<-file[,paste("Hospital.30.Day.Death..Mortality..Rates.from", sub(" ",".",simpleCap(outcome)),sep=".")]  ## set vector for outcomeswrite.csv(result.table,"test.csv")
  outcome.values<- as.numeric(as.character(outcome.values.init))
  
  ##combine columns together
  new.table<-cbind.data.frame(state.list,hosp.name,outcome.values)
  
  
  ##filter on state match
  result.table=new.table[state.list==state,]
  
  
  ##options(warn=oldw)## turn back on warnings
  
  
  ##sort by outcome value and then by hospital name
  sort.table = with(result.table,order(outcome.values, hosp.name))
  final.table.init<-result.table[sort.table,]
  
  ##remove na values from the table
  final.table<-na.omit(final.table.init)
  
  ##Converting num to integer
  rnum<-0
  if(num=="best"){
    rnum<-1
  }
  else if(num=="worst")
    {rnum<-nrow(final.table)}
  else {rnum<-as.numeric(as.character(num))}
  
  
  ##return requested value
  as.vector(final.table[rnum,2])
  
}

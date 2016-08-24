
### SETTING THE WORK DIRECTORY
wkdir = "~\\"
subdir = "Github\\R programming"

# create directory if it doesn't exist yet
dir.create(file.path(wkdir, subdir))
setwd(file.path(wkdir, subdir))

#zip file download to director
url = "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-data.zip"
download.file(url, destfile = paste0(wkdir,"\\hospdata.zip"))


#######
# FUNCTIONS
#######

best <- function(state, outcome) {
  ## Read outcome data
  dataset <- read.csv(unz("hospdata.zip","outcome-of-care-measures.csv"), stringsAsFactors = FALSE)
  dataset = dataset[,c(2,7,11,17,23)]
  colnames(dataset) = c("Name","State","heart attack","heart failure","pneumonia")
  list_states = unique(dataset[,"State"])
  correct_outcomes = c("heart attack","heart failure","pneumonia")
  if(state %in% list_states){
    if(outcome %in% correct_outcomes){
      dataset[,"heart attack"] = as.numeric(dataset[,"heart attack"])
      dataset[,"heart failure"] = as.numeric(dataset[,"heart failure"])
      dataset[,"pneumonia"] = as.numeric(dataset[,"pneumonia"])
      ## Check that state and outcome are valid
      data = dataset[dataset$State == state,c("Name","State",outcome)]
      data = data[complete.cases(data),]
      ## Return hospital name in that state with lowest 30-day death
      data = data[order(data[,"Name"]),]
      output = data[data[,outcome] == min(data[,3]),]
      return(output)
    }else{
      cat(paste0('Error in best("',state,'","',outcome,'"): invalid outcome'))
      
    }
  }else{
    cat(paste0('Error in best("',state,'","',outcome,'"): invalid state'))
    
  }
}



rankhospital <- function(state,outcome,rank){
  dataset <- read.csv(unz("hospdata.zip","outcome-of-care-measures.csv"), stringsAsFactors = FALSE)
  dataset = dataset[,c(2,7,11,17,23)]
  colnames(dataset) = c("Name","State","heart attack","heart failure","pneumonia")
  list_states = unique(dataset[,"State"])
  correct_outcomes = c("heart attack","heart failure","pneumonia")
  if(state %in% list_states){
    if(outcome %in% correct_outcomes){
      dataset[,"heart attack"] = as.numeric(dataset[,"heart attack"])
      dataset[,"heart failure"] = as.numeric(dataset[,"heart failure"])
      dataset[,"pneumonia"] = as.numeric(dataset[,"pneumonia"])
      # ## Check that state and outcome are valid
      data = dataset[dataset$State == state,c("Name","State",outcome)]
      data = data[complete.cases(data),]
      if(rank == "best"){
        rank = 1
      }else if(rank == "worst"){
        rank = length(data[,1])
      }else if(rank > length(data[,1])){
        return(NA)
      }else{
        rank = rank
      }
      data = data[order(data[,"Name"]),]
      ordered_data = data[order(data[,outcome]),]
      ## Return hospital name in that state with lowest 30-day death
      output = ordered_data[rank,]
      return(output)
    }else{
      cat(paste0('Error in best("',state,'","',outcome,'"): invalid outcome'))
    }
  }else{
    cat(paste0('Error in best("',state,'","',outcome,'"): invalid state'))
  }
}



rankall <- function(outcome,ranking){
  dataset <- read.csv(unz("hospdata.zip","outcome-of-care-measures.csv"), stringsAsFactors = FALSE)
  dataset = dataset[,c(2,7,11,17,23)]
  colnames(dataset) = c("Name","State","heart attack","heart failure","pneumonia")
  list_states = unique(dataset[,"State"])
  correct_outcomes = c("heart attack","heart failure","pneumonia")
  if(outcome %in% correct_outcomes){
    dataset[,"heart attack"] = as.numeric(dataset[,"heart attack"])
    dataset[,"heart failure"] = as.numeric(dataset[,"heart failure"])
    dataset[,"pneumonia"] = as.numeric(dataset[,"pneumonia"])
    # ## Check that state and outcome are valid
    j = 1
    output = list()
    for(i in list_states){
      data = dataset[dataset$State == i,c("Name","State",outcome)]
      data = data[complete.cases(data),]
      if(ranking == "best"){
        rank = 1
      }else if(ranking == "worst"){
        rank = length(data[,1])
      }else if(ranking > length(data[,1])){
        rank = NA
      }else{
        rank = ranking
      }
      if(is.na(rank)){
        output[[j]] = c(NA,i)
      }else{
      data = data[order(data[,"Name"]),]
      ordered_data = data[order(data[,outcome]),]
      
      output[[j]] = c(ordered_data[rank,1],ordered_data[rank,2])
      }
      j = j+1
    }
    foutput = as.data.frame(do.call(rbind,output))
    colnames(foutput) = c("hospital","state")
    foutput = foutput[order(foutput[,"state"]),]
    return(foutput)
  }else{
    cat(paste0('Error in best("',state,'","',outcome,'"): invalid outcome'))
  }
  
}





best("SC","heart attack")
best("NY","pneumonia")
best("AK","pneumonia")
rankhospital("NC","heart attack","worst")
rankhospital("WA","heart attack",7)
rankhospital("TX","pneumonia",10)
rankhospital("NY","heart attack",7)


r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)


r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)

#written by Albert Salvador

make_LD <- function(x){
  structure(x,class="LongitudinalData")
}

print.LongitudinalData <- function(x){
  paste("Longitudinal dataset with",length(unique(x$id)),"subjects")
}

subject <- function(x, id) UseMethod("subject")

subject.LongitudinalData <- function(x,id){
  if (any(x$id==id)){
    which_id<-which(x$id==id)
    structure(list(id=x[["id"]][which_id],
                   visit=x[["visit"]][which_id],
                   room=x[["room"]][which_id],
                   value=x[["value"]][which_id],
                   timepoint=x[["timepoint"]][which_id]),
              class="Subject")
  } else {
    return(NULL)
  }
}

print.Subject <- function(variable){
  paste("Subject ID:",unique(variable[["id"]]))
}

summary.Subject <- function(variable){
  
  output <- data[which(data$id==variable$id[1]),2:5] %>%
    group_by(visit,room) %>%
    summarise(value=mean(value)) %>%
    spread(room,value) %>%
    as.data.frame
  structure(list(id=variable[["id"]],
                 output=output),
            class="Summary")
  return(output)
}

visit <- function(subject, visit) UseMethod("visit")

visit.Subject <- function(subject, visit) {
  data <- data[which(data$id==subject$id[1]),2:5] %>% 
    filter(visit == visit) %>% 
    select(-visit)
  structure(list(id = subject[["id"]],
                 visit = visit,
                 data = data), class = "Visit")
}

room <- function(visit, room) UseMethod("room")

room.Visit <- function(visit, room) {
  data <- data[which(data$id==visit$id[1] &
                       data$visit==visit$visit[1]),2:5] %>% 
    filter(room == room) %>% 
    select(-room)
  structure(list(id = visit[["id"]],
                 visit = visit[["visit"]],
                 room = room,
                 data = data), class = "Room")
}

print.Room <- function(variable) {
  cat("ID:", unique(variable[["id"]]), "\n")
  cat("Visit:", unique(variable[["visit"]]), "\n")
  cat("Room:", unique(variable[["room"]]))
}

summary.Room <- function(object) {
  output <- summary(object[["data"]][["value"]])
  structure(list(id = object[["id"]],
                 output = output), class = "Summary")
  return(output)
}



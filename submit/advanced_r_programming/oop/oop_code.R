library(data.table)

make_LD <- function(data_f, ...) {
  if(!"data.frame" %in% class(data_f))
    stop("argument should be a data frame\n Problem", names(data_f), 
         "is not a data.frame")
  object <- data_f
  class(object) <- c("LongitudinalData", "data.frame")
  object
}

print.LongitudinalData <- function(x, ...){
  cat("Longitudinal dataset with", length(unique(x$id)), "subjects")
  return(invisible(x))
}

subject <- function(x, sub_id){
  if(!"LongitudinalData" %in% class(x)){
    stop("argument x should be an object of class LongitudinalData")
  }
  ans <- list(ld = x[x$id == sub_id, ], sid = sub_id)
  class(ans) <- "Subject"
  ans
}

print.Subject <- function(x, ...){
  data_returned <- x$ld
  if(dim(data_returned)[[1]] == 0){
    cat("NULL")
  }else{
    cat("Subject ID: ", x$sid)
  }
  return(invisible(x))
}

summary.Subject <- function(x, ...){
  data2 <- data.table::as.data.table(x$ld)
  ans <- as.data.frame(data2[, .(value=mean(value)), by=.(visit, room)])
  object <- list(ans = ans, sid = x$sid)
  class(object) <- "summary_Subject"
  return(object)
}

print.summary_Subject <- function(x, ...){
  cat("ID: ", x$sid, "\n")
  print(x$ans)
}

visit <- function(x, visit_num){
  if(!"Subject" %in% class(x)){
    stop("argument x should be an object of class Subject")
  }
  ans <- list(ld = x$ld[x$ld$visit == visit_num, ], sid = x$sid,
              vid = visit_num)
  class(ans) <- c("Visit")
  ans
}

print.Visit <- function(x, ...){
  data_returned <- x$ld
  if(dim(data_returned)[[1]] == 0){
    cat("NULL")
  }else{
    cat("Visit: ", x$vid)
  }
  return(invisible(x))
}

summary.Visit <- function(x, ...){
  data2 <- data.table::as.data.table(x$ld)
  ans <- as.data.frame(data2[, .(value=mean(value)), by=.(room)])
  object <- list(ans = ans)
  class(object) <- "summary_Visit"
  return(object)
}

print.summary_Visit <- function(x, ...){
  cat("Visit: ", x$vid)
  print(x$ans)
}

room <- function(x, room){
  if(!"Visit" %in% class(x)){
    stop("argument x should be an object of class Visit")
  }
  ans <- list(ld = x$ld[x$ld$room == room, ], sid = x$sid,
              vid = x$vid, rid = room)
  class(ans) <- c("Room")
  ans
}

print.Room <- function(x, ...){
  data_returned <- x$ld
  if(dim(data_returned)[[1]] == 0){
    cat("NULL")
  }else{
    cat("ID: ", x$sid, "\n")
    cat("Visit: ", x$vid, "\n")
    cat("Room: ", x$rid)
  }
  return(invisible(x))
}

summary.Room <- function(x, ...){
  object <- list(ans = x$ld, sid = x$sid)
  class(object) <- "summary_Room"
  return(object)
}

print.summary_Room <- function(x, ...){
  cat("ID: ", x$sid, "\n")
  print(summary(x$ans$value))
}

# run_play.R

drive_state <- list(D=1, YTG=10, FP=25, exit_drive=0)

#managing the downs
run_play <- function() {
  if (drive_state$D == 1) {
    down_one()
  } else if (drive_state$D == 2) {
    down_two()
  } else if (drive_state$D == 3) {
    down_three()
  } else {
    down_four()
  }
}

#function for first down
down_one <- function() {
  YG <- sample_yards_gained()
  drive_state$YTG <- max(drive_state$YTG - YG, 0)
  drive_state$FP <- drive_state$FP + YG
  
  if (drive_state$YTG == 0) {
    drive_state$D <- 1
    drive_state$YTG <- 10
  } else {
    drive_state$D <- 2
  }
}

#function for second down
down_two <- function() {
  YG <- sample_yards_gained()
  drive_state$YTG <- max(drive_state$YTG - YG, 0)
  drive_state$FP <- drive_state$FP + YG
  
  if (drive_state$YTG == 0) {
    drive_state$D <- 1
    drive_state$YTG <- 10
  } else {
    drive_state$D <- 3
  }
}

#function for third down
down_three <- function() {
  YG <- sample_yards_gained()
  drive_state$YTG <- max(drive_state$YTG - YG, 0)
  drive_state$FP <- drive_state$FP + YG
  
  if (drive_state$YTG == 0) {
    drive_state$D <- 1
    drive_state$YTG <- 10
  } else {
    drive_state$D <- 4
  }
}

#function for fourth down decisions
down_four <- function() {
  decision <- fourth_down_decision()
  
  if (decision == "go for it") {
    YG <- sample_yards_gained()
    drive_state$YTG <- max(drive_state$YTG - YG, 0)
    drive_state$FP <- drive_state$FP + YG
    
    if (drive_state$YTG == 0) {
      drive_state$D <- 1
      drive_state$YTG <- 10
    } else {
      drive_state$exit_drive <- 1 #turnover
    }
  } else if (decision == "punt") {
    drive_state$FP <- drive_state$FP - sample(30:50, 1)
    drive_state$exit_drive <- 1
  } else {
    fg_success <- sample(c(TRUE, FALSE), 1, prob=c(0.75, 0.25))
    if (fg_success) {
      drive_state$FP <- 115 #made a field goal
    }
    drive_state$exit_drive <- 1
  }
}

#function to decide what to do on 4th down
fourth_down_decision <- function() {
  if (drive_state$FP > 60) {
    if (drive_state$YTG < 2) {
      return(sample(c("go for it", "field goal"), 1, prob=c(0.7, 0.3)))
    } else {
      return("field goal")
    }
  } else if (drive_state$FP > 30) {
    return(sample(c("punt", "go for it"), 1, prob=c(0.8, 0.2)))
  } else {
    return("punt")
  }
}

#function to sample yards gained based on down and situation
sample_yards_gained <- function() {
  return(sample(-5:20, 1, prob=c(rep(0.05, 5), rep(0.9/20, 20))))
}


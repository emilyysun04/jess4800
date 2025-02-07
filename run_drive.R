run_drive <- function(down, ytg, fp) {
  
  new_fp <- sample(c(80, 105, 115), 1, prob=c(.9, .05, .05))
  
  list(down=1, ytg=10, fp=new_fp)
}


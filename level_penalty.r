level_penalty <- function(player, monster) {
  diff = floor(3 + (player/16))
  lower = player - diff
  upper = player + diff 
  penalty = 0
  
  if(monster >= lower && monster <= upper) { # No experience penalty
    penalty = 0
  } else if(monster < lower) {
    penalty = lower - monster
  } else { # if(monster > upper) {
    penalty = monster - upper
  }
  
  final = ((player + 5) / (player + 5 + penalty^2.5))^1.5
  #final = as.numeric(format(round(final, 2), nsmall = 2))
  if(is.nan(final)) {
    print(sprintf("%d:%d", player, monster))
  }
  final
}

df_penalties <- function(levels, monster) {
  future_matrix = c()
  
  size_lev = length(levels)
  size_mon = length(monster)
  
  for(level in levels) {
    for(mon in monster) {
      future_matrix = c(future_matrix, as.numeric(level_penalty(level, mon)))
    }
  }
  matrix(future_matrix, size_lev, size_mon, byrow = TRUE)
}


create_graph <- function() {
  if(!require("reshape2") || !require("ggplot2")) {
    library(reshape2)
    library(ggplot2)
  }
  levels <- c(seq(1,100))
  monsters <- c(seq(1,78))
  
  poe.matrix <- df_penalties(levels, monsters)
  
  melted <- melt(poe.matrix)
  colnames(melted) <- c("Player", "Monster", "Penalty")
    
  ggplot(data=melted, aes(x=Player, y=Monster, fill=Penalty)) + geom_tile() + scale_fill_gradient(low="green", high="red")
}
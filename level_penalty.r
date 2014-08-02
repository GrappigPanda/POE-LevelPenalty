level_penalty <- function(player, map) {
  diff = 3 + floor(player/16)
  lower = player - diff
  upper = player + diff 
  penalty = 0
  
  if(map > lower && map < upper) { # No experience penalty
    penalty = 0
  } else if(map < lower) {
    penalty = lower - map
  } else { # if(map > upper) {
    penalty = map - upper
  }
  
  final = ((player + 5) / (player + 5 + penalty^2.5))^1.5
  final = format(round(final, 2), nsmall = 2)
  final
}

df_penalties <- function(levels, map) {
  future_matrix = c()
  
  size_lev = length(levels)
  size_map = length(maps)
  
  for(level in levels) {
    for(map in maps) {
      future_matrix = c(future_matrix, as.numeric(level_penalty(level, map)))
    }
  }
  matrix(future_matrix, size_lev, size_map, byrow = TRUE)
}


create_graph <- function() {    
  levels <- c(seq(1,100))
  monsters <- c(seq(1,78))
  
  poe.matrix <- df_penalties(levels, monsters)
  
  melted <- melt(poe.matrix)
  colnames(melted) <- c("Player", "Monster", "Penalty")
  melted[is.na(melted)] <- 1.00 # I desperately need to figure out why this goes to NaN  
    
  ggplot(data=melted, aes(x=Player, y=Monster, fill=Penalty)) + geom_tile() + scale_fill_gradient(low="green", high="red")
}
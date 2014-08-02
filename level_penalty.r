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

old_df_penalties <- function(levels, map) {
  future_matrix = c()
  
  size = length(poe.df$levels)
  
  for(level in poe.df$levels) {
    for(map in poe.df$maps) {
      future_matrix = c(future_matrix, as.numeric(level_penalty(level, map)))
    }
  }
  matrix(future_matrix, size, size, byrow = TRUE)
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
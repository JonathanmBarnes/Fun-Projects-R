library(ggplot2)
library(gganimate)
Size <- 35
Lives <- 100
Conway <- function(Size, Lives, prob = c(.9, .1)){
GameSize <- Size^2
GameBoard <- matrix(sample(c(0,1), GameSize, replace = T, prob), nrow = Size, ncol = Size)
#print(GameBoard)
WholeGame <- reshape2::melt(GameBoard)
WholeGame$Gen <- 0
  for(i in 1:Lives){
    GameBoard <- SimLife(GameBoard, Size)
    WholeGameHold <- reshape2::melt(GameBoard)
    WholeGameHold$Gen <- i
    WholeGame <- rbind(WholeGame, WholeGameHold)
    EndGame <- 1 %in% GameBoard
    if (EndGame != T)
    {
      WholeGameHold$Gen <- i + 1
      WholeGame <- rbind(WholeGame, WholeGameHold)
      break}
  }

Game <- ggplot(WholeGame, aes(x = Var1, y = Var2, fill = value)) +
  geom_raster() +
  transition_time(Gen) + theme_void() + theme(legend.position = "none")
  print(animate(Game, nframes = max(WholeGame$Gen), duration = max(WholeGame$Gen)/7))
}

SimLife <- function(GameBoard, Size){
  GameScore <- matrix(0,nrow = Size, ncol = Size)
  #print(GameScore)
  for(i in 1:Size){
    Up <- i -1
    Down <- i + 1
    if (i == 1){
      Up <- Size
      Down <- i + 1
    }
    else if(i == Size){
      Up <- i -1
      Down <- 1
    }
    for(j in 1:Size){
      Left <- j - 1
      Right <- j + 1
      if (j == 1){
        Left <- Size
        Right <- j + 1
      }
      else if(j == Size){
        Left <- j - 1
        Right <- 1
      }
      if(GameBoard[j,i] == 1){
        #print(c("Start" ,j,i,Left, Right,Up,Down))
        GameScore[Left,i] <- GameScore[Left,i] + 1
        GameScore[j,Up] <- GameScore[j,Up] + 1
        GameScore[Right,i] <- GameScore[Right,i] + 1
        GameScore[j,Down] <- GameScore[j,Down] + 1
        GameScore[Left,Down] <- GameScore[Left,Down] + 1
        GameScore[Right,Up] <- GameScore[Right,Up] + 1
        GameScore[Left,Up] <- GameScore[Left,Up] + 1
        GameScore[Right,Down] <- GameScore[Right,Down] + 1
      }
    }
  }
  #Test <- 2 %in% GameScore | 3 %in% GameScore
  #print(GameScore)
  Sc1 <- which(GameScore < 2)
  Sc3 <- which(GameScore == 3)
  Sc4 <- which(GameScore > 3)
  GameBoard[Sc3] <- 1
  GameBoard[Sc1] <- 0
  GameBoard[Sc4] <- 0
  return(GameBoard)
}

Conway(Size, Lives)

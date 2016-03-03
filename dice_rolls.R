#' Rolling Dice
#'
#' \code{dice.rolls} returns a vector of results from throwing \code{number.dice} of
#' \code{sides} sided dice \code{number.rolls} times. Each item in the vector is the 
#' sum of dice for a given throw. 
#' 
#' @param number.dice Positive Integer. How many dice to roll on each roll. Default = 1
#' @param number.rolls Positive Integer. How many rolls to do. Default = 1
#' @param sides. Positive Integer. How many sides the dice have. 
#' 
#' All the dice will have the same
#' number of sides. The allowable dice are 1   2   3   4   5   6   7   8   9  10  11  12  13  14  
#' 15  16  17  18  19  20  22  24  30  32 34  48  50  60 100 120 144. These were taken from
#' \href{https://commons.wikimedia.org/wiki/Dice_by_number_of_sides}{Wikimedia, Dice by number
#' of sides.}
#'  
#' 
#' 
#
dice.rolls <- function(number.dice = 1, number.rolls = 1, sides=6)
{
  output <- NULL
  if (!is.element(sides, allowable.sides))
  {
    return(paste("Error: ", toString(sides), " is not an allowable number of sides."))
  }
  
  return(replicate(number.rolls, sum(sample(1:sides, number.dice, replace = TRUE))))
                                                       
}


allowable.sides <- c(1:20, 22 , 24, 30, 32, 34, 48, 50, 60, 100, 120, 144)
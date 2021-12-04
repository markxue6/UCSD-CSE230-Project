# UCSD-CSE230-Project
team members: Feng Xue, Shuchi Zhang, Ziyang Fan, Maoze Liu

## Proposal
In this project, a simple chess game connect 4 will be implemented. The connect four game is quite similar to Gobang. It requires two players. There is a n*n board as:

> x x x x x x x<br>
  x x x x x x x<br>
  x x x x x x x<br>
  x x x x x x x<br>
  x x x x x x x<br>
  0 1 2 3 4 5 6<br> 
  (n = 7 in this example. 'x' indicates empty cell, 'a' represents user a and 'b' represents user b)

Two user will choose a specific column to put their chess in alternation. The chess will be altermatically put on the downmost empty cell of the chosen column. For example, we have a current board look like:

> x x x x x x x<br>
  x x x x x x x<br>
  x x x x x x x<br>
  x x x x b x x<br>
  x a a a b x x<br>
  0 1 2 3 4 5 6

And user b choose the 0 as the next step. The board will be updated as:

> x x x x x x x<br>
  x x x x x x x<br>
  x x x x x x x<br>
  x x x x b x x<br>
  b a a a b x x<br>
  0 1 2 3 4 5 6

When there is a four consecutive chess in the row or column or diagonal from the same user, the user wins the game. Below is a board showing user winning the game:

> x x x x x x x<br>
  x x x b a x x<br>
  x x x a b x x<br>
  x x a a b x x<br>
  b a a a b b x<br>
  0 1 2 3 4 5 6

In this project, we will use haskell brick library to build a nice looking user interface for the game. Apart from the basic game mode, we have extensions on building more game modes.

* We have single player mode to play against computer. The game algorithm is minimax.
* In single player mode, different difficulties can be set by user.
* The size of the board can be set by user.
* The number of consecuitive chess to win can be set by user.
* If the chesses are full of the board and there are no four consecutive chesses, then there is a tie game result.

If it is possible, we will extend our game to a networked two-player game. Two players can play the game on different computers. In all, our project will focus on a chess game. When implementing the game, if we find the above rules are too simple for four people, we will try to make it more complex.


## MIlestone 2: Updates
We have finish the main part of the project. Some of the group members have done researches on how the game logic can be implemented. Shuchi has finished the coding part of a prototype. Current game has a simple UI using show method and print on the command line. The gaming algorithm is minimax which calculates 4 more steps and AI will choose the best step to be the next move.

We are planning to move the game to brick library in the next few weeks. There are a lot of challenges when we implemneted this project. For example, brick library and network library in Haskell is totally new for us. Feng Xue has natework programming experience in C++, but it still hard for him to figure it out in Haskell. Therefore, we are not going to add netwroking function. It will just the local chess game. From milestone 1 to milestone 2, we have zoom meeting every week. For the next two weeks, we will add brick libary to our project and give some test cases.

## First Version

The first version contains a two player version game. To Play the game, clone the repository and enter the following instructions sequecially:

>cd Connect4

>stack build

>stack exec Connect4

Then the main page will pop out in the terminal. Press 2 on the keyboard and press enter, the game should be able to play.
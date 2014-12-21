go-game-ai
Nyles Breecher
December 20, 2014
For AI 710 - Term Project
========== Overview ==========

Introduction
Files
How to use
Go game rules
Project goals
Project results / reflection

========== Introduction ==========

This programming project was initiated as my term project for AI 710.  The resulting program implements the rules for Go, including Ko and captures, but does not include a way to score a resulting board; that must be done by hand!  The AI was implemented using a minimax algorithm.

========== Files ==========

go-game-ai/
    minimax.lisp
        Contains the functions necessary to setup and play a 2-person game of go.  There are no components of the AI parts of the application in this file.
    README.md
        This file.  Outlines the project.
    minimax.lisp
        The implementation for the minimax algorithm.

========== How to use ==========

If you would like to play a game of Go, load up a lisp shell and enter the command (load "main.lisp"), then to start the game use the command (play-game).  Moves must be entered in the form "E7" where the letter indicates the row and the number indicates the column.

Settings need to be modified directly in the code itself.  One can change between a having the second player be human or an AI by commenting or uncommenting those sections under the function "play-game" in the file main.lisp.  One can adjust the board size by modifying the variable "board-size" in the "play-game" function.

========== Go game rules ==========

The following will be a very brief description of the rules of Go.  

Go is a game is game for territory.  The standard game is a 19x19 size board, but many smaller versions exist primarily for learning (9x9 and 13x13).  Black always starts.  When it's any player's move they are allowed to play their marker, called stones, on any non-occupied space on the board, with a couple exceptions described later.

Stones have things called "liberties," which are the amount of free spaces around that stone on the board.  If all of a stone's liberties are occupied by the enemy, then that piece is captured (removed from the board), and gets added to the opponent's score.  Stones of the same color which touch each other share liberties.  These are called "chains."  The whole chain is removed if all its liberties are removed.

Normally a player is not allowed to play into a location where they would be immediately captured, but if in doing so this player captures the opponent instead this is allowed.  However, in my particular implementation "suicide" moves are allowed, but hopefully the AI doesn't pick them because they're not good moves!

The winner (by Japanese rules) at the end is deteremed by who has the most territory + prisoners.  Territory is counted for a color if there is empty space (or lots of empty space) completely surrounded by one color.  In practice there might be opponent's pieces inside territory, but both players know these stones are "dead" and will count as prisoners without formally having to be captured.

Play stops when both players see no profitable moves and both pass in succession.

========== Project goals ==========

The goals of this project were very open.  Given that I know how difficult AI search strategies become for Go, I was entirely unsure what I could accomplish.  At the very least implementing a playable version of Go where a potential AI could access the information it needed to was an important goal from the start.  It didn't help that at the beginning of the project I had never actually played a single game of Go before.

So my first goal was to learn how to actually play Go.  Developing an AI would be pretty tough without a base understanding of the game.  I also wanted to do some research into AI techniques for Go and to try to see how I could apply the things which we worked on in class to a practical problem.

========== Project results / reflection ==========

The end result is a program which can play a game of Go using an AI equipped with a move maker using the minimax algorithm but with a very basic static board evaluator.  I believe that I have learned a significant amount during the project, but in many respects the final result doesn't really reflect much of that.  So in this section I would like to describe some of my challenges and things that I learned, all of which cannot really be obtained from just examining the code.

The first is that programming the rules of Go took a lot longer than expected.  The rules seem rather simple, but implementing things took a lot longer than I expected.  And I couldn't really save much time here, because in order for the AI to choose from proper moves and know resulting board states all of these things were required.  I considered using an outside resource to help with these types of issues (for example, there is a nice program called GoGUI which provides a GUI for GO), but at the end of the day for my AI to have internal knowledge of board states I needed to hardcode all of the rules myself.

Building up the program was quite useful in terms of thinking about how I might eventually implement my AI.  For example, being able to determine how many liberties chains have seems like it could be a potential feature for a board evaluator to look at.  I also had a practical problem to think about: how my eventual AI was going to take advantage of the other features I had coded into the game.

Once things finally game together, these features started to feel very slow.  Along the way I didn't spend too much time optimizing my program, but I was thinking about ways in which my implementation could be reduce the amount of computing time needed to expand nodes.  For example, while I was just testing the game itself my "ai" was just picking a random move.  This lead me to create a list of available moves, which I would choose randomly from and then update as the game went along.

Later, when I was implementing my minimax algorithm I included this available moves list in each of my nodes of the minimax tree.  While I wasn't able to test this, it made me wonder how much space these lists were using up.  Certainly it would be easy to write a function to calculate all available moves, but then that requires more computation time.  This was a very potent example for me in terms of recognizing some tradeoffs one might need to make between space and time constraints.

Originally, I had intended to implement a different algorithm instead of minimax.  When doing research about computer Go, the idea of Monte Carlo search came up a lot.  Basically it works by expanding the search tree one node at a time by doing random playouts from a leaf node and then propograting results back up to the top of the tree.  It then chooses the next node for expansion based on a function which balances exploration of promising nodes and nodes which haven't been explored much.

After getting my general program up and running, working on using this algorithm was actually where I spent a significant amount of time.  Since I'm still a novice at Go, I figured using an algorithm which could function without needing a heuristic would be very valuable.  Indeed, finding good strategies off random moves often happens with monte carlo search.  However, I ran into a very significant issue: determining a finished game of Go is actually not easy.  Most games of Go end because both players can see that there's not any more room for them to make plays, but a computer?  Without programming knowledge of how its stones will be safe or not it's impossible for the computer to know when the game is truly over.

I spent a lot of time trying to problem solve this issue and frankly never came up with a good solution.  The best that I did was making a function which could determine if a space was an eye, i.e. an empty board position surrounded by one chain of the same color.  Then another function which did a sweep over the board to check that all open spaces were eyes.  Then, for my Monte Carlo search I could have the game continue to play out until only eyes were left on the board (and give the knowledge to not play into eyes in my AI).  Then I could just count the stones of each color on the board to determine score, as is done in the chinese scoring rules for Go. The functions are at the end of the minimax.lisp file.

While in theory I knew this idea had merit, I was still not confident that this would actually be a good way to determine a game over state of the board.  In particular since both sides can capture pieces (and perhaps large groups at once), I didn't know if in general I would run into terminal states very quickly.  This was not the only major issue though.  While having a good idea in general of the algorithm, I was unsure how to properly grow my tree in terms of adding new leaves to the search tree.  So the issue for me was that I was unsure when I pick a node to expand how many and which of its children to add to the tree.  When the branching factor is as huge as it is for Go, not having to branch out completely for every step is very important.

So despite the efforts for learning this algorithm, I decided that to for sure get an algorithm done I wanted to do the minimax algorithm.  I did this in a couple steps.  First I made a function to generate the nodes and the board states they represented.  Before looking at any board values this was important to make sure it was working properly.   Then I made a function to calculate the values, using the minimax algorithm, each node should take.

The board evaluator that I used is a very simple one.  It turns the open spaces into digits, then looks at each stone on the board and radiates influence out from that stone, stronger near the stone and weaker farther away.  No influence is given to a space occupied by a stone, and stones of the opposite color radiate negative influence.  For example a single stone would radiate influence like this:

0 0 0 1 0 0 0
0 0 1 2 1 0 0
0 1 2 3 2 1 0
1 2 3 B 3 2 1
0 1 2 3 2 1 0
0 0 1 2 1 0 0
0 0 0 1 0 0 0

There are many issues with this board evaluator, but given my limited knowledge of Go strategies its one of the few things I could think to examine (even if perhaps it takes a long time to calculate).  In order to create a better board evaluator I would need to spend sigificant more time into researching heuristics for Go and to have more experience with the game.  That said, I'm very happy knowing that my minimax algorithm works (as I tested using some random tree values and the back propogation works just fine) and that I have at least some board evaluator as well.

After getting everything working, I tried playing a game and ran into a space error seaching at depth 4 and on a 9x9 board (unsuprisingly).  I had to reduce down to a search depth of 2 and play on an even smaller board (size 6) to get moves from the opponent in any "reasonable" time frame.  That said, my algorithm works kind of like how I'd expect--it plays moves to remove influence of its opponents stones and plays spaced apart to maximize its own influence.  A 6x6 is fairly degenerate, since there's not much play for moves, but that's okay.

Personally I believe that the run time is still incredibly slow, at least for the first few moves--as the amount of moves goes down the program speeds up considerably.  A goal I was hoping for at the beginning of the project but unable to focus on was optimization.  I tried to be careful where possible when building my program, but the major issue is that I'm unsure where to analyze where my problems lie (at least in Lisp).  For example, in Python there's a nice library called profiler which can run a function and tell you where it spent its runtime.  For my program I'm not sure if improvements need to be made on populating the tree with nodes, my board evaluator, a more efficient algorithm (such as alpha-beta pruning), or even making the game rules more optimized.

I guess this presents a problem which all people workin projects face: where to go next.  If I were to work on this project in the future, improving the run time would be the first thing I would aim at.  Then after that I would want to work on developing (and comparing) various heuristics and play them against each other to see which programs do the best.  I would also love to implement a monte carlo search method, because I think the method sounds very interesting and would like to explore how it compares to my (likely horrible) heurstics in a minimax method.
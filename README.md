# aoc2017
Advent of Code 2017: Solutions in R

- Started at: 12:52, 22 February 2019
- Completed at: 11:38, 11 March 2019
- Source: https://adventofcode.com/2017

After doing Advent of Code 2018, the Synacor challenge and some other minor challenges I decided to try AoC 2017. These problems were not as challenging as the 2018 versions, but still really fun to do. This year's solutions are far more concise and clear than my code from AoC 2018, so that already is a huge improvement. Also, for many problems I knew what to do right away and this resulted in a much higher speed. Now, I can't proof it (wink-wink), but I'm sure I would have ended up in the top 100 for day 12.1. However, to really end up in the top 100 I have to get up before 6:00 in the morning and this already disqualifies me, as I am worthless in the early morning. 

The only problem I really had difficulties with, was day 24. I first tried the 'all_simple_paths'-way, which of course works with the test input, but with the larger input the memory quickly explodes. After that I think I spent a day trying to manipulate the input, suitable for shortest-path-finding algorithms (with negative edge-weights). I couldn't find a correct solution so at this point I dropped my goal of completing AoC completely without help. I turned to the forum and it was pretty clear the basic shortest-path-algorithms would not help me, I had to try recursive functions. Even with this help I couldn't manage to find a working solution, as every function I made ended up nested too deep. I thought maybe R just was not suitable for this problem and turned to Python. Now it didn't take long to get a working solution. Out of curiosity I literally translated it to R and, for whatever reason, it worked! So the lesson learned here is that sometimes it's a good idea to try another language, just to look at the problem from a slightly different perspective.  

On to the next previous year!

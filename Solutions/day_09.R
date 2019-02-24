input <- readLines("input_day_09.txt")

# Remove exclamation marks
input <- gsub('\"', "s", input)
input <- gsub("!!", "", input)
input <- gsub("!.", "", input)

# Calculate garbage for part two
inp <- gsub("<[^>]+>", "<>", input)
garb <- nchar(input) - nchar(inp)

# Remove all but brackets
inp <- gsub("[^{}]+", "", inp)

# Calculate score
score <- 0
while(nchar(inp) > 0){
  worth <- regexpr("}", inp)[1] - 1
  inp   <- sub("\\{}", "", inp)
  score <- score + worth
}

# Part One
cat(score)

# Part Two
cat(garb)

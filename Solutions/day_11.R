input <- readLines("input_day_11.txt")
inp <- unlist(strsplit(input,","))
y <- x <- inp


# Part One
y<- gsub("nw", "0.5", y)
y<- gsub("ne", "0.5", y)
y<- gsub("se", "-0.5", y)
y<- gsub("sw", "-0.5", y)
y<- gsub("n", "1", y)
y<- gsub("s", "-1", y)


x <- gsub("nw", "-1", x)
x <- gsub("ne", "1", x)
x <- gsub("sw", "-1", x)
x <- gsub("se", "1", x)
x <- gsub("n", "0", x)
x <- gsub("s", "0", x)

dx <- abs(sum(as.numeric(x)))
dy <- abs(sum(as.numeric(y)))

# Only works if dx/2 < dy and dx,dy is an uneven pair
result <- dx + (dy - dx * 0.5)
cat(result)


# Part Two

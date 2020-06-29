### Stem-and-Leaf Plot Example ###
### Author: Matt Sweitzer; email@mattsweitzer.com

# Let's start by pulling in the "mtcars" example data
# This is an example data set built into R
# You can find a codebook here: https://www.rdocumentation.org/packages/datasets/versions/3.6.2/topics/mtcars
# Obviously, if you are applying this to your own data set, this step is not necessary

data(mtcars)

# In this example, we'll create a stem-and-leaf plot of the "wt" variable
# This is the weight of the car in 1,000 pound units
# Let's take a look at the summary statistics

summary(mtcars$wt)

# So we have a minimum value of 1.513, a maximum of 5.424, and it seems like this variable is rounded to the thousandths place
# Lets start by rounding everything to the tenths place -- you can adjust the rounding by altering "digits" and "nsmall"
# Note: the "format()" function ensures that zeroes are not truncated when rounding

mtcars$wt_Rounded <- format(round(mtcars$wt, digits = 1), nsmall=1)

# Next we'll need to perform some operations to separate the ones place and the tenths place
# To accomplish this, we'll use the function "strsplit" from base R
# This takes a character string and splits it into two or more strings whenever a character sequence, specified by the user, occurs

# Lets begin by converting our "wt_Rounded" to a character class vector
# This will allow us to perform string operations, such as strsplit()

mtcars$wt_Rounded <- as.character(mtcars$wt_Rounded)

# Now, to split the vector into ones and tenths, we'll need to split on the character "." (period)
# In R, periods serve as operators as well, so we'll need to provide the split function the regular expression "[.]" instead
# Let's take a look at the output of splitting just the first observation:

strsplit(mtcars$wt_Rounded[1], split="[.]")

# This function returns a nested list object there the first index tells us which observation the result refers to
# and within that first index, we have a separate list of length 2 containing the characters "2" and "6"
# These are the ones and tenths place of the first observation!
# To convert these to a vector that we can store in our data frame, we'll wrap the function inside the unlist() function
# Let's take a look at the head to make sure it's in the right format:

head(unlist(strsplit(mtcars$wt_Rounded, split="[.]")))

# Here, we have a problem. The ones and tenths places are part of the same list!
# This appears to follow a regular pattern:
# Odd-numbered indices in our output (e.g., 1, 3, 5) contain the ones places
# and even-numbered indices in our output (e.g., 2, 4, 6) contain the tenth's place
# We can fix this by indexing the output with a repeated list of "TRUE" "FALSE"
# Let's take a look:

head(unlist(strsplit(mtcars$wt_Rounded, split="[.]"))[c(TRUE, FALSE)])

# That's all the ones places! Now to get the tenths places, we'll just need to invert our TRUE/FALSE list
# Here's what that looks like:

head(unlist(strsplit(mtcars$wt_Rounded, split="[.]"))[c(FALSE, TRUE)])

# Perfect! Now that we know how to extract the needed information, let's store these in our data frame
# We'll store the ones places in a variable called "wt_Ones", and the tenths places in a variable called "wt_Tenths"
# Note that this procedure will still work if you chose to use hundredths or thousandths place rounding

mtcars$wt_Ones <- unlist(strsplit(mtcars$wt_Rounded, split="[.]"))[c(TRUE, FALSE)]
mtcars$wt_Tenths <- unlist(strsplit(mtcars$wt_Rounded, split="[.]"))[c(FALSE, TRUE)]

# Let's take a quick look at the table of wt_Ones to get a sense of what our plot will look like:

table(mtcars$wt_Ones)

# So we have 4 cars that are between 1,000 and 1,999 pounds; 8 cars that are between 2,000 and 2,999 pounds, et cetera
# Let's create a new data frame -- in this frame, we'll store our summary data to be used in our stem-and-leaf plot
# We'll call this "SL" for stem-and-leaf
# In the first column, called "Ones", we'll store the *unique* values of wt_Ones
# For now, we'll fill the second column called "Tenths" with missing values

SL <- as.data.frame(cbind("Ones"=unique(mtcars$wt_Ones), "Tenths"=NA))

# For ease of use, let's reorder SL so that the lowest ones place is on top and the highest is on the bottom -- kind of like a stem-and-leaf plot

SL <- SL[order(SL$Ones),]

# Next comes the tricky part. We need to write a function that will take the value of Ones for each row in SL,
# ... pull all of the wt_Tenths values in mtcars where wt_Ones matches the value in SL,
# ... put them in order from lowest to highest,
# ... and return them as a character string where they are separated by a space [you could add a comma between values by altering the collapse parameter in the paste() function below]

getTenthsFn <- function(SLOnes){
  SLTenths <- mtcars$wt_Tenths[mtcars$wt_Ones==SLOnes]
  SLTenths <- SLTenths[order(SLTenths)]
  SLTenths <- paste(unlist(SLTenths), collapse=" ")
  return(SLTenths)
}

# Let's try this out on the first row:

getTenthsFn(SL$Ones[1])

# It works! Now we need to apply this to every observation in SL$Ones
# To accomplish this, we'll unlist the results of lapply() -- lapply applies a function to every member of a vector
# We'll then save the results into SL$Tenths

SL$Tenths <- unlist(lapply(SL$Ones, getTenthsFn))

# The last piece of information we'll need is the length of the tenths place
# This will help us determine how wide our stem-and-leaf plot should be
# Since the table of mtcars$wt_Ones is already in the same order as our SL$Ones, we can simply convert the table to a numeric vector
# Double-check this with your own data!
# We'll just store that in a new variable in SL called "TLength"

SL$TLength <- as.numeric(table(mtcars$wt_Ones))

# Now, let's take a look at SL:

SL

# This looks EXACTLY like a stem-and-leaf plot!
# Now we just need to make it pretty and save the plotted results
# With R plotting, it helps to think of everything as though it's laid out on a grid
# We know in this example that we have 5 rows -- we'll probably want to add a sixth to add the headers "Stem" and "Leaf"
# We also know that the width is going to be variable, depending on how many observations we have for each ones place
# Since there are 16

# Let's just start by creating an empty plot
# This will use type="n" to avoid plotting anything
# We'll then set xlim and ylim using the width of the tenths column (TLength) and the length of the ones column from SL
# We'll add 1 to each of these to add the extra column and row (respectively) needed for the plot
# Finally, we'll remove axes and labels for now

plot(1, 1, type='n', xlim=c(1, max(SL$TLength)+1), ylim=c(1, length(SL$Ones)+1), main='', xlab='', ylab='', axes=F)

# Now let's add in the headers "Stem", and "Leaf"
# To do this, we'll call the function text() *WHILE THE PLOT WINDOW IS STILL OPEN!*
# text() takes 3 arguments: the x-coordinate, y-coordinate, and the text to be displayed
# We'll add to that "font=2" to make it bold
# ... and "cex=1.25" to make it 125% bigger than the default

# "Stem" should be in the top-left position in our grid
# This means x should be 1 and y should be the length of SL$Ones plus 1

text(1, length(SL$Ones)+1, "Stem", font=2, cex=1.25)

# "Leaf" should be in the top row of our grid and centered between columns 2 and the right-most column
# We can get there by setting x to the mean of 2 and the maximum of SL$TLength

text(mean(c(2, max(SL$TLength)+1)), length(SL$Ones)+1, "Leaf", font=2, cex=1.25)

# Great! Now, let's add in the vertical and horizontal lines that make the plot readable
# Again, if you think of the plotting space like a grid, we want two lines:
# One beneath the top row and above the second row that spans across the whole width
# and one between the first and second columns that goes from the bottom up to the first line, but not into the top row
# We'll use increments of .5 in our grid space to find the space in the middle of two rows or columns

# The first line is easiest since it spans the full width of the plot
# We'll us the "abline" function to draw a horizonal line (using the parameter "h")
# And since we want it just below our top row, we'll use "length(SL$Ones)+0.5"

abline(h=length(SL$Ones)+0.5)

# We'll then use the "segments()" command to draw the second line
# This takes four parameters to start: the starting x position, the starting y position, the ending x position, and the ending y position (in that order)
# Both the starting x and ending x are easy: we want them to the right of the first column, so we'll give the function "1.5" for both
# The starting y should be below the bottom column, so we'll provide 0.5 -- since this is below the y limit set in "plot()", we'll add "xpd=T" to tell R to draw the line beyond the limit
# Finally, the ending y should be in between the first and second rows, so we'll use the same "length(SL$Ones)+0.5" as above

segments(1.5, 0.5, 1.5, length(SL$Ones)+0.5, xpd=T)

# Now we can start adding text; we'll start with the "Stem" column, or the "Ones" variable in our SL data frame
# We'll again use "text()" to add text to our plotting space
# For the x position, we want everything to appear in the first column, so "1"
# For the y position, we'll want to provide a list of values
# These values should indicate the second row (length(SL$Ones)) though the bottom row (1), so "c(length(SL$Ones):1)"
# Finally, for the text, we can just provide "SL$Ones"

text(1, c(length(SL$Ones):1), SL$Ones)

# To fill in the "Leafs", we'll need a more complicated function
# We'll need to be able to add text to each row from 2 to the bottom
# and we'll need to add to each column from left to right depending on the size of TLength for the corresponding row

# To accomplish this, we'll use two nested for loops:
# The first one will cycle through the values 1 through the length of SL$Ones (rows)
# The next will cycle through the values 1 through the TLength for the corresponding row (columns)
# Then inside those loops, we'll call the "text()" function, using "j+1" to indicate the column we want to write to
# .. and "length(SL$Ones)-i+1" to indicate the row we want to write to
# Finally, we'll take the text by splitting the "Tenths" value on spaces, unlisting the results, and indexing the j-ith entry from the list

for(i in 1:length(SL$Ones)){
  for(j in 1:SL$TLength[i]){
    text(j+1, length(SL$Ones)-i+1, unlist(strsplit(SL$Tenths[i], " "))[j])
  }
}

# That's it! We have everything we need! Now to save out the results, we'll call the function "pdf()"
# This function tells R to save a pdf file to the current working directory (see "getwd()" to know where it saves on your computer, or "setwd()" to change it)
# It will then fill that pdf with whatever plotting functions follow
# then we can tell R that we're done plotting by using the "dev.off()" command

# pdf() takes 4 parameters: a string indicating the name of the file, width (inches), height (inches), and family (font -- see "family" in ?postscript for other options)
# We'll use the name "SL_Example.pdf" as the name
# I used width and height values that fit the current data -- you'll have to adjust those values to fit your own data if you're adapting this code
# You can also try adjusting the size of text using the "cex" parameter in "text()" functions
# Finally, you can adjust the margins of the plot in the "par()" function by setting the values of the "mar" parameter -- these values adjust clockwise from the bottom side

pdf("SL_Example.pdf", width=10, height=2.5, family="Times")
par(mar=c(0.4, 0.2, 0.4, 0.2))
plot(1, 1, type='n', xlim=c(1, max(SL$TLength)+1), ylim=c(1, length(SL$Ones)+1), main='', xlab='', ylab='', axes=F)
text(1, length(SL$Ones)+1, "Stem", font=2, cex=1.25)
text(mean(c(2, max(SL$TLength)+1)), length(SL$Ones)+1, "Leaf", font=2, cex=1.25)
abline(h=length(SL$Ones)+0.5)
segments(1.5, 0.5, 1.5, length(SL$Ones)+0.5, xpd=T)
text(1, c(length(SL$Ones):1), SL$Ones)
for(i in 1:length(SL$Ones)){
  for(j in 1:SL$TLength[i]){
    text(j+1, length(SL$Ones)-i+1, unlist(strsplit(SL$Tenths[i], " "))[j])
  }
}
dev.off()

# And that's it! Please feel free to email me with any questions: email@mattsweitzer.com

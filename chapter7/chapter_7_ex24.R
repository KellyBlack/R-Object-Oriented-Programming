# File simpleExecute.R
# This is a simple example used to demonstrate the source command.
# This script will prompt the person running it to enter a number,
# and it will find the square root of the number. It tests the original
# number to make sure it is positive and prints out an appropriate
# warning message if it is negative.

x <- as.double(readline("What is the value of x? "))     # Read in a number
cat("I got the number ",format(x,digits=6),".\n")
if(x < 0)
    {
        # The number is negative. What are they thinking?
        print("Why would you give me a negative number?")
        x <- abs(x)
    }

# Find the square root and assign it the variable "y."
y <- sqrt(x)

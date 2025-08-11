#Welcome to R!
#R can be used for basic functions
2+2

#you can set variables 
a <- 2+2
a

#there are built in functions
print("Hello world")
max(2,5,3,9,1)
min(2,5,3,9,1)

#there are different object types including numbers, strings, and lists
class(2)
class("hello")
class(c("I", "love", "sleep"))
class(list("I", "love", "sleep"))
#we will mostly be working with dataframes
df <- data.frame(numbers = c("A", "B", "C", "D"), integers = c(1,2,3,4))

#we can use functions to analyze and plot data
plot(x = c(1,2,3,4), y = c(5, 10, 2, 5))

#plotting
example_x <- c(1,1,2,4,5,6,7,8,8,7,6,5.5,5, 4,4,4,2,1)
example_y <- c(4,12,9,12,12,12,12,10,7,5,5,5, 1, 5,5,5,7,4)
plot(x = example_x, y = example_y, type = "b", col = "blue", lwd = 4)
plot(x = example_x, y = example_y, type = "l", col = "blue", lwd = 4)
symbols(6.5,9, circles = 0.1, add = TRUE, bg = "red", lwd = 4, inches = 0.3)
symbols(6.5,9, circles = 0.1, add = TRUE, bg = "black", lwd = 4, inches = 0.1)

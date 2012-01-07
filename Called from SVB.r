
#' test the parameters passed to the script:

if (exists("one") && exists("two")) {
	cat("\nInteresting fact:\n\t", one, "+", two, "=", one + two, "\n")
	cat("type of 'one' is:\n")
	mode(one) # display var type
}

if (exists("spreadsheet"))  print(spreadsheet)
if (exists("number_array")) print(number_array)	
if (exists("string_array")) print(string_array)
if (exists("number")) print(number)
if (exists("string")) print(string)

if (exists("STATISTICA.Version")) cat("STATISTICA Version:", STATISTICA.Version, "\n")

if (exists("func")) func("some string parameter to func()")

#' basic operations / graphs:

x <- rnorm(100)           # 100 random numbers from a normal(0,1) distribution
y <- exp(x) + rnorm(100)  # an exponential function with error

result <- lm(y ~ x)      # regress x on y and store the results
summary(result)          # print the regression results

plot(x,y)                 # pretty obvious what this does
abline(result)            # add the regression line to the plot
lines(lowess(x,y), col=2) # add a nonparametric regression line (a smoother)

hist(result$residuals)    # histogram of the residuals from the regression

#' built-in datasets:

RouteOutput(Orange, "Growth of Orange Trees")
RouteOutput(WorldPhones, "Number of Telephones Worldwide")

#' use R to make any output you want:

comments <- matrix(c("Some comments", "and more comments..."), byrow=TRUE)
RouteOutput(comments, "Comments Section")

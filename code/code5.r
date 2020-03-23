# homework2 - question 5 part A
print(dbinom(0:10, 10, 0.97))
barplot(dbinom(0:10, 10, 0.97), main="0-10 Chips", 
        ylab = "probability", names.arg=c(0:10))

# homework2 - question 5 part B
print(dbinom(8, 10, 0.97))
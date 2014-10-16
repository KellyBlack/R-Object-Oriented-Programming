source('grades.R')
source('ops.R')
source('overriding.R')

course <- Course()
course <- SetFileName(course,"shortList.csv")
course <- ReadGrades(course)
print(course['test1'])
print(course['test1',3])

course['test1',3] <- 99.1
print(course['test1',3])

x <- course['test1']
summary(x)


p <- course['project1']
summary(p)


course <- SetFileName(course,"math100.csv")
course <- ReadGrades(course)
x <- course['test1']
y <- course['test2']
z <- (x + y)/2
print(z)

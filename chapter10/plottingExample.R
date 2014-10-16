source('grades.R')
source('ops.R')
source('overriding.R')

course <- Course()
course <- SetFileName(course,"math100.csv")
course <- ReadGrades(course)

x <- course['test1']
y <- course['test2']
z <- course['project1']

print(summary(y))
plot(y,main="Results from Test 2",maxGrade=100,lowerBound=10)
plot(z,main="Results from Project 1")

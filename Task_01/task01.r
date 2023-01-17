# 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
library (swirl)
swirl ()
Tyler
1
2
getwd()
ls()
x <- 9
ls()
list.files()
?list.files
args(list.files)
old.dir <- getwd()
dir.create("testdir")
setwd("testdir")
file.create("mytest.R")
file.exists("mytest.R")
file.info("mytest.R")
fie.info("mytest.R")$mode
file.rename("mytest.R", "mytest2.R")
file.remove("mytest2.R")
file.copy("mytest2.R", "mytest3.R")
file.path("mytest3.R")
file.path("folder1", "folder2")
?dir.create
dir.create(file.path('testdir2', 'testdir3'), recursive=TRUE)           
setwd(old.dir)
1
1
3
1:20
pi:10
15:1
?':'
seq(1,20)
seq(0,10, by=0.5)
my_seq <- seq(5,10, length=30)
length(my_seq)
1:length(my_seq)
seq(along.with = my_seq)
seq_along(my_seq)
rep(0,times=40)
rep(c(0,1,2), times=10)
rep(c(0,1,2), each=10)
1
1
4
num_vect <- c(0.5,55,-10,6)
tf <- num_vect < 1
2
tf
num_vect >= 6
1
2
1
my_char <- c("My", "name", "is")
my_char
paste(my_char, collapse = " ")
my_name <- c(my_char, "Tyler")
my_name
paste(my_name, collapse = " ")
paste("Hello", "world!", sep= " ")
paste(1:3, c("X", "Y", "Z"), sep= "")
paste(LETTERS, 1:4, sep= "-")
1
1
5
x <- c(44,NA,5,NA)
x*3
y <- rnorm(1000)
z <- rep(NA, 1000)
my_data <- sample(c(y,z),100)
my_na <- is.na(my_data)
my_na
my_data == NA
sum(my_na)
my_data
0/0
Inf-Inf
1
1
6
x
x[1:10]
4
x[is.na(x)]
y <- x[!is.na(x)]
y
3
y[y>0]
x[x>0]
x[!is.na(x) & x>0]
x[c(3,5,7)]
x[0]
x[3000]
x[c(-2,-10)]
x[-c(2,10)]
vect <- c(foo=11, bar=2, norf=NA)
vect
names(vect)
vect2 <- c(11,2,NA)
names(vect2) <- c("foo", "bar", "norf")
identical(vect, vect2)
3
vect["bar"]
vect[c("foo", "bar")]
1
1
7
my_vector <- 1:20
my_vector
dim(my_vector)
length(my_vector)
dim(my_vector) <- c(4,5)
dim(my_vector)
attributes(my_vector)
my_vector
class(my_vector)
my_matrix <- my_vector
?matrix
my_matrix2 <- matrix(1:20, nrow=4, ncol=5, byrow=FALSE)
identical(my_matrix, my_matrix2)
patients <- c("Bill", "Gina", "Kelly", "Sean")
cbind(patients, my_matrix)
my_data <- data.frame(patients, my_matrix)
my_data
class(my_data)
cnames <- c("patient", "age", "weight", "bp", "rating", "test")
colnames(my_data) <- cnames
my_data
2
1
8
TRUE==TRUE
(FALSE==TRUE)==FALSE
6==7
6<7
10<=10
1
2
5 != 7
!(5==7)
2
3
FALSE & FALSE
TRUE & c(TRUE, FALSE, FALSE)
TRUE && c(TRUE, FALSE, FALSE)
TRUE | c(TRUE, FALSE, FALSE)
TRUE || c(TRUE, FALSE, FALSE)
5>8 || 6 != 8 && 4>3.9
1
2
isTRUE(6>4)
3
identical('twins', 'twins')
2
xor(5==6, !FALSE)
1
ints <- sample(10)
ints
ints>5
which(ints>7)
2
any(ints<0)
all(ints>0)
2
1
1
9
Sys.Date()
mean(c(2,4,5))
submit()
boring_function('My first function!')
boring_function
submit()
my_mean(c(4,5,10))
submit()
remainder(5)
remainder(11,5)
remainder(divisor=11, num=5)
remainder(4,div=2)
args(remainder)
submit()
evaluate(sd, c(1.4, 3.6, 7.9, 8.8))
evaluate(function(x){x+1}, 6)
evaluate(function(x){x[1]}, c(8,4,0))
evaluate(function(x){x[length(x)]}, c(8,4,0))
?paste
paste("Programming", "is", "fun!")
submit()
telegram(c("Hello"))
submit()
mad_libs(place="Savannah", adjective= "lovely", noun= "person")
submit()
"I" %p% "love" %p% "R!"
2
1
15
data(cars)
?cars
head(cars)
plot(cars)
?plot
plot(x=cars$speed, y=cars$dist)
plot(x=cars$dist, y=cars$speed)
plot(x=cars$speed, y=cars$dist, xlab="Speed")
plot(x=cars$speed, y=cars$dist, ylab= "Stopping Distance")
plot(x=cars$speed, y=cars$dist, xlab="Speed", ylab= "Stopping Distance")
plot(cars, main= "My Plot")
plot(cars, sub= "My Plot Subtitle")
plot(cars, col=2)
plot(cars, xlim= c(10,15))
plot(cars, pch=2)
data(mtcars)
?boxplot
boxplot(mpg~ cyl, data=mtcars)
hist(mtcars$mpg)
2

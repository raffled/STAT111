########################### gradingScript.R ###########################
#### This script reads in a .csv of grades exported from MyStatLab
#### and calculates total score based on syllabus.

#### MyStatLab has an awkward excel-format.  Get assignment names first.
assign.names <- unlist(strsplit(readLines("stat111.csv", n = 2)[2],
                                ","))[-c(1:4, 6)]
#### get col. inds for assignment type.
inds.list <- list(hw = grepl("Homework", assign.names),
                  quiz = grepl("Quiz", assign.names),
                  labs = grepl("Lab|Practice", assign.names),
                  final = grepl("Exam", assign.names)
)

#### read in the grade table and fix col names
grades.raw <- read.csv("stat111.csv", skip = 5,
                       stringsAsFactors = FALSE)[-(21:26), -c(6, 33)]
colnames(grades.raw)[-(1:4)] <- assign.names

#### get names/emails/etc
id <- grades.raw[,1:3]

#### fix NA's
scores <- apply(grades.raw[,-(1:4)], 2, function(col){
                    col[is.na(col)] <- 0
                    col
                }
)

#### calculate total grades
n <- c(10, 5, 5, 1)
final <- apply(scores, 1, function(i){
                   means <- sapply(1:4, function(j){
                       if(j == 2){
                           q.vec <- i[inds.list[[j]]]
                           q.vec <- append(q.vec, mean(q.vec[c(2, 6)]))
                           mean(head(sort(q.vec[-c(2, 6)], TRUE), n[j]))
                       } else {
                           mean(head(sort(i[inds.list[[j]]], TRUE), n[j]))
                       }
                   })
                   round(sum(means)/3)
               }
)

final.df <- data.frame(id, final)

stem(final, scale = 2)

letter <- cut(final, c(0, seq(60, 100, 10)),
              rev(LETTERS[c(1:4, 6)]),
              right = FALSE)
rev(table(letter))
data.frame(final.df, letter)[order(final, decreasing = TRUE),]

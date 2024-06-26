% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{wisc}
\alias{wisc}
\title{Fake student data from the Wisconsin State Dept. of Ed}
\format{
A data frame with 2700 rows and 26 variables:
\describe{
  \item{student_id}{numeric: student's unique ID #}
  \item{grade}{numeric: grade level}
  \item{district}{numeric: district code}
  \item{school}{numeric: school code}
  \item{white}{numeric: is the student white?}
  \item{black}{numeric: is the student black?}
  \item{hisp}{numeric: is the student Hispanic?}
  \item{indian}{numeric: is the student Native-American Indian?}
  \item{asian}{numeric: is the student Asian?}
  \item{econ}{numeric: is the student economically-disadvantaged?}
  \item{female}{numeric: is the student female?}
  \item{ell}{numeric: is the student an English Language Learner?}
  \item{disab}{numeric: does the student have a learning disability?}
  \item{year}{numeric: school year}
  \item{attday}{numeric: days attended}
  \item{readSS}{numeric: student's reading standardized test score}
  \item{mathSS}{numeric: student's math standardized test score}
  \item{proflvl}{factor: student's proficiency level}
  \item{race}{factor: student's single-category race}
  ...
}
}
\source{
\url{https://github.com/jknowles/r_tutorial_ed/}, posted under a Creative Commons license. The script used to generate the data set is here, although not very well documented: \url{https://github.com/jknowles/r_tutorial_ed/blob/master/data/simulate_data.R}
}
\usage{
wisc
}
\description{
A generated data set containing data on 1200 imaginary individual K-12 students in Wisconsin.  They are nested within 6 schools in 3 districts.  In adapting this from the source, Sam switched the school and district variables (there had been multiple districts per school) and made other minor changes, including dropping columns that I didn't understand or that didn't seem relevant (e.g., variables like "luck" that were used to calculate the reading and math scores).
}
\keyword{datasets}

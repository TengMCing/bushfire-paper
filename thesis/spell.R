library(spelling)

rmd <- rmd[grep("^[A-Z]-[0-9]{2}", rmd)]

rmd <- c("index.Rmd",
         "01-Abstract.Rmd",
         "02-Introduction.Rmd",
         "03-Review_of_literature.Rmd",
         "04-Data.Rmd",
         "05-Classifier.Rmd",
         "06-Risk.Rmd",
         "07-Discussion.Rmd",
         "08-Conclusion.Rmd",
         "A-appA.Rmd",
         "B-appB.Rmd",
         "C-appC.Rmd")


ignore <- readLines("WORDLIST")

check_spelling <- spell_check_files(
  rmd,
  ignore = ignore,
  lang = "en_GB"
)

if (nrow(check_spelling) > 0) {
  print(check_spelling)
  stop("Check spelling in Rmd files!")
}

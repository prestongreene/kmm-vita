#! /usr/bin/Rscript
setwd("./scripts")
source("generate_cv.r")
generate_cv("../content/middleton-full-vita.yaml",
            "../style/jk-vita.sty",
            "../output")

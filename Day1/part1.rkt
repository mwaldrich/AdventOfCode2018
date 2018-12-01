#lang racket

(foldr + 0 (file->list "input.txt"))

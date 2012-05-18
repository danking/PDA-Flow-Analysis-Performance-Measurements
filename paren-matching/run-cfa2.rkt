#lang racket

(require "paren-pda.rkt")
(require "../run-cfa2.rkt")

(pretty-print (run-cfa2 paren-pda-risc))


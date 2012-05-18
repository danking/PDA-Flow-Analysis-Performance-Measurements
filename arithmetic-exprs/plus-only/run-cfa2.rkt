#lang racket

(require "pda.rkt")
(require "../../run-cfa2.rkt")

(pretty-print (run-cfa2 arithmetic-plus-only-pda-risc))

#lang racket

(require "calculator-pda.rkt")
(require "../run-cfa2.rkt")

(pretty-print (run-cfa2 calculator-pda-risc))

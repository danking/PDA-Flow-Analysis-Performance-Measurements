#lang racket

(require "pda.rkt")
(require "../../run-cfa2.rkt")

(run-cfa2 arithmetic-plus-only-pda-risc #:debug 0)

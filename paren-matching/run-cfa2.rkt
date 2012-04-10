#lang racket

(require "paren-pda.rkt")
(require "../run-cfa2.rkt")

(run-cfa2 paren-pda-risc #:debug 0)

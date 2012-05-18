#lang racket

(require "pda.rkt")
(require "../../../run-cfa2.rkt")

(pretty-print (run-cfa2 polish-notation-plus-pda-risc))

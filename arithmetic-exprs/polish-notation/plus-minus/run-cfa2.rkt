#lang racket

(require "pda.rkt")
(require "../../../run-cfa2.rkt")

(run-cfa2 polish-notation-plus-minus-pda-risc #:debug 0)
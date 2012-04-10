#lang racket

(require "pda.rkt")
(require "../../run-cfa2.rkt")

(file-stream-buffer-mode (current-output-port) 'none)

(pretty-print (run-cfa2 arithmetic-pda-risc #:debug 0))

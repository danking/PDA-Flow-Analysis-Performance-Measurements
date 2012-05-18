#lang racket

(require "ansi-c-pda.rkt")
(require "../run-cfa2.rkt")

(file-stream-buffer-mode (current-output-port) 'none)

(pretty-print (run-cfa2 ansi-c-pda-risc))

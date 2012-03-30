#lang racket

(require "ansi-c-pda.rkt")
(require "../run-cfa2.rkt")

(file-stream-buffer-mode (current-output-port) 'none)

(run-cfa2 ansi-c-pda-risc #:debug 0)

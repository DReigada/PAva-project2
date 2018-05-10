#lang racket

(require
    "extras/settersGetters.rkt"
    "preprocess.rkt")

(add-active-token "@Setters" process-setters)
(add-active-token "@Getters" process-getters)
(add-active-token "@SettersGetters" process-getters-and-setters)
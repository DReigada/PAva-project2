#lang racket

(require
    "extras/settersGetters.rkt"
    "preprocess.rkt")

(add-active-token "@Setters" process-setters)
(add-active-token "@Getters" process-getters)
(add-active-token "@SettersGetters" process-getters-and-setters)
(add-active-token "@ValueClass" process-value-class)
(add-active-token "@Constructor" process-constructor)

#lang racket
(require rsound)
;(play ding)
;(audio? (path->audio "./八号风球.mp3"))
(define VOLUME 0.1)
(define FREQUENCY 430)
 (play ding)
(define (sine-tone f)
  (* VOLUME (sin (* 2 pi FREQUENCY (/ f FRAME-RATE)))))
 
;(play (build-sound (* 1 FRAME-RATE) sine-tone))
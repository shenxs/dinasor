#lang racket
(require  picturing-programs
          )
;;游戏所需的一些辅助函数
;;尽量不涉及图片数据
;;鸡蛋不能放在一个篮子里面
;;captian my captian
(define (color->n p x y )
  (let([P (get-pixel-color x y p)])
    (cond
      [(or (color=? (color 247 247 247 255)  P)
           (color=? (color 255 255 255 255)  P)) 3];;白
      [(or (color=? (color 255 255 255 0) P)
           (color=? (color 0 0 0 0)       P)) 0];;透明
      [(or (color=? (color 83 83 83 255) P)
           (color=? (color 84 84 84 255) P)) 1];;黑
      [else  (display P)(error "未知颜色")])))

(define (to a b)
  (cond
    [(= a b) (cons b '())]
    [(a . > . b) (error "a 比b大")]
    [else (cons a (to (+ a 1) b ))]))

(define (img->list/n p x0 y0 x y)
  ;(display x) (display " ") (displayln y)
  (for*/list ([i ( x0 . to . x)]
              [j ( y0 . to . y)])
    (color->n p i j)))


(define (img->list p x0 y0 x y)
  ;(display x) (display " ") (displayln y)
  (for*/list ([i ( x0 . to . x)]
              [j ( y0 . to . y)])
    (get-pixel-color  i j p)))

(define (touch? p1 p2 s-x s-y e-x e-y)
  ;(display p1) (displayln "\n") (display p2) (display s-x)
  (local ((define p1-list (img->list/n p1 s-x s-y e-x e-y))
          (define p2-list (img->list/n p2 s-x s-y e-x e-y))
          (define (foo l1 l2)
            ;(display (first l1)) (displayln (first l2))
            (cond
              [(and (empty? l1) (empty? l2)) #f]
              ;[(= (first l1) (first l2)) (display "一样")(foo (rest l1) (rest l2))]
              [(and (= 1 (first l1))
                    (= 3 (first l2))) #t]
              [else (foo (rest l1) (rest l2))]))
          )
    ;(displayln p1-list)
    ;(displayln p2-list)
    (foo p1-list p2-list)))

(provide (all-defined-out))
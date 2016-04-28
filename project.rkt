#lang racket
(require gigls/unsafe)

(define next-steps
  (let ([transcribe
         (lambda (dir)
           (cond
             [(equal? 'u dir) 'r]
             [(equal? 'r dir) 'd]
             [(equal? 'd dir) 'l]
             [(equal? 'l dir) 'u]
             [else (error "The following is not u,d,l,r:" dir)] 
             ))])
    (lambda (lst)
      (append lst (map transcribe (reverse lst))))))

(define generate-steps
  (lambda (steps iteration)
    (if (zero? iteration)
        steps
        (generate-steps (next-steps steps) (- iteration 1)))))

(define move-dragon
  (lambda (step)
    (cond
      [(equal? 'r step)
       (turtle-face! dragon 0)
       (turtle-forward! dragon side-length)]
      [(equal? 'd step)
       (turtle-face! dragon 90)
       (turtle-forward! dragon side-length)]
      [(equal? 'l step)
       (turtle-face! dragon 180)
       (turtle-forward! dragon side-length)]
      [(equal? 'u step)
       (turtle-face! dragon 270)
       (turtle-forward! dragon side-length)]
      [else
       (error "move-dragon: The following is not u,d,l,r:" step)]
      )))

(define dragon-curve-turtle
  (lambda (col row dir iterations)
    (turtle-teleport! dragon col row)
    (map move-dragon (generate-steps (list dir) iterations))
    ))

(define clear
  (lambda (image)
    (context-set-fgcolor! (irgb 0 0 0))
    (image-select-all! image)
    (image-fill-selection! image)
    (image-select-nothing! image)
    (context-set-fgcolor! (irgb 255 255 255))
    ))


(define side-length 20)
(define canvas (image-show (image-new 1000 1000)))
(define dragon (turtle-new canvas))
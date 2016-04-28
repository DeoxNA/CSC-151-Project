#lang racket
(require gigls/unsafe)

(define next-steps
  (let ([orthogonal (compose (r-s mod 360) (r-s + 90))])
    (lambda (lst)
      (append lst (map orthogonal (reverse lst))))))

(define generate-steps
  (lambda (iterations)
    (let kernel ([steps (list 0)]
                 [i 0])
      (if (= i iterations)
          steps
          (kernel (next-steps steps) (+ i 1))))))

(define move-dragon
  (lambda (turtle step angle)
    (turtle-face! turtle (+ step angle))
    (turtle-forward! turtle side-length)))

(define dragon-curve
  (lambda (turtle col row angle iterations)
    (turtle-teleport! turtle col row)
    (map (section move-dragon turtle <> angle)
         (generate-steps iterations))))

(define clear
  (lambda (image)
    (let ([current-color (context-get-fgcolor)])
      (context-set-fgcolor! (irgb 255 255 255))
      (image-select-all! image)
      (image-fill-selection! image)
      (image-select-nothing! image)
      (context-set-fgcolor! current-color)
      )))


(define side-length 20)
(define canvas (image-show (image-new 1000 1000)))
(define dragon (turtle-new canvas))

(define asterisk
  (lambda (n col row radius)
(let ([angle (/ (* 2 pi) n)])
    
    (image-draw-line! image col1 row1 col2 row2)


    
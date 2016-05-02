#lang racket
(require gigls/unsafe)

(define vector-length
  (lambda (vec)
    (sqrt (+ (square (car vec)) (square (cdr vec))))))

;(define canvas (image-show (image-new 1000 1000)))
;(define dragon (turtle-new canvas))

(define project-background
  (lambda (n width height color1 color2)
    (image-compute (lambda (col row)
                     (cond [(equal? n 1)
                            (irgb (+ (irgb-red color1)
                                     (* (- (irgb-red color2)
                                           (irgb-red color1))
                                        (/ row height)))
                                  (+ (irgb-green color1)
                                     (* (- (irgb-green color2)
                                           (irgb-green color1))
                                        (/ row height)))
                                  (+ (irgb-blue color1)
                                     (* (- (irgb-blue color2)
                                           (irgb-blue color1))
                                        (/ row height))))]
                           [(equal? n 2)
                            (irgb (+ (irgb-red color1)
                                     (* (- (irgb-red color2)
                                           (irgb-red color1))
                                        (/ (sqrt (+ (square col)
                                                    (square row)))
                                           (sqrt (+ (square height)
                                                    (square width))))))
                                  (+ (irgb-green color1)
                                     (* (- (irgb-green color2)
                                           (irgb-green color1))
                                        (/ (sqrt (+ (square col)
                                                    (square row)))
                                           (sqrt (+ (square height)
                                                    (square width))))))
                                  (+ (irgb-blue color1)
                                     (* (- (irgb-blue color2)
                                           (irgb-blue color1))
                                        (/ (sqrt (+ (square col)
                                                    (square row)))
                                           (sqrt (+ (square height)
                                                    (square width)))))))]
                           [(equal? n 3)
                            (irgb (+ (irgb-red color1)
                                     (* (- (irgb-red color2)
                                           (irgb-red color1))
                                        (/ (sqrt (+ (square (- width col))
                                                    (square row)))
                                           (sqrt (+ (square height)
                                                    (square width))))))
                                  (+ (irgb-green color1)
                                     (* (- (irgb-green color2)
                                           (irgb-green color1))
                                        (/ (sqrt (+ (square (- width col))
                                                    (square row)))
                                           (sqrt (+ (square height)
                                                    (square width))))))
                                  (+ (irgb-blue color1)
                                     (* (- (irgb-blue color2)
                                           (irgb-blue color1))
                                        (/ (sqrt (+ (square (- width col))
                                                    (square row)))
                                           (sqrt (+ (square height)
                                                    (square width)))))))]))
                   width height)))

(define background-color-helper
  (lambda (n)
    (cond [(equal? n 0)
           ;(context-set-fgcolor! color)
           (list (irgb 0 0 0) (irgb 86 29 116))]
          [(equal? n 1)
           (list (irgb 234 37 17) (irgb 122 54 157))]
          [(equal? n 2)
           (list (irgb 171 216 248) (irgb 48 72 189))]
          [(equal? n 3)
           (list (irgb 49 189 140) (irgb 52 66 134))]
          [(equal? n 4)
           (list (irgb 255 165 0) (irgb 87 22 17))])))

(define clear
  (lambda (image)
    (let ([current-color (context-get-fgcolor)])
      (context-set-fgcolor! (irgb 255 255 255))
      (image-select-all! image)
      (image-fill-selection! image)
      (image-select-nothing! image)
      (context-set-fgcolor! current-color))))

(define generate-steps
  (let* ([orthogonal (compose (r-s mod 360) (r-s + 90))]
         [next-steps
          (lambda (lst)
            (append lst (map orthogonal (reverse lst))))])
    (lambda (iterations)
      (let kernel ([steps (list 0)]
                   [i 0])
        (if (= i iterations)
            steps
            (kernel (next-steps steps) (+ i 1)))))))

(define dragon-curve
  (lambda (turtle col row length angle aspect-ratio iterations color)
    (let* ([vec-atan
            (lambda (vec)
              (let ([ratio (/ (cdr vec) (car vec))])
                (if (< (car vec) 0)
                    (+ pi (atan ratio))
                    (atan ratio))))]
           [rad-angle (degrees->radians angle)]
           [unit-vector (cons (cos rad-angle) (sin rad-angle))]
           [ortho-vector (cons (- (sin rad-angle))
                               (cos rad-angle))]
           [scaled-vector (cons (* (car unit-vector) length aspect-ratio)
                                (* (cdr unit-vector) length))]
           [scaled-ortho-vector (cons (* (car ortho-vector) length aspect-ratio)
                                      (* (cdr ortho-vector) length))]
           [rad-scale-angle (vec-atan scaled-vector)]
           [scale-angle (round (radians->degrees rad-scale-angle))]
           ;[scale-angle angle]
           [len-start (vector-length scaled-vector)]
           [len-orthogonal (vector-length scaled-ortho-vector)]
           [move-dragon
            (lambda (step)
              (turtle-face! turtle (+ step scale-angle))
              (if (or (= step 0) (= step 180))
                  (turtle-forward! turtle len-start)
                  (turtle-forward! turtle len-orthogonal)))])
      (turtle-set-color! turtle color)
      (turtle-set-brush! turtle "2. Hardness 100" 0.2)
      (turtle-teleport! turtle col row)
      (for-each move-dragon
                (generate-steps iterations)))))


(define dragons-offset
  (lambda (turtle n col row radius step-length start-angle step-angle offset-angle aspect-ratio iterations color)
    (let* ([draw-dragon (section dragon-curve turtle <> <> step-length <> aspect-ratio iterations <>)]
           [degree-angles (map (compose
                                (r-s + start-angle)
                                (r-s * step-angle))
                               (iota n))]
           [rad-angles (map degrees->radians
                            degree-angles)]
           [offset-angles (map (r-s + offset-angle)
                               degree-angles)]
           [cols (map (compose
                       (r-s + col)
                       (r-s * radius)
                       cos)
                      rad-angles)]
           [rows (map (compose
                       (r-s + row)
                       (r-s * radius)
                       sin)
                      rad-angles)]
           [alternate-color
            (lambda (n)
              (if (even? n)
                  color
                  (irgb-add (irgb 32 32 32) color)))]
           [colors (map alternate-color
                    (iota n))])
      (for-each draw-dragon cols rows offset-angles colors))))

;Sea weed? (dragons-offset dragon 3 500 500 200 15 240 30 0 6) 


;aspect-ratio is horizontal / vertical
(define n-star
  (lambda (image n col row radius angle aspect-ratio color)
    (context-set-fgcolor! color)
    (context-set-brush! "2. Hardness 100" 0.2)
    (let* ([rad-angle (degrees->radians angle)]
           [interior-angle (/ (* 2 pi) n)]
           [angles (map (compose
                         (r-s + rad-angle)
                         (r-s * interior-angle))
                        (iota n))]
           [draw-spokes
            (lambda (theta)
              (image-draw-line! image col row
                                (+ col (*  radius (sin theta) aspect-ratio))
                                (+ row (* radius (cos theta)))))])
      (for-each draw-spokes angles))))

(define chaos
  (lambda (r x0 iterations)
    (let kernel ([n 0]
                 [xn x0])
      (if (equal? n iterations)
          xn
          (kernel (+ 1 n) (* r xn (- 1 xn)))))))

(define chaos-coordinates
  (lambda (x opt divisions)
    (map (compose (r-s + 1)
                  (r-s mod (- divisions 1))
                  (section chaos 3.6 <> 6))
         (map (compose
               (section + 2 opt <>))
              (iota x)))))


(define image-series
  (lambda (n width height)
    (let* ([background-colors 
            (background-color-helper (modulo n 5))]
           [background (project-background (ceiling (/ (+ 1 (modulo n 15)) 5))
                                           width
                                           height
                                           (car background-colors)
                                           (cadr background-colors))]
           [aspect-ratio (/ width height)]
           [stars-x
            (map (section * <> width 1/20) (chaos-coordinates 30 (+ n 0.1) 20))]
           [stars-y
            (map (section * <> height 1/20) (chaos-coordinates 30 n 20))]
           [stars-color
            (map (compose irgb-complement
                          (section image-get-pixel background <> <>))
                 stars-x
                 stars-y)]
           ;w/40 means stars are just touching
           [dragon (turtle-new background)]
           [dragon-x
            (* height 1/4 (car (chaos-coordinates 1 (+ n 0.1) 4)))]
           [dragon-y
            (* height 1/4 (car (chaos-coordinates 1 n 4)))]
           [dragon-pixel (image-get-pixel background dragon-x dragon-y)]
           [dragon-color
            (hsv->irgb (hsv (modulo (round (+ 90 (irgb->hue dragon-pixel))) 360) 
                       (irgb->saturation dragon-pixel)
                       (- 1 (irgb->value dragon-pixel))))]
           [twinkle 
            (lambda (col row color)
              (n-star background 10 col row (/ width 40) 0 aspect-ratio color)
              (n-star background 10 col row (* (/ width 40) 0.8) (/ 360 20) aspect-ratio (irgb-add (irgb 16 16 16) color)))])
      (for-each twinkle stars-x stars-y stars-color)
      (dragons-offset dragon 4 dragon-x dragon-y 0 (/ height 75) 0 90 0 aspect-ratio 6 dragon-color)
      background)))
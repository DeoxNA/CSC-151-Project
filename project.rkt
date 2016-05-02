#lang racket
(require gigls/unsafe)

(define canvas (image-show (image-new 1000 1000)))
(define dragon (turtle-new canvas))

;;; Procedure
;;;   Project-background
;;; Parameters
;;;   z, an integer
;;;   width, an integer
;;;   height, an integer
;;;   color1, an integer-encoded rgb color
;;;   color2, an integer-encoded rgb color
;;; Purpose
;;;   Creates a background image for our project, in the form of a color fade
;;; Produces
;;;   background, an image
;;; Preconditions
;;;   z = 1, 2, or 3
;;;   width > 0 and height > 0
;;; Postconditions
;;;   background is a an image width wide and height high
;;;   background is a color fade from color1 to color2
;;;   If z = 1, background will be a vertical fade from color1 (top) to color2
;;;   If z = 2, background fades from color1 (top right) to color2 (bottom left)
;;;   If z = 3, background fades from color1 (top left) to color2 (bottom right)
;;;   The color of an individual pixel varies directly with its row and column
;;;   in relation to width and height
(define project-background
  (lambda (z width height color1 color2)
    (let* ([first-red (irgb-red color1)]
           [first-green (irgb-green color1)]
           [first-blue (irgb-blue color1)]
           [second-red (irgb-red color2)]
           [second-green (irgb-green color2)]
           [second-blue (irgb-blue color2)])
      (image-compute (lambda (col row)
                       (let* ([1-pixel-change (/ row height)]
                              [2-pixel-change (/ (sqrt (+ (square col)
                                                          (square row)))
                                                 (sqrt (+ (square height)
                                                          (square width))))]
                              [3-pixel-change (/ (sqrt (+ (square (- width col))
                                                          (square row)))
                                                 (sqrt (+ (square height)
                                                          (square width))))])
                         (cond [(equal? z 1)
                                (irgb (+ first-red (* (- second-red first-red)
                                                      1-pixel-change))
                                      (+ first-green
                                         (* (- second-green first-green)
                                            1-pixel-change))
                                      (+ first-blue 
                                         (* (- second-blue first-blue)
                                            1-pixel-change)))]
                               [(equal? z 2)
                                (irgb (+ first-red (* (- second-red first-red)
                                                      2-pixel-change))
                                      (+ first-green)
                                      (* (- second-green first-green)
                                         2-pixel-change))
                                (+ first-blue (* (- second-blue first-blue)
                                                 2-pixel-change))]
                               [(equal? z 3)
                                (irgb (+ first-red (* (- second-red first-red)
                                                      3-pixel-change))
                                      (+ first-green 
                                         (* (- second-green first-green)
                                            3-pixel-change))
                                      (+ first-blue 
                                         (* (- second-blue first-blue)
                                            3-pixel-change)))])))
                     width height))))

;;; Procedure
;;;   Background-color-helper
;;; Parameters
;;;   a, an integer
;;; Purpose
;;;   Generates a list of colors for Project-background
;;; Produces
;;;   lst, a list
;;; Preconditions
;;;   0 <= a <= 4
;;; Postconditions
;;;   lst consists of 2 integer-encoded rgb colors depending on a.
;;;   The colors lst consists of are specifically coded into the procedure.
(define background-color-helper
  (lambda (n)
    (cond [(equal? n 0)
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
    (let* ([lengths (if (or (= angle 0) (= angle 180))
                              (cons (* length aspect-ratio) length)
                              (cons length (* length aspect-ratio)))]
           [len-start (car lengths)]
           [len-orthogonal (cdr lengths)]
           [move-dragon
            (lambda (step)
              (turtle-face! turtle (+ step angle))
              (if (or (= step 0) (= step 180))
                  (turtle-forward! turtle len-start)
                  (turtle-forward! turtle len-orthogonal)))])
      (turtle-set-color! turtle color)
      (turtle-set-brush! turtle "2. Hardness 100" 0.1)
      (turtle-teleport! turtle col row)
      (for-each move-dragon
                (generate-steps iterations)))))


(define dragon-spiral
  (lambda (turtle n col row radius step-length offset-angle aspect-ratio iterations color)
    (let* ([draw-dragon (section dragon-curve turtle <> <> step-length <> aspect-ratio iterations <>)]
           [alternate-color
            (lambda (n)
              (if (even? n)
                  color
                  (irgb-add (irgb 32 32 32) color)))]
           [degree-angles (map (r-s * 90)
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
           [colors (map alternate-color
                    (iota n))])
      (for-each draw-dragon cols rows offset-angles colors))))

;Sea weed? (dragons-offset dragon 3 500 500 200 15 240 30 0 6) 


;aspect-ratio is horizontal / vertical
(define n-star
  (lambda (image n col row radius angle aspect-ratio color)
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
      (context-set-brush! "2. Hardness 100" 0.1)
      (context-set-fgcolor! color)
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
            (* width 1/4 (car (chaos-coordinates 1 (+ n 0.1) 4)))]
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
      (dragon-spiral dragon 4 dragon-x dragon-y 0 (/ height 75) 0 aspect-ratio 6 dragon-color)
      background)))

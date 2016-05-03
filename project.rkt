#lang racket
(require gigls/unsafe)

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
                                      (+ first-green
                                      (* (- second-green first-green)
                                         2-pixel-change))
                                      (+ first-blue (* (- second-blue first-blue)
                                                       2-pixel-change)))]
                               [(equal? z 3)
                                (irgb (+ first-red (* (- second-red first-red)
                                                      3-pixel-change))
                                      (+ first-green 
                                         (* (- second-green first-green)
                                            3-pixel-change))
                                      (+ first-blue 
                                         (* (- second-blue first-blue)
                                            3-pixel-change)))]
                               [else 
                                (display z)
                                (error "z is outside the range 1-3")])))
                     width height))))

;;; Procedure
;;;   Background-color-helper
;;; Parameters
;;;  option, an integer
;;; Purpose
;;;   Generates a list of colors for Project-background
;;; Produces
;;;   lst, a list
;;; Preconditions
;;;   0 <= option <= 4 [Unverified] 
;;; Postconditions
;;;   lst consists of 2 integer-encoded rgb colors depending on option.
;;;   The colors lst consists of are specifically coded into the procedure.
(define background-color-helper
  (lambda (option)
    (cond [(equal? option 0)
           (list (irgb 0 0 0) (irgb 86 29 116))]
          [(equal? option 1)
           (list (irgb 234 37 17) (irgb 122 54 157))]
          [(equal? option 2)
           (list (irgb 171 216 248) (irgb 48 72 189))]
          [(equal? option 3)
           (list (irgb 49 189 140) (irgb 52 66 134))]
          [(equal? option 4)
           (list (irgb 255 165 0) (irgb 87 22 17))])))

;;; Procedure
;;;   generate-steps
;;; Parameters
;;;   n, a positive non-zero integer
;;; Purpose
;;;   Generates a list of steps for drawing the dragon curve.
;;; Produces
;;;   steps, a list
;;; Preconditions
;;;   [No Additional.]
;;; Postconditions
;;;   (length steps) = 2^n  
;;;   Steps contains only 0, 90, 180, 270.
;;;   Steps contain a list of instructions to 
;;;      draw a dragon curve of n iterations 
;;;      deep.
(define generate-steps
  (let* ([orthogonal (compose (r-s mod 360) (r-s + 90))]
         [next-steps
          (lambda (lst)
            (append lst (map orthogonal (reverse lst))))])
    (lambda (n)
      (let kernel ([steps (list 0)]
                   [i 0])
        (if (= i n)
            steps
            (kernel (next-steps steps) (+ i 1)))))))


;DON'T FORGET TO FILL IN PRODUCES***
;;; Procedure
;;;   dragon-curve
;;; Parameters
;;;   turtle, a turtle
;;;   col, a non-zero positive integer
;;;   row, a non-zero positive integer
;;;   length, a non-zero positive real number
;;;   angle, a real number
;;;   aspect-ratio, a non-zero positive real number
;;;   color, an integer-encoded RGB color
;;;   steps, a list
;;; Purpose
;;;   Draws a dragon fractal on an image
;;; Produces
;;;   drawn-curve, a turtle-drawn change to a preexisting
;;;     image
;;; Preconditions
;;;   0 <= angle <= 360
;;;   Steps was generated by the generate-steps procedure.
;;;   Aspect-ratio > 0 
;;; Postconditions
;;;   turtle will draw with brush "2. Hardness 100" with size proportional to 
;;;      aspect-ratio and color.
;;;   turtle will follow a path of each element of steps in order, specifically,
;;;      it will move forward by len-start or len-orthogonal, turn angle, and 
;;;      repeat for each element of each element of steps (a list of lists)
(define dragon-curve
  (lambda (turtle col row length angle aspect-ratio color steps)
    (let* ([lengths (if (or (= angle 0) (= angle 180))
                        (cons (* length aspect-ratio) length)
                        (cons length (* length aspect-ratio)))]
           [len-start (car lengths)]
           [len-orthogonal (cdr lengths)]
           [move-dragon
            (lambda (step)
              (turtle-face! turtle (+ step angle))
              (cond
                ;Turtle will move in original direction
                [(or (= step 0) (= step 180))
                 (turtle-set-brush! turtle "2. Hardness 100" (/ len-start 8))
                 (turtle-forward! turtle len-start)]
                ;Turtle will move in orthogonal direction
                [else
                 (turtle-set-brush! turtle "2. Hardness 100" (/ len-orthogonal 8))
                 (turtle-forward! turtle len-orthogonal)]))])
      (turtle-set-color! turtle color)
      (turtle-teleport! turtle col row)
      (for-each move-dragon steps))))

;DON'T FORGET TO FILL IN PRODUCES***
;;; Procedure*******
;;;   dragon-spiral
;;; Parameters
;;;   turtle, a turtle
;;;   n, a non-zero positive integer
;;;   col, a non-zero positive integer
;;;   row, a non-zero positive integer
;;;   radius, a positive real number
;;;   length, a non-zero positive real number
;;;   aspect-ratio, a non-zero positive real number
;;;   color, an integer-encoded RGB color
;;;   steps, a list
;;; Purpose
;;;   Draws n amount of dragon curves
;;; Produces
;;;   draw-spiral, a turtle-drawn to a preexisting
;;;     image
;;; Preconditions
;;;   1 <= n <= 4 
;;;   Steps was generated by the generate-steps procedure.
;;;   Aspect-ratio > 0 
;;; Postconditions
;;;   draws n dragon curves, each radius distance from point (col, row)
(define dragon-spiral
  (lambda (turtle n col row radius length aspect-ratio color steps)
    (let* ([draw-dragon (section dragon-curve turtle <> <> length <> 
                                 aspect-ratio <> steps)]
           [alternate-color
            (lambda (n)
              (if (even? n)
                  color
                  (irgb-add (irgb 32 32 32) color)))]
           [degree-angles (map (r-s * 90)
                               (iota n))]
           [rad-angles (map degrees->radians
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
      (for-each draw-dragon cols rows degree-angles colors))))

;;; Procedure
;;;   n-star
;;; Parameters
;;;   image, an image
;;;   n, a non-zero positive integer
;;;   col, a non-zero positive integer
;;;   row, a non-zero positive integer
;;;   radius, a non-zero positive real number
;;;   angle, a  real number
;;;   aspect-ratio, a non-zero positive real number
;;;   color, an integer-encoded RGB color
;;; Purpose
;;;   Draws stars with n amount of points
;;; Produces
;;;   star, a drawing
;;; Preconditions
;;;   0 <= angle <= 360
;;;   col >= 0
;;;   row >= 0
;;;   aspect-ratio > 0
;;;   aspect-ratio is horizontal / vertical
;;; Postconditions
;;;   draws stars with n evenly spaced points with length proportional to 
;;;      aspect-ratio
;;;   the points will be offset by a fraction of n and by angle
;;;   the stars will be drawn by the brush "2. Hardness 100" 0.1
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

;;; Procedure
;;;   chaos
;;; Parameters
;;;   r, an integer
;;;   x0, an integer
;;;   iterations, an integer
;;; Purpose 
;;;   creates an unpredictable but repeatable number
;;; Produces
;;;   chaotic-number, an integer
;;; Preconditions
;;;   r >= 0
;;;   iterations > 0
;;; Postconditions
;;;   chaotic-number is a very large integer, unpredictable from the initial 
;;;   parameters
(define chaos
  (lambda (r x0 iterations)
    (let kernel ([n 0]
                 [xn x0])
      (if (equal? n iterations)
          xn
          (kernel (+ 1 n) (* r xn (- 1 xn)))))))

;;; Procedure
;;;   chaos-coordinates
;;; Parameters
;;;   x, a positive integer
;;;   opt, a real number
;;;   divisions, a positive integer
;;; Purpose
;;;   Generates a list of positive integers between 1 and divisions
;;; Produces
;;;   lst, a list
;;; Preconditions
;;;    1 < divisions
;;; Postconditions
;;;   Each entry in lst will be a chaos-generated integer between 1 and 
;;;      divisions
(define chaos-coordinates
  (lambda (x opt divisions)
    (map (compose (r-s + 1)
                  (r-s mod (- divisions 1))
                  (section chaos 3.6 <> 6))
         (map (compose
               (section + 2 opt <>))
              (iota x)))))

;;; Procedure
;;;   image-series
;;; Parameters
;;;   n, a positive integer
;;;   width, a positive integer 
;;;   height, a positive integer
;;; Purpose
;;;   Creates a variety of interesting images of dragon fractals and stars
;;; Produces
;;;   image, an image
;;; Preconditions
;;;   [no additional]
;;; Postconditions
;;;   image will be a color-fade determined by project-background and n
;;;   image will have 60 stars generated by n-star, with 2 of different sizes
;;;      and numbers of points overlaid on top of each other at 30 positions
;;;   the stars will have positions generated by chaos-coordinates
;;;   the color of the stars will be the complement of the background at the 
;;;      center of the star
;;;   image will have 2 sets of 4 dragon fractals created by dragon-spiral, both  
;;;      centered around one of the stars
;;;   the color of the dragon-spiral will be computed from the color of the background
;;;      at the central pixel of the dragon-spiral
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
            (map (section * <> width 1/20) (chaos-coordinates 40 (+ n 0.1) 20))]
           [stars-y
            (map (section * <> height 1/20) (chaos-coordinates 40 n 20))]
           [stars-color
            (map (compose irgb-complement
                          (section image-get-pixel background <> <>))
                 stars-x
                 stars-y)]
           [dragon (turtle-new background)]
           [dragon-steps (generate-steps 7)]
           [dragon-x
            (* width 1/4 (car (chaos-coordinates 1 (+ n 0.1) 4)))]
           [dragon-y
            (* height 1/4 (car (chaos-coordinates 1 n 4)))]
           [dragon-pixel (image-get-pixel background dragon-x dragon-y)]
           [dragon-color
            (hsv->irgb (hsv (mod (round (+ 90 (irgb->hue dragon-pixel))) 360) 
                            (irgb->saturation dragon-pixel)
                            (- 1 (irgb->value dragon-pixel))))]
           [twinkle 
            (lambda (col row color)
              ;A radius of height/40 means adjacent stars are just touching (they are separated by height/20 pixels)
              (n-star background 10 col row (/ height 40) 0 aspect-ratio color)
              ;Second star has 80% the radius of the first, and its initial angle (/ 360 20) places the its spokes
              ;in between those of the first star, creating a twinkle effect.
              (n-star background 10 col row (* (/ height 40) 0.8) (/ 360 20) aspect-ratio (irgb-lighter color)))])
      (for-each twinkle stars-x stars-y stars-color)
      (dragon-spiral dragon 4 dragon-x dragon-y (* (/ height 16) (mod n 4)) (/ height 75) aspect-ratio dragon-color dragon-steps)
      (dragon-spiral dragon 4 dragon-x dragon-y (* (/ height 16) (floor (/ (mod n 16) 4))) (/ height 75) aspect-ratio dragon-color dragon-steps)
      background)))

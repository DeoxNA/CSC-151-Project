#lang racket
(require gigls/unsafe)

; Citation: Code for project-background was inspired by problem 5, exam 2 
;            written by John Lennon. 
;;; Procedure:
;;;   project-background
;;; Parameters:
;;;   z, an integer
;;;   width, an integer
;;;   height, an integer
;;;   color1, an integer-encoded rgb color
;;;   color2, an integer-encoded rgb color
;;; Purpose:
;;;   Creates a background image for our project, in the form of a color fade
;;; Produces:
;;;   background, an image
;;; Preconditions:
;;;   z = 1, 2, or 3
;;;   width > 0 and height > 0
;;; Postconditions:
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
      (image-compute 
       (lambda (col row)
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




;;; Procedure:
;;;   background-color-helper
;;; Parameters:
;;;  option, an integer
;;; Purpose:
;;;   Generates a list of colors for project-background
;;; Produces:
;;;   lst, a list
;;; Preconditions:
;;;   0 <= option <= 4 [Unverified] 
;;; Postconditions:
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




;;; Procedure:
;;;   generate-steps
;;; Parameters:
;;;   n, a positive non-zero integer
;;; Purpose:
;;;   Generates a list of steps for drawing the dragon curve.
;;; Produces:
;;;   steps, a list
;;; Preconditions:
;;;   [No Additional.]
;;; Postconditions:
;;;   (length steps) = 2^n  
;;;   Steps contains only 0, 90, 180, 270.
;;;   Steps contain a list of instructions to 
;;;      draw a dragon curve n iterations deep.
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




;;; Procedure:
;;;   dragon-curve
;;; Parameters:
;;;   turtle, a turtle
;;;   col, a non-zero positive integer
;;;   row, a non-zero positive integer
;;;   length, a non-zero positive real number
;;;   angle, a real number
;;;   aspect-ratio, a non-zero positive real number
;;;   color, an integer-encoded RGB color
;;;   steps, a list
;;; Purpose:
;;;   Draws a dragon fractal on an image, starting at
;;;    column col and row row, with an initial angle of
;;;    angle, and properly scaled to the aspect-ratio.
;;; Produces:
;;;   nothing, called for side-effects
;;; Preconditions:
;;;   angle = 90*i for i = 0,1,2,...
;;;   Steps was generated by the generate-steps procedure
;;; Postconditions:
;;;   turtle will draw in the specified color with brush "2. Hardness 100" 
;;;    the size of the turtle's brush depends on the aspect ratio and the
;;;    direction it is moving.
;;;   turtle will begin drawing from the specified column and row, facing the
;;;    specified angle
;;;   for each element i of steps:
;;;     turtle will face the angle specified by i
;;;     turtle will move forward by len-start if facing a direction parallel
;;;      to angle
;;;     turtle will move forward by len-orthogonal if facing a direction
;;;      orthogonal to angle
(define dragon-curve
  (lambda (turtle col row length angle aspect-ratio color steps)
    (let* ([lengths 
            ;Ensures the horizontal path length is scaled by
            ;aspect-ratio
            (if (or (= angle 0) (= angle 180))
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
                 (turtle-set-brush! turtle "2. Hardness 100" 
                                    (/ len-start 8))
                 (turtle-forward! turtle len-start)]
                ;Turtle will move in orthogonal direction
                [else
                 (turtle-set-brush! turtle "2. Hardness 100" 
                                    (/ len-orthogonal 8))
                 (turtle-forward! turtle len-orthogonal)]))])
      (turtle-set-color! turtle color)
      (turtle-teleport! turtle col row)
      (for-each move-dragon steps))))




;;; Procedure
;;;   dragon-spiral
;;; Parameters
;;;   turtle, a turtle
;;;   n, a non-zero positive integer
;;;   col, a non-zero positive integer
;;;   row, a non-zero positive integer
;;;   radius, a real number
;;;   length, a non-zero positive real number
;;;   aspect-ratio, a non-zero positive real number
;;;   color, an integer-encoded RGB color
;;;   steps, a list
;;; Purpose
;;;   Draws n amount of dragon curves
;;; Produces
;;;   nothing, called for side-effect
;;; Preconditions
;;;   1 <= n <= 4 
;;;   Steps was generated by the generate-steps procedure.
;;;   Aspect-ratio > 0
;;;   radius > 0
;;; Postconditions
;;;   draws n dragon curves, at 90 degrees to each other
;;;   Each dragon will be initialized at radius distance from point (col, row)
;;;   The color of odd-numbered dragons drawn will be color
;;;   The color of even-numbered dragons drawn will be (irgb 32 32 32) + color
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




;;; Procedure:
;;;   n-star
;;; Parameters:
;;;   image, an image
;;;   n, a positive integer
;;;   col, a positive integer
;;;   row, a positive integer
;;;   radius, a positive real number
;;;   angle, a real number
;;;   aspect-ratio, a positive real number
;;;   color, an integer-encoded RGB color
;;; Purpose:
;;;   Draws stars with n amount of star-spokes
;;; Produces:
;;;   nothing, called for side-effect
;;; Preconditions:
;;;   0 <= angle <= 360 
;;;   aspect-ratio = (/ (image-width image) (image-height image))
;;; Postconditions:
;;;   Draws stars with n evenly spaced star-spokes
;;;   The width of the stars is aspect-ratio times the height of
;;;    the stars
;;;   radius is half the height of the stars
;;;   The angle between adjacent points is (/ 360 n)
;;;   The stars will be drawn with the brush "2. Hardness 100" 0.1
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




;Citation: Bradley, Larry. 2010. Received idea for chaos procedure.
;          <http://www.stsci.edu/~lbradley/seminar/logdiffeqn.html>
;;; Procedure
;;;   chaos
;;; Parameters
;;;   r, a positive real number
;;;   x0, a real number
;;;   iterations, a postive integer
;;; Purpose 
;;;   creates an unpredictable but repeatable number
;;; Produces
;;;   chaotic-number, a real number
;;; Preconditions
;;;   [No additional]
;;; Postconditions
;;;   chaotic-number is a large number, unpredictable from 
;;;    a small change to the initial parameters
(define chaos
  (lambda (r x0 iterations)
    (let kernel ([n 0]
                 [xn x0])
      (if (equal? n iterations)
          xn
          (kernel (+ 1 n) (* r xn (- 1 xn)))))))




;;; Procedure:
;;;   chaos-coordinates
;;; Parameters:
;;;   n, a positive integer
;;;   opt, a real number
;;;   divisions, a positive integer
;;; Purpose:
;;;   Generates a list of n positive integers between 1 and (- divisions 1)
;;; Produces:
;;;   coordinates, a list
;;; Preconditions:
;;;    1 < divisions
;;; Postconditions:
;;;   Each entry in coordinates will be a  integer between 1 and divisions
;;;     generated by the chaos procedure
(define chaos-coordinates
  (lambda (n opt divisions)
    (map (compose (r-s + 1)
                  (r-s mod (- divisions 1))
                  (section chaos 3.6 <> 6))
         (map (compose
               (section + 2 opt <>))
              (iota n)))))




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
;;;   [No additional]
;;; Postconditions
;;;   image will be a color-fade determined by project-background and n
;;;   image will have 50 stars generated by n-star, with 4 different sizes
;;;      and numbers of points overlaid on top of each other at 30 positions
;;;   the stars will have positions generated by chaos-coordinates
;;;   the stars will vary in size with 4 sizes:
;;;     size1: 1/20 height
;;;     size2: 1/40 height
;;;     size3: 1/60 height
;;;     size4: 1/80 height
;;;   the color of the stars will be the complement of the background at the 
;;;      center of the star
;;;   image will have 2 sets of 4 dragon fractals created by dragon-spiral, both  
;;;      centered around one of the stars
;;;   the color of the dragon-spiral will be computed from the color of the 
;;;      background at the central pixel of the dragon-spiral
(define image-series
  (lambda (n width height)
    (let* ([background-colors 
            (background-color-helper (modulo n 5))]
           [background (project-background 
                        (ceiling (/ (+ 1 (modulo n 15)) 5))
                        width
                        height
                        (car background-colors)
                        (cadr background-colors))]
           [aspect-ratio (/ width height)]
           ;Adding 0.1 to the input of chaos-coordinates makes stars-x
           ; significantly different from stars-y
           [stars-x
            (map (section * <> width 1/20) 
                 (chaos-coordinates 50 (+ n 0.1) 20))]
           [stars-y
            (map (section * <> height 1/20) 
                 (chaos-coordinates 50 n 20))]
           [stars-color
            (map (compose irgb-complement
                          (section image-get-pixel background <> <>))
                 stars-x
                 stars-y)]
           ;Creates a repeating list of height/20, height/40, height/60,
           ; height/80
           [stars-radii (map (compose
                              (r-s * height)
                              (l-s / 1)
                              (r-s + 20)
                              (r-s * 20)
                              (r-s mod 4)) 
                             (iota 50))]
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
            (lambda (col row radius color)
              ;A radius of height/40 means adjacent stars are just touching 
              ; (they are separated by height/20 pixels)
              (n-star background 10 col row radius 0 aspect-ratio color)
              ;Second star has 80% the radius of the first, and its initial
              ; angle of (/ 360 20) = 18 places the its spokes in between those  
              ; of the first star, creating a twinkle effect.
              (n-star background 10 col row (* radius 0.8) 18 
                      aspect-ratio (irgb-lighter color)))])
      (for-each twinkle stars-x stars-y stars-radii stars-color)
      (dragon-spiral dragon 4 dragon-x dragon-y
                     (* (/ width 16) (mod n 4)) 
                     (/ height 75) aspect-ratio dragon-color dragon-steps)
      (dragon-spiral dragon 4 dragon-x dragon-y 
                     (* (/ width 16) (floor (/ (mod n 16) 4))) 
                     (/ height 75) aspect-ratio dragon-color dragon-steps)
      background)))



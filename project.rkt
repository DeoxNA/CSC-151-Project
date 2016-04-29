#lang racket
(require gigls/unsafe)

(define canvas (image-show (image-new 1000 1000)))
(define dragon (turtle-new canvas))

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
  (lambda (turtle col row length angle iterations)
    (let ([move-dragon
           (lambda (step)
             (turtle-face! turtle (+ step angle))
             (turtle-forward! turtle length))])
      (turtle-teleport! turtle col row)
      (for-each move-dragon
                (generate-steps iterations)))))

;aspect-ratio is horizontal / vertical
(define n-star
  (lambda (image n col row radius angle aspect-ratio)
    (let* ([rad-angle (degrees->radians angle)]
           [interior-angle (/ (* 2 pi) n)]
           [angles (map (compose
                         (r-s + rad-angle)
                         (r-s * interior-angle))
                        (iota n))]
           [draw-spokes
            (lambda (theta)
              (image-draw-line! image col row
                                (+ col (* radius (cos theta) aspect-ratio))
                                (+ row (* radius (sin theta)))))])
      (for-each draw-spokes angles))))

(define dragons-offset
  (lambda (turtle n col row radius step-length start-angle step-angle offset-angle iterations)
    (let* ([draw-dragon (section dragon-curve turtle <> <> step-length <> iterations)]
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
                      rad-angles)])
      (for-each draw-dragon cols rows offset-angles))))


; Sea weed? (dragons-offset dragon 3 500 500 200 15 240 30 0 6)
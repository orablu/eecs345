(   (var x)
    (= x 10)
    (var y (* (* 3 x) 5))
    (if (> x y)
        (return x)
        (if (> (* x x) y)
            (return (* x x))
            (if (> (* x (+ x x)) y)
                (return (* x (+ x x)))
                (return (- y 1))
            )
        )
)   )

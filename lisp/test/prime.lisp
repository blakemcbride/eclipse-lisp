(defun prime-count (s e)
  (declare ;(optimize (speed 3) (safety 0))
           (integer s e))
  (macrolet ((search-template (typeint)
               `(loop for n ,typeint from (if (oddp x) x (1+ x)) to y by 2
                    for stop ,typeint = (isqrt n)
                    with count ,typeint = (if (<= x 2 y) 1 0) ; to count 2
                    when (= n (loop for test-div ,typeint from 3 to stop by 2
                                  when (zerop (mod n test-div)) do (return test-div)
                                  finally (return n)))
                    do (incf count)
                    finally (print count))))
    (flet ((search-fixnum-range (x y) (search-template fixnum))
           (search-bignum-range (x y) (search-template integer)))
      (if (> e most-positive-fixnum)
          (search-bignum-range s e)
        (search-fixnum-range s e)))))

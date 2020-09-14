(seed (in "/dev/urandom" (rd 8)))
(load "functions.l")

# from reference
(test 1 (sprp 480194653 2139))
(test 0 (sprp 20074069 32374))
(test 1 (sprp 8718775377449 2))
(test 1 (sprp 8718775377449 687))
(test 1 (sprp 8651776913431 2))
(test 0 (sprp 8651776913431 767))
(test 1 (sprp 3315293452192821991 2))
(test 1 (sprp 3315293452192821991 8638))
(test 1 (sprp 3315293452192821991 135))
(test 1 (sprp 1152965996591997761 2))
(test 1 (sprp 1152965996591997761 1941))
(test 0 (sprp 1152965996591997761 15))

(test 0 (berg? 1152965996591997761))
(test 1 (berg? 3315293452192821991))
(test 0 (berg? 8651776913431))
(test 1 (berg? 8718775377449))
(prinl "reference tests - OK")

#{
(prinl "generate random N-digit primes and test:")
(for N (range 3 19)
   (do 128
      (test 1 (berg? (prime N))) )
      (prinl N "-digits primes - OK") )
}#

#{
(in "test.dat"
   (until (eof)
      (let N (format (line T))
         (println 'N N (berg? N)) ) ) )
}#

(in "psps-below-2-to-64.txt"
   (until (eof)
      (let N (format (line T))
         (when (=1 (berg? N))
            (println 'N N) )
      )
   )
)


(msg 'ok)
(bye)

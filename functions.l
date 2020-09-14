(load "tables.l")
(setq
   *U32 `(hex "FFFFFFFF")
   *U49 `(hex "1FFFFFFFFFFFF")
   *U64 `(hex "FFFFFFFFFFFFFFFF") )
# prime functions from @misc/rsa.l from PicoLisp
(de longRand (N)
   (use (R D)
      (while (=0 (setq R (abs (rand)))))
      (until (> R N)
         (unless (=0 (setq D (abs (rand))))
            (setq R (* R D)) ) )
      (% R N) ) )
# Probabilistic prime check
(de prime? (N)
   (and
      (> N 1)
      (bit? 1 N)
      (let (Q (dec N)  K 0)
         (until (bit? 1 Q)
            (setq
               Q  (>> 1 Q)
               K  (inc K) ) )
         (do 50
            (NIL (_prim? N Q K))
            T ) ) ) )
# (Knuth Vol.2, p.379)
(de _prim? (N Q K)
   (use (X J Y)
      (while (> 2 (setq X (longRand N))))
      (setq
         J 0
         Y (**Mod X Q N) )
      (loop
         (T
            (or
               (and (=0 J) (= 1 Y))
               (= Y (dec N)) )
            T )
         (T
            (or
               (and (> J 0) (= 1 Y))
               (<= K (inc 'J)) )
            NIL )
         (setq Y (% (* Y Y) N)) ) ) )
# Find a prime number with `Len' digits
(de prime (Len)
   (let P (longRand (** 10 (*/ Len 2 3)))
      (unless (bit? 1 P)
         (inc 'P) )
      (until (prime? P)  # P: Prime number of size 2/3 Len
         (inc 'P 2) )
      # R: Random number of size 1/3 Len
      (let (R (longRand (** 10 (/ Len 3)))  K (+ R (% (- P R) 3)))
         (when (bit? 1 K)
            (inc 'K 3) )
         (until (prime? (setq R (inc (* K P))))
            (inc 'K 6) )
         R ) ) )
# ^^^^^^^
(de mod32 (N)
   (& N *U32) )
(de mod32* (A B)
   (mod32 (* A B)) )
(de **Mod (X Y N)
   (let M 1
      (loop
         (when (bit? 1 Y)
            (setq M (% (* M X) N)) )
         (T (=0 (setq Y (>> 1 Y)))
            M )
         (setq X (% (* X X) N)) ) ) )
(de sprp (N B)
   (let (N1 (dec N)  D (>> 1 N1)  Z 0  R)
      (until (bit? 1 D)
         (setq D (>> 1 D))
         (inc 'Z) )
      (setq R (**Mod B D N))
      (cond
         ((or (=1 R) (= R N1)) 1)
         ((> R 1)
            (if (=0 Z)
               @
               (do Z
                  (setq R (% (* R R) N))
                  (T (= R N1) 1)
                  0 ) ) )
         ((>= B N) 1)
         (T 0) ) ) )
# XXX
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


(de check (N)
   (let
      (S
         (cond
            ((> *U32 N) 0)
            ((> *U49 N) 1)
            (T 2) )
         B
         (get
            (if (gt0 S ) *B64 *B32)
            (inc (>> (if (gt0 S) 18 24) (mod32* `(hex "AD625B89") N))) )
      )
      (println 'S S 'B B)
   )
)

(msg 'ok)
(bye)

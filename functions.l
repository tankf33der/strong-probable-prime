(load "tables.l")
(setq
   *U32 `(hex "FFFFFFFF")
   *U49 `(hex "1FFFFFFFFFFFF")
   *U64 `(hex "FFFFFFFFFFFFFFFF") )
# generation prime from @misc/rsa.l from PicoLisp
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
(de positive (N)
   (- 64 (length (bin N))) )
(de sprp (N B)
   (msg 'next)
   (let (D1 (>> 1 (dec N))  D D1  Z 0  R 0)
      (until (bit? 1 D1)
         (setq D1 (>> 1 D1))
         (inc 'Z) )
      (println 'D D 'D1 D1 'Z Z)
      (println 'N N 'N-1 (dec N) (* D (** 2 Z)))

      (println 'mod B D N)
      (setq R (**Mod B D N))
      (println 'R R)

      (do Z
         (setq R (% (* R R) N))
         (println 'inside-Z 'R R)
         (when (= R (dec N))
            (println 'found 'R R 'decN (dec N))
         )
      )
      (println 'after-Z-loop 'B B 'N N)
   )
)
#(sprp 480194653 2139)
#(sprp 20074069 32374)
#(sprp 92829781 3350)
#(sprp 8718775377449 2)
#(sprp 8651776913431 2)
#(sprp 8651776913431 767)
#(sprp 19473535927 4351)
#(sprp 19 1627)

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
(pp 'check)
(println (check 480194653))
(println (check 8718775377449))
(println (check 8651776913431))
(println (check 1152965996591997761))




(msg 'ok)
(bye)

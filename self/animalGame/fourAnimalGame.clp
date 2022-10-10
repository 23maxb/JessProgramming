(clear)
(reset)

(defglobal ?*finished* =FALSE)

(deftemplate attribute (slot name) (slot value))

(defrule cow
   "selects for a cow"
   (test (= ?*finished* FALSE))
   (attribute (name "domesticated") (value TRUE))
   (attribute (name "hooves") (value TRUE))
   (attribute (name "horns") (value TRUE))
   =>
   (verify "cow")
)
(defrule sheep
   "selects for a sheep"
   (test (= ?*finished* FALSE))
   (attribute (name "wool") (value TRUE))
   =>
   (verify "sheep")
)
(defrule chicken
   "selects for a chicken"
   (test (= ?*finished* FALSE))
   (attribute (name "domesticated") (value TRUE))
   (attribute (name "hooves") (value TRUE))
   (attribute (name "wings") (value TRUE))
   =>
   (verify "chicken")
)

(defrule beaver
   "selects for a beaver"
   (test (= ?*finished* FALSE))
   (attribute (name "aquatic") (value TRUE))
   (attribute (name "damns") (value TRUE))
   =>
   (verify "beaver")
)

(defrule getSnout
   "gets whether the animal has a snout"
   (test (= ?*finished* FALSE))
   =>
   (printout t "Does your animal have a snout?")
   (assert (attribute (name "snout") (value (getbool))))
)

(defrule getSnout
   "gets whether the animal has a snout"
   (test (= ?*finished* FALSE))
   =>
   (printout t "Does your animal have a snout?")
   (assert (attribute (name "snout") (value (getbool))))
)

(deffunction vowelp (?letter)
   (return (or (= ?letter "a") (= ?letter "e") (= ?letter "i") (= ?letter "o") (= ?letter "u")))
)

(deffunction getbool ()
   (return (= (sub-string 1 1 (sym-cat (read))) "y"))
)

(deffunction verify (?name)
   (printout t "Is your animal a")
   (if (vowelp (sub-string 1 1 ?name)) then
      (printout t "n")
   )
   (printout t " " ?name "? ")
   (bind ?*finished* TRUE)
   (if (getbool) then
      (printout t "I win!" crlf)
    else
      (printout t "I lose." crlf)
   )
)

(run)
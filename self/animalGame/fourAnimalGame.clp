/**
* Plays an animal game with the user.
* Requires Dr. Nelson's utilities_v4.clp to be batched in before.
*
* @author Max Blennemann
* @version 10/10/22
*/

(clear)
(reset)
(batch "executable\\utilities_v4.clp")

;Checks whether the game is finished or not
(defglobal ?*finished* = FALSE)

;Defines the template for any information about the animal
(deftemplate attribute (slot name) (slot value))
(do-backward-chaining attribute)

(defrule cow
   "selects for a cow"
   (test (= ?*finished* FALSE))
   (attribute (name "domesticated") (value TRUE))
   (attribute (name "wool") (value FALSE))
   =>
   (verify "cow")
)

(defrule end "Runs if the program cant guess the animal."
      (declare (salience -1))
   =>
      (printline "I don't know. I lose.")
)

(defrule startingPrompt
   "prompts the user"
   (declare (salience 99))
   =>
   (printline "Answer each question with yes or no.")
)

(defrule sheep
   "selects for a sheep"
   (test (= ?*finished* FALSE))
   (attribute (name "domesticated") (value TRUE))
   (attribute (name "wool") (value TRUE))
   =>
   (verify "sheep")
)

(defrule chicken
   "selects for a chicken"
   (test (= ?*finished* FALSE))
   (attribute (name "domesticated") (value TRUE))
   (attribute (name "wings") (value TRUE))
   =>
   (verify "chicken")
)

(defrule beaver
   "selects for a beaver"
   (test (= ?*finished* FALSE))
   (attribute (name "dams") (value TRUE))
   =>
   (verify "beaver")
)

(defrule getAquatic
   "gets whether the animal is aquatic"
   (test (= ?*finished* FALSE))
   (need-attribute (name "aquatic"))
   (not (attribute (name "aquatic")))
   =>
   (printout t "Is your animal aquatic?")
   (bind ?a (getbool))
   (assert (attribute (name "dams") (value ?a)))
   (if ?a then
      (assert (attribute (name "wings") (value (not ?a))))
      (assert (attribute (name "wool") (value (not ?a))))
   )
)

(defrule getWool
   "gets whether the animal has wool"
   (test (= ?*finished* FALSE))
   (not (attribute (name "wool")))
   =>
   (printout t "Does your animal have wool?")
   (bind ?a (getbool))
   (assert (attribute (name "wool") (value ?a)))
   (if ?a then
      (assert (attribute (name "aquatic") (value (not ?a))))
      (assert (attribute (name "wings") (value (not ?a))))
      (assert (attribute (name "dams") (value (not ?a))))
   )
)

(defrule getWings
   "gets whether the animal has wings" 
   (test (= ?*finished* FALSE))
   (not (attribute (name "wings")))
   =>
   (printout t "Does your animal have wings?")
   (bind ?a (getbool))
   (assert (attribute (name "wings") (value ?a)))
   (if ?a then
      (assert (attribute (name "dams") (value (not ?a))))
      (assert (attribute (name "wings") (value (not ?a))))
   )
)

(defrule getDams
   "gets whether the animal builds dams"
   (test (= ?*finished* FALSE))
   (not (attribute (name "dams")))
   =>
   (printout t "Does your animal build dams?")
   (bind ?a (getbool))
   (assert (attribute (name "dams") (value ?a)))
   (if ?a then
      (assert (attribute (name "wings") (value (not ?a))))
      (assert (attribute (name "wool") (value (not ?a))))
   )
)

(defrule getDomesticated
   "gets whether the animal is domesticated"
   (declare (salience 90))
   (test (= ?*finished* FALSE))
   (not (attribute (name "domesticated")))
   =>
   (printout t "Is your animal domesticated?")
   (bind ?a (getbool))
   (assert (attribute (name "domesticated") (value ?a)))
   (if ?a then
      (assert (attribute (name "dams") (value (not ?a))))
   )
)

/**
** Gets user input by checking whether the first letter of what the user put was y.
** Ignores case.
** 
** @return true if the given string starts with y or Y
** otherwise false.
*/
(deffunction getbool ()
   (return (= (lowcase (sub-string 1 1 (sym-cat (read)))) "y"))
)

/**
** Only called by rules. Checks whether the animal ?name is the animal the program is trying to guess. 
** Halts the program whether the guess was right or wrong.
** 
** @param ?name the name of the animal to check
*/
(deffunction verify (?name)
   (printout t (str-cat "Is your animal a(n) " ?name "? "))
   (bind ?*finished* TRUE)
   (if (getbool) then
      (printout t "I win!" crlf)
    else
      (printout t "I lose." crlf)
   )
   (halt)
)

(run)
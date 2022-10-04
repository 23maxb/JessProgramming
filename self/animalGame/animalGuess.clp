/**
* Prints out anagrams of any length less than 11. 
* Requires Dr. Nelson's utilities_v4.clp to be batched in before.
*
* @author Max Blennemann
* @version 9/28/22
*/

(deftemplate attribute (slot question) (slot value))
;value is integer
;-1 is not set
; 0 is no
; 1 is yes
; 2 is unknown

(defglobal ?*PossibleQuestions* = (create$ 
   "1. Is your animal eaten by humans?"
   "2. Is your animal domesticated?"
   "3. Does your animal have fur?"
   "4. Does your animal have horns?"
   "5. Does your animal have wool?"
   "6. Does your animal have hooves?"
   "7. Is your animal a rodent?"
   "8. Does your animal eat meat?"
   "9. Is your animal aquatic?"
   "10. Can your animal fly? (chained to next question)"
   "11. Does your animal have wings?"
   "12. Is your animal a type of fish?"
   "13. Is your animal a cephalopod?"
   "14. Is your animal a type of lizard?"
   "15. Is your animal a type of crustacean?"
   "16. Does your animal have bones?"
   "17. Does your animal build structures?"
   "18. Does your animal hvae a tail?"
   "19. Does your animal have legs?"
   "20. Does your animal have fins?"
   "21. Does your animal have a bill?"
))

(deffunction requestInfo (?question)
   (bind ?response (ask ?question))
   (bind ?val 2)
   (if (= (asc (sub-string 1 1 ?response)) (asc "y")) then
      (bind ?val 1)
      else 
      (if (= (asc (sub-string 1 1 ?response)) (asc "n")) then
         (bind ?val 0)
      )
   )
   (printline ?val)
   (assert (attribute (question ?question) (value ?val)))
)

(deffunction gameOver (?win)
   (if (= (asc (sub-string 1 1 ?win)) (asc "y")) then
      (bind ?val TRUE)
    else 
      (if (= (asc (sub-string 1 1 ?win)) (asc "n")) then
         (bind ?val FALSE)
      )
   )
   (if ?win then
      (printline "I won! :)")
    else
      (printline "I lost. :(")
   )
   (clear)
   (reset)
)

(deffunction getAnimalData (?fileName)
   (open ?fileName r)
   (printout t)
  ; (createAnimal (readline))
)

(deffunction createAnimal (?name ?data*)
   (bind ?toRule (str-cat "(defrule " ?name " \"The rule that checks to see if the animal is a " ?name "\" "))
   ;(bind ?toRule (attribute (question) (value)))
   
   
   (printline (str-cat ?toRule "=>(gameOver (ask \"Is your animal a " ?name "?\")))"))

   )

/*
(defrule cow
   (attribute (question ?q) (value ?v))
   
   =>
   (gameOver (ask "Is your animal a cow"))
)*/


(getAnimalData "animalListAndAttributes.csv")
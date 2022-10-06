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

/**
** Checks if two strings are equal.
** 
** @param ?str1 
*/
(deffunction str-eq (?str1 ?str2)
   (return (= (str-compare (str-cat ?str1) (str-cat ?str2)) 0))
)

/**
** Splits a string by the ?splitter
** 
** written by andrew fox and modified by me
*/
(deffunction split$ (?str ?splitter)
   (bind ?list (create$))
   (bind ?currentString "")
   (for (bind ?i 1) (<= ?i (str-length ?str)) (++ ?i)
      (bind ?addToEnd (sub-string ?i ?i ?str))
      (if (str-eq ?addToEnd ?splitter) then
         (bind ?list (insert$ ?list (+ (length$ ?list) 1) ?currentString))
         (bind ?currentString "")
      else
         (bind ?currentString (str-cat ?currentString ?addToEnd))
      )
   )
   (return (insert$ ?list (+ (length$ ?list) 1) ?currentString))
)

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
   (bind ?animalDataLoading TRUE)
   (while ?animalDataLoading
      (try
         (createAnimal (split$ (readline r) ","))

      catch
         ;ignore eof reached
         (printline "Animal Data loaded as rules.")
         (bind ?animalDataLoading FALSE)
      )
   ) 
)

(deffunction createAnimal (?name $data)
   (bind ?toRule (str-cat "(defrule " ?name " \"The rule that checks to see if the animal is a " ?name "\" "))
   ;(bind ?toRule (attribute (question) (value)))
   
   (for (bind ?i 2) (<= ?i (str-length ?str)) (++ ?i)
      (bind ?toRule (str-cat "(attribute (question ?q" ?i " & \"" (nth$ (- ?i 1) 1 ?*PossibleQuestions*) "\") (value ?v" ?i " & " (nth$ ?i $data)))
   )
   
   (printline (str-cat ?toRule "=>(gameOver (ask \"Is your animal a " ?name "?\")))"))

   )

/*
(defrule cow
   (attribute (question ?q1 & "questionhere1") (value ?v1 & 1))
   (attribute (question ?q2 & "questionhere2") (value ?v2 & 1))
   (attribute (question ?q3 & "questionhere3") (value ?v3 & 1))
   (attribute (question ?q4 & "questionhere4") (value ?v4 & 1))
   (attribute (question ?q5 & "questionhere5") (value ?v5 & 1))
   (attribute (question ?q6 & "questionhere6") (value ?v6 & 1))
   
   =>
   (gameOver (ask "Is your animal a cow"))
)*/


(getAnimalData "animalListAndAttributes.csv")
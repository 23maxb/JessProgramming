/**
* Plays an animal game with the user.
* Requires Dr. Nelson's utilities_v4.clp to be batched in before.
*
* @author Max Blennemann
* @version 10/6/22
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
   "10. Can your animal fly?"
   "11. Does your animal have wings?"
   "12. Is your animal a type of fish?"
   "13. Is your animal a cephalopod?"
   "14. Is your animal a type of lizard?"
   "15. Is your animal a type of crustacean?"
   "16. Does your animal have bones?"
   "17. Does your animal build structures?"
   "18. Does your animal have a tail?"
   "19. Does your animal have legs?"
   "20. Does your animal have fins?"
   "21. Does your animal have a bill?"
)) ; (defglobal ?*PossibleQuestions* = (create$ 

(defglobal ?*QuestionNum* = 1)

/**
** Checks if two strings are equal.
** Will also convert non string things to strings to compare.
** Technically could be used for other data types for equality like numbers or lists.
** 
** @param ?str1 the first String to compare
** @param ?str2 the second String to compare 
*/
(deffunction str-eq (?str1 ?str2)
   (return (= (str-compare (str-cat ?str1) (str-cat ?str2)) 0))
)

/**
** Splits a string by the ?splitter character or phrase. 
** 
** @param ?str the string to split
** @param ?splitter the splitting character.
** @return the created list
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
) ; (deffunction split$ (?str ?splitter)

/**
** Concludes the game. 
** 
** @param ?win whether or not the computer has successfully determined the animal.
*/
(deffunction gameOver (?win)
   (bind ?win (lowcase ?win))
   (if (= (asc (sub-string 1 1 ?win)) (asc "y")) then
      (bind ?val TRUE)
    else 
      (if (= (asc (sub-string 1 1 ?win)) (asc "n")) then
         (bind ?val FALSE)
      )
   )
   (if ?val then
      (printline "I won! :)")
    else
      (printline "I lost. :(")
   )
   (halt)
); (deffunction gameOver (?win)

/**
** Gets the animal data from a csv file based on the ?fileName
** 
** @param ?fileName the file name to use
*/
(deffunction getAnimalData (?fileName)
   (open ?fileName r)
   (bind ?animalDataLoading TRUE)
   (for (bind ?i 1) (<= ?i 4) (++ ?i) ; only loads in the first 4 animals
      ;(try
         (createAnimal (split$ (readline r) ","))
/*
      catch
         ;ignore eof reached
         (printline "Animal Data loaded as rules.")
         (bind ?animalDataLoading FALSE)
      )*/
   )
) ; (deffunction getAnimalData (?fileName)

/**
** Creates an animal rule based on data in the form of a list that is passed as a parameter.
** 
** Example Rule for the data for cow:
** 
(defrule Cow "The rule that checks to see if the animal is a Cow"
   (attribute (question ?q1 & "1. Is your animal eaten by humans?") (value ?v1 & 1))
   (attribute (question ?q2 & "2. Is your animal domesticated?") (value ?v2 & 1))
   (attribute (question ?q3 & "3. Does your animal have fur?") (value ?v3 & 0))
   (attribute (question ?q4 & "4. Does your animal have horns?") (value ?v4 & 1))
   (attribute (question ?q5 & "5. Does your animal have wool?") (value ?v5 & 0))
   (attribute (question ?q6 & "6. Does your animal have hooves?") (value ?v6 & 1))
   (attribute (question ?q7 & "7. Is your animal a rodent?") (value ?v7 & 0))
   (attribute (question ?q8 & "8. Does your animal eat meat?") (value ?v8 & 0))
   (attribute (question ?q9 & "9. Is your animal aquatic?") (value ?v9 & 0))
   (attribute (question ?q10 & "10. Can your animal fly? (chained to next question)") (value ?v10 & 0))
   (attribute (question ?q11 & "11. Does your animal have wings?") (value ?v11 & 0))
   (attribute (question ?q12 & "12. Is your animal a type of fish?") (value ?v12 & 0))
   (attribute (question ?q13 & "13. Is your animal a cephalopod?") (value ?v13 & 0))
   (attribute (question ?q14 & "14. Is your animal a type of lizard?") (value ?v14 & 0))
   (attribute (question ?q15 & "15. Is your animal a type of crustacean?") (value ?v15 & 0))
   (attribute (question ?q16 & "16. Does your animal have bones?") (value ?v16 & 1))
   (attribute (question ?q17 & "17. Does your animal build structures?") (value ?v17 & 0))
   (attribute (question ?q18 & "18. Does your animal have a tail?") (value ?v18 & 1))
   (attribute (question ?q19 & "19. Does your animal have legs?") (value ?v19 & 1))
   (attribute (question ?q20 & "20. Does your animal have fins?") (value ?v20 & 0))
   (attribute (question ?q21 & "21. Does your animal have a bill?") (value ?v21 & 0))
   =>
   (gameOver (ask "Is your animal a Cow?")
)
) ; (defrule Cow "The rule that checks to see if the animal is a Cow")
** 
** @param ?data a list with the data 
*/
(deffunction createAnimal (?data)
   (bind ?toRule (str-cat "(defrule " (nth$ 1 ?data) " \"The rule that checks to see if the animal is a " (nth$ 1 ?data) "\" 
   "))
   (for (bind ?i 2) (<= ?i (length$ ?data)) (++ ?i)
      (bind ?toRule (str-cat ?toRule "(attribute (question ?q" (- ?i 1) " & \"" 
      (nth$ (- ?i 1) ?*PossibleQuestions*) "\") (value ?v" (- ?i 1) " & " (nth$ ?i ?data) "))
      ")
      )
   )
   (printline (str-cat "Building the rule for a " (nth$ 1 ?data) "."))
   (build (str-cat ?toRule "=>(gameOver (ask \"Is your animal a " (nth$ 1 ?data) "?\")))"))

) ; (deffunction createAnimal (?data)


(defrule main "The starting point for the game."
   (declare (salience 1))

   =>

   (getAnimalData "animalListAndAttributes.csv")
   (assert (attribute (question "start")))
)
   
(defrule end "The ending point of the game."
      (declare (salience -1))
   =>
      (gameOver "n")
)

(reset)
(run)
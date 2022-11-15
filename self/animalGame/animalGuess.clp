/**
* Plays an animal game with the user.
* Requires Dr. Nelson's utilities_v4.clp to be batched in before. (batch "executable\\utilities_v4.clp")
* To run this file within the jess terminal run (batch "executable\\self\\animalGame\\animalGuess.clp")
*
* @author Max Blennemann
* @version 11/11/22
*/

(deftemplate attribute (slot question) (slot value))
(do-backward-chaining attribute)
;value is integer
;-1 is not set (not used)
; 0 is no
; 1 is yes
; 2 is unknown (not used)

(defglobal ?*PossibleQuestions* = (create$ 
   "1. Is your animal eaten by humans?"
   "2. Is your animal domesticated or kept as a pet often?"
   "3. Does your animal have fur?"
   "4. Does your animal have horns?"
   "5. Does your animal have wool?"
   "6. Does your animal have hooves?"
   "7. Does any species of your animal have more than one color?"
   "8. Does your animal eat meat?"
   "9. Is your animal aquatic?"
   "10. Can your animal fly?"
   "11. Does your animal have wings?"
   "12. Is your animal a type of fish?"
   "13. Is your animal a decapod?"
   "14. Is your animal a type of reptile?"
   "15. Does your animal lay eggs?"
   "16. Does your animal have bones?"
   "17. Does your animal build structures?"
   "18. Does your animal have a tail?"
   "19. Does your animal have legs?"
   "20. Does your animal have fins?"
   "21. Does your animal have a bill or beak?"
   "22. Does it eat fish?"
   "23. Does it spend any time or hunt in the ocean?"
   "24. Is it a feline?"
)) ; (defglobal ?*PossibleQuestions* = (create$ 

;use (split$ ?inputRead ",") to access the data as a 2d array
(defglobal ?*AnimalData* = 
   (create$)
)

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
** Asks the user a question and records the answer in the fact base.
** 
** @param ?question the question to ask
*/
(deffunction requestInfo (?questionNumber)
   (retract-string (str-cat "(need-attribute (question \"" (nth$ ?questionNumber ?*PossibleQuestions*) "\"))"))
   (bind ?response (ask (nth$ ?questionNumber ?*PossibleQuestions*)))
   (bind ?val 2)
   (if (= (asc (sub-string 1 1 ?response)) (asc "y")) then
      (bind ?val 1)
      else 
      (if (= (asc (sub-string 1 1 ?response)) (asc "n")) then
         (bind ?val 0)
      )
   )
   (if (= ?val 2) then
      (eval (str-cat "(undefrule askQuestionNumber" ?questionNumber ")"))
   else
      (for (bind ?i 1) (<= ?i (length$ ?*AnimalData*)) (++ ?i)
         (bind ?thisAnimalData (split$ (nth$ ?i ?*AnimalData*) ","))
         (if (not (str-eq ?val (nth$ (+ ?questionNumber 1) ?thisAnimalData))) then
            (undefrule (nth$ 1 ?thisAnimalData))
            (eval (str-cat "(retract-string \"(length" (length$ ?*AnimalData*) ")\")"))
            ;(printline (str-cat "Based on your answers I have eliminated " (nth$ 1 (split$ (nth$ ?i ?*AnimalData*) ",")) "."))
            (bind ?*AnimalData* (delete$ ?*AnimalData* ?i ?i))
            (eval (str-cat "(assert (length" (length$ ?*AnimalData*) "))"))
            (-- ?i)
         )
      )
      (assert (attribute (question (nth$ ?questionNumber ?*PossibleQuestions*)) (value ?val)))
   )
   (return (= ?val 1))
); (deffunction requestInfo (?question)

/**
** Concludes the game. 
** 
** @param ?win whether or not the computer has successfully determined the animal.
*/
(deffunction gameOver (?win)
   (bind ?win (lowcase ?win))
   (bind ?val nil)
   (if (= (asc (sub-string 1 1 ?win)) (asc "y")) then
      (bind ?val TRUE)
    else 
      (if (= (asc (sub-string 1 1 ?win)) (asc "n")) then
         (bind ?val FALSE)
      )
   )
   (if (= ?val nil) then
      (printline "Bruh")
   else
      (if ?val then
         (printline "I won! :)")
      else
         (printline "I lost. :(")
      )
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
   (while ?animalDataLoading
      (bind ?inputRead (readline r)) 
      (if (not (str-eq ?inputRead "EOF")) then
         (bind ?*AnimalData* (insert$ ?*AnimalData* (+ 1 (length$ ?*AnimalData*)) ?inputRead))
         (createAnimal (split$ ?inputRead ","))
      else
         (printline "Animal Data loaded as rules.")
         (eval (str-cat "(assert (length" (length$ ?*AnimalData*) "))"))
         (bind ?animalDataLoading FALSE)
      )
   )
) ; (deffunction getAnimalData (?fileName)

/**
** Creates an animal rule based on data in the form of a list that is passed as a parameter.
** 
** Example Rule for the data for cow:
** 
(defrule Cow "The rule that checks to see if the animal is a Cow"
   (declare (salience 1))
   (or (Cow) (and
   (attribute (question "1. Is your animal eaten by humans?") (value 1))
      (attribute (question "2. Is your animal domesticated or kept as a pet often?") (value 1))
      (attribute (question "3. Does your animal have fur?") (value 0))
      (attribute (question "4. Does your animal have horns?") (value 1))
      (attribute (question "5. Does your animal have wool?") (value 0))
      (attribute (question "6. Does your animal have hooves?") (value 1))
      (attribute (question "7. Does your animal have more than one color?") (value 1))
      (attribute (question "8. Does your animal eat meat?") (value 0))
      (attribute (question "9. Is your animal aquatic?") (value 0))
      (attribute (question "10. Can your animal fly?") (value 0))
      (attribute (question "11. Does your animal have wings?") (value 0))
      (attribute (question "12. Is your animal a type of fish?") (value 0))
      (attribute (question "13. Is your animal a decapod?") (value 0))
      (attribute (question "14. Is your animal a type of reptile?") (value 0))
      (attribute (question "15. Does your animal lay eggs?") (value 0))
      (attribute (question "16. Does your animal have bones?") (value 1))
      (attribute (question "17. Does your animal build structures?") (value 0))
      (attribute (question "18. Does your animal have a tail?") (value 1))
      (attribute (question "19. Does your animal have legs?") (value 1))
      (attribute (question "20. Does your animal have fins?") (value 0))
      (attribute (question "21. Does your animal have a bill or beak?") (value 0))
      (attribute (question "22. Does it eat fish?") (value 0))
      (attribute (question "23. Does it spend any time or hunt in the ocean?") (value 0))
      (attribute (question "24. Is it a feline?") (value 0))
   )
   )
   =>
   (bind ?victory TRUE)
   (if ?victory then
      (gameOver (ask "Is your animal a Cow?"))
   )
)
** 
** @param ?data a list with the data 
*/
(deffunction createAnimal (?data)
   (bind ?toRule (str-cat "(defrule " (nth$ 1 ?data) " \"The rule that checks to see if the animal is a " (nth$ 1 ?data) "\" 
   (declare (salience 1))
   (or (" (nth$ 1 ?data) ") (and 
   "))
   (for (bind ?i 2) (<= ?i (length$ ?data)) (++ ?i)
      (bind ?toRule (str-cat ?toRule "(attribute (question \"" 
      (nth$ (- ?i 1) ?*PossibleQuestions*) "\") (value " (nth$ ?i ?data) "))
      ")
      )
   )
   (printline (str-cat "Building the rule for a " (nth$ 1 ?data)))

   (if (str-eq "Cow" (nth$ 1 ?data)) then
      (printline "Sample Below: ")
      (printline (str-cat ?toRule "
      )
      )
      =>
      (bind ?victory TRUE)
      (if ?victory then
         (gameOver (ask \"Is your animal a " (nth$ 1 ?data) "?\"))
      ))"))
      (printline "***********")
   )

   (build 
      (str-cat ?toRule "
      )
      )
      =>
      (bind ?victory TRUE)
      (if ?victory then
         (gameOver (ask \"Is your animal a " (nth$ 1 ?data) "?\"))
      ))")
   )
   

) ; (deffunction createAnimal (?data)

/**
** Builds all the rules needed for asking the questions about the game.
** 
** 
** @param ?questionNumber the question number to ask
**/
(deffunction createQuestionRule (?questionNumber)
   ;(printline (str-cat "Building rule for the question " (nth$ (- ?questionNumber) ?*PossibleQuestions*) "."))
   (build 
      (str-cat "(defrule askQuestionNumber" ?questionNumber " \"The rule that will ask the user to resolve the attribute for question id " ?questionNumber ".\"
                  ;(need-attribute (question \"" (nth$ ?questionNumber ?*PossibleQuestions*) "\"))
                  (not (attribute (question \"" (nth$ ?questionNumber ?*PossibleQuestions*) "\")))
                =>
                  (autoAssert " ?questionNumber "(requestInfo " ?questionNumber "))
                )"
      )
   )
)

/*
** Automatically creates logical conclusions based on the fact space.
** 
** @param ?questionNumber the number of the current question
** @param ?val the result of the question that was asked
*/
(deffunction autoAssert (?questionNumber ?val)
   (for (bind ?i 2) (<= ?i (length$ ?*PossibleQuestions*)) (++ ?i)
      (bind ?firstVal (nth$ (+ ?i 1) (split$ (nth$ 1 ?*AnimalData*) ",")))
      (bind ?canAdd TRUE)
      (for (bind ?j 1) (<= ?j (length$ ?*AnimalData*)) (++ ?j)
         (if (not (= ?firstVal (nth$ (+ ?i 1) (split$ (nth$ ?j ?*AnimalData*) ",")))) then
            (bind ?canAdd FALSE)
            (break)
         )
      )
      (if ?canAdd then
         (assert (attribute (question (nth$ ?i ?*PossibleQuestions*)) (value ?firstVal)))
      )
   )
   (return "")
)

(defrule main "The starting point for the game."
   (declare (salience 2))
=>
   (getAnimalData "animalListAndAttributes.csv")
   (for (bind ?i 1) (<= ?i (length$ ?*PossibleQuestions*)) (++ ?i)
      (createQuestionRule ?i)
   )
   (printline "Completed the rule creation of question rules.")
   (printline "Welcome to the animal game!")
   (printline "Choose an animal from the above list and then I'll try to guess which one you are thinking of.")
   (printline "If you are unsure about the answer you can input \"idk\" or any other string that doesnt start with y or n.")
)

(defrule end "The ending point of the game if the user has lost."
   (declare (salience 2))
   (length0)
=>
   (gameOver "n")
)

(defrule end "The ending point of the game if there is an error."
   (declare (salience -9))
=>
   (bind ?toPrint "I know your animal is either a ")
   (for (bind ?i 1) (<= ?i (length$ ?*AnimalData*)) (++ ?i)
      (bind ?toPrint (str-cat ?toPrint (nth$ 1 (split$ (nth$ ?i ?*AnimalData*) ",")) ", "))
      (if (= ?i (- (length$ ?*AnimalData*) 1)) then
         (bind ?toPrint (str-cat ?toPrint "or "))
      )
   )
   (bind ?toPrint (str-cat (sub-string 1 (- (str-length ?toPrint) 2) ?toPrint) "."))
   (printline (str-cat ?toPrint " I'm just going to guess a random one."))
   (eval (str-cat "(assert (" (nth$ 1 (split$ (nth$ (+ (mod (random) (length$ ?*AnimalData*)) 1) ?*AnimalData*) ",")) "))"))
)

(defrule win "The ending point of the game if the user has won."
   (declare (salience 2))
   (length1)
=>
   (eval (str-cat "(assert (" (nth$ 1 (split$ (nth$ 1 ?*AnimalData*) ",")) "))"))
)

(reset)
(run)
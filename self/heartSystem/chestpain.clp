/**
** Diagnoses the users chest pain.
** Possible chestpain causes are listed in chestPainData.csv.
** Recommendations stated in chestPainTreatmentData.csv
** This chest pain diagnostic tool was built around using as many dynamically created rules to streamline this file. 
** All the data for the chest pain is stored in a csv file named chestPainData.csv which must be 
** located in the same folder as the jess directory. 
** To add new diagnoses just answer each of the questions listed in ?*PossibleQuestions* in the csv file, using 1
** to denote yes and 0 to denote no. 
** 
** Requires Dr. Nelson's utilities_v4.clp to be batched in before. (batch "executable\\utilities_v4.clp")
** To run this file within the jess terminal run (batch "executable\\self\\animalGame\\chestPain.clp")
** Alternatively, call runfile.bat in any terminal (windows only).
**
** @author Max Blennemann
** @version 1/5/23
** @version 1.0
*/

(deftemplate attribute (slot question) (slot value))
(do-backward-chaining attribute)
;value is integer either 0 or 1
; 0 is no
; 1 is yes

/**
** This is the possible question bank
** DO NOT CHANGE THE ORDER OF THE QUESTIONS.
** But if you do change the order, reorder the answers in the csv file accordingly.
** If you want to add a question just append it to the end.
** You will need to answer the question for all possible chest pain causes in the csv.
*/
(defglobal ?*PossibleQuestions* = (create$ 
   "Did the pain start recently?";1
   "Does it hurt only when you breathe?";2
   "Does the pain get worse when you are excercising?";3
   "Is there a rash on your chest near the pain area?";4
   "Does the pain feel like a something ripping apart?";5
   "Are you under extreme stress?";6
   "Do you have a fever?";7
   "Have you been recently in a situation where you experienced blunt force trauma?";8
   "Get an x-ray. Does it show a bruised/broken rib?";9
   "Does it hurt only when you eat?";10
   "Do you have yellow skin?";11
   "Do you have leg swelling?"';12
   "Get an x-ray. Do you have tumors?";13
)) ; (defglobal ?*PossibleQuestions* = (create$ 

;use (split$ ?inputRead ",") to access the data as a 2d array
(defglobal ?*ChestPainData* = 
   (create$)
)

/**
** Checks if two strings are equal.
** Will also convert non string things to strings to compare.
** Technically could be used for other data types for equality like numbers or lists.
** 
** @param ?str1 the first String to compare
** @param ?str2 the second String to compare 
** @return true if they are the same otherwise false
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
** @return true if the given response is yes otherwise false
*/
(deffunction requestInfo (?questionNumber)
   (retract-string (str-cat "(need-attribute (question \"" (nth$ ?questionNumber ?*PossibleQuestions*) "\"))"))
   (bind ?response (ask (nth$ ?questionNumber ?*PossibleQuestions*)))
   (bind ?val 3)
   (if (= (asc (sub-string 1 1 ?response)) (asc "y")) then
      (bind ?val 1)
   else 
      (if (= (asc (sub-string 1 1 ?response)) (asc "n")) then
         (bind ?val 0)
      else
         (if (= (asc (sub-string 1 1 ?response)) (asc "i")) then
            (bind ?val 2)
         )
      )
   ); (if (= (asc (sub-string 1 1 ?response)) (asc "y")) then

   ;catches errors below if there are problems with the input and just repeats question
   (if (= ?val 3) then
      (return (requestInfo ?questionNumber))
   )

   (if (= ?val 2) then
      (eval (str-cat "(undefrule askQuestionNumber" ?questionNumber ")"))
   else
      (for (bind ?i 1) (<= ?i (length$ ?*ChestPainData*)) (++ ?i)
         (bind ?thisPossibleCauseData (split$ (nth$ ?i ?*ChestPainData*) ","))

         (if (not (str-eq ?val (nth$ (+ ?questionNumber 1) ?thisPossibleCauseData))) then
            (undefrule (nth$ 1 ?thisPossibleCauseData))
            (eval (str-cat "(retract-string \"(length" (length$ ?*ChestPainData*) ")\")"))
            (printline (str-cat "Based on your answers I have eliminated " (nth$ 1 (split$ (nth$ ?i ?*ChestPainData*) ",")) "."))
            (bind ?*ChestPainData* (delete$ ?*ChestPainData* ?i ?i))
            (eval (str-cat "(assert (length" (length$ ?*ChestPainData*) "))"))
            (-- ?i)
         )
      ) ; (for (bind ?i 1) (<= ?i (length$ ?*ChestPainData*)) (++ ?i)
      (assert (attribute (question (nth$ ?questionNumber ?*PossibleQuestions*)) (value ?val)))
   ) ; (if (= ?val 2) then

   (return (= ?val 1))
); (deffunction requestInfo (?question)

/**
** Concludes the game. Simply gets user input if yes or any other string starting with y
** then I won.
** If any string starting with n then I lost.
** 
** @param ?win whether or not the computer has successfully determined the chest pain cause.
** @return true if the user won otherwise false
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
      (printline "I won probably? :|")
   else
      (if ?val then
         (printline "I won! :)")
      else
         (printline "I lost. :(")
      )
   )
   
   (halt)
   (return ?val)
); (deffunction gameOver (?win)

/**
** Gets the chest pain data from a csv file based on the ?fileName.
** This function will also load the chets pain causes into the game by adding rules. Which correspond to the 
** Known causes of chest pain are listed in the csv. 
** 
** @param ?fileName the file name to use
*/
(deffunction getChestPainData (?fileName)
   (open ?fileName r)
   (bind ?chestPainDataLoading TRUE)
   (while ?chestPainDataLoading
      (bind ?inputRead (readline r)) 
      (if (not (str-eq ?inputRead "EOF")) then
         (bind ?*ChestPainData* (insert$ ?*ChestPainData* (+ 1 (length$ ?*ChestPainData*)) ?inputRead))
         (createCause (split$ ?inputRead ","))
      else
         (printline "Chest pain data loaded as rules.")
         (eval (str-cat "(assert (length" (length$ ?*ChestPainData*) "))"))
         (bind ?chestPainDataLoading FALSE)
      )
   ) ; (while ?chestPainDataLoading
   (return)
) ; (deffunction getChestPainData (?fileName)

/**
** Creates an chest Pain cause rule based on data in the form of a list that is passed as a parameter.
** 
** Example Rule for the data for stress:

(defrule Stress "The rule that checks to see if the cause of chest pain is Stress" 
   (declare (salience 1))
   (or (Stress) (and
      (attribute (question "Did the pain start recently?") (value 0))
      (attribute (question "Does it hurt only when you breathe?") (value 0))
      (attribute (question "Does the pain get worse when you are excercising?") (value 0))
      (attribute (question "Is there a rash on your chest near the pain area?") (value 1))
      (attribute (question "Does the pain feel like a something ripping apart?") (value 0))
      (attribute (question "Are you under extreme stress?") (value 0))
      (attribute (question "Do you have a fever?") (value 0))
      (attribute (question "Have you been recently in a situation where you experienced blunt force trauma?") (value 0))
      (attribute (question "Get an x-ray. Does it show a bruised/broken rib?") (value 0))
      (attribute (question "Does it hurt only when you eat?") (value 0))
      (attribute (question "Do you have yellow skin?") (value 0))
      (attribute (question "Do you have leg swelling?") (value 0))
      (attribute (question "'") (value 0))
   ))
=>
   (bind ?victory TRUE)
   (if ?victory then
      (bind ?victory (gameOver (ask "Do you have Stress?")))
   )
   (if ?victory then
      (getInfo "Stress")
   )
)

** 
** @param ?data a list with the data 
*/
;TODO GENERATE A NEW SAMPLE CODE
(deffunction createCause (?data)
   (bind ?toRule (str-cat "(defrule " (nth$ 1 ?data) " \"The rule that checks to see if the cause of chest pain is " 
   (nth$ 1 ?data) "\" 
   (declare (salience 1))
   (or (" (nth$ 1 ?data) ") (and 
   "))
   (for (bind ?i 2) (<= ?i (length$ ?data)) (++ ?i)
      (bind ?toRule (str-cat ?toRule "(attribute (question \"" 
      (nth$ (- ?i 1) ?*PossibleQuestions*) "\") (value " (nth$ ?i ?data) "))
      ")
      )
   )
   (build 
      (str-cat ?toRule "
      )
      )
      =>
      (bind ?victory TRUE)
      (if ?victory then
         (bind ?victory (gameOver (ask \"Do you have " (nth$ 1 ?data) "?\")))
      )
      (if ?victory then
         (getInfo \"" (nth$ 1 ?data) "\")
      )
      )")
   )

   (return)
) ; (deffunction createCause (?data)

/**
** Builds all the rules needed for asking the questions about the game. The rules outline that 
** if the answer is already known do not ask, and also only ask if I need this attribute.
** Sample Below:

(defrule askQuestionNumber1 "The rule that will ask the user to resolve the attribute for question id 1."
   ;(need-attribute (question "Did the pain start recently?"))
   (not (attribute (question "Did the pain start recently?")))
=>
   (autoAssert 1(requestInfo 1))
)

** 
** @param ?questionNumber the question number to ask
**/
(deffunction createQuestionRule (?questionNumber)
   (build 
      (str-cat "(defrule askQuestionNumber" ?questionNumber 
               " \"The rule that will ask the user to resolve the attribute for question id " ?questionNumber ".\"
                  ;(need-attribute (question \"" (nth$ ?questionNumber ?*PossibleQuestions*) "\"))
                  (not (attribute (question \"" (nth$ ?questionNumber ?*PossibleQuestions*) "\")))
                =>
                  (autoAssert " ?questionNumber "(requestInfo " ?questionNumber "))
                )"
      )
   )
   (return)
);(deffunction createQuestionRule (?questionNumber)

/**
** Gets the relevant information for the diagnosis and prints it.
** 
** @param ?problem the diagnosis as a string
**/
(deffunction getInfo (?problem)
   (open "chestPainTreatmentData.csv" r)
   (bind ?chestPainDataLoading TRUE)
   (bind ?outputData (create$))
   (while ?chestPainDataLoading
      (bind ?inputRead (readline r)) 
      (if (not (str-eq ?inputRead "EOF")) then
         (bind ?outputData (insert$ ?outputData (+ 1 (length$ ?outputData)) ?inputRead))
      else
         (bind ?chestPainDataLoading FALSE)
      )
   ) ; (while ?chestPainDataLoading
   (bind ?i 1)
   (while (<= ?i (length$ ?outputData)) 
      (if (str-eq (nth$ 1 (split$ (nth$ ?i ?outputData) ",")) ?problem) then
         (printline (nth$ 2 (split$ (nth$ ?i ?outputData) ",")))
         (bind ?i (+ 1 (length$ ?outputData)))
      )
      (++ ?i)
   )
   (return)
);(deffunction getInfo (?problem)

/*
** Automatically creates logical conclusions based on the fact space.
** 
** @param ?questionNumber the number of the current question
** @param ?val the result of the question that was asked
*/
(deffunction autoAssert (?questionNumber ?val)
   (for (bind ?i 2) (<= ?i (length$ ?*PossibleQuestions*)) (++ ?i)
      (bind ?firstVal (nth$ (+ ?i 1) (split$ (nth$ 1 ?*ChestPainData*) ",")))
      (bind ?canAdd TRUE)
      (for (bind ?j 1) (<= ?j (length$ ?*ChestPainData*)) (++ ?j)
         (if (not (= ?firstVal (nth$ (+ ?i 1) (split$ (nth$ ?j ?*ChestPainData*) ",")))) then
            (bind ?canAdd FALSE)
            (bind ?j (+ (length$ ?*ChestPainData*) 1))
         )
      )
      (if ?canAdd then
         (assert (attribute (question (nth$ ?i ?*PossibleQuestions*)) (value ?firstVal)))
      )
   )
   (return)
); (deffunction autoAssert (?questionNumber ?val)

(defrule main "The starting point for the game. Prints out dialogue to prompt the user and imports the data."
   (declare (salience 2))
=>
   (getChestPainData "chestPainData.csv")
   (for (bind ?i 1) (<= ?i (length$ ?*PossibleQuestions*)) (++ ?i)
      (createQuestionRule ?i)
   )
   (printline "Completed the rule creation of question rules.")
   (printline "Welcome to my office!")
   (printline "If you have chest pain I'll try to figure out what is causing it.")
   (printline "If you are unsure about the answer you can input \"idk.\" Otherwise enter y(es) or n(o).")
   (printline "Hint: if you want to see all the possible outcomes just answer unknown to everything.")
); (defrule main "The starting point for the game. Prints out dialogue to prompt the user and imports the data."

(defrule end "The ending point of the game if the user has lost."
   (declare (salience 2))
   (length0)
=>
   (gameOver "n")
)

(defrule end "The ending point of the game if there are multiple possible chest pain causes."
   (declare (salience -9))
=>
   (bind ?toPrint "I know your pain is caused by ")

   (for (bind ?i 1) (<= ?i (length$ ?*ChestPainData*)) (++ ?i)
      (bind ?toPrint (str-cat ?toPrint (nth$ 1 (split$ (nth$ ?i ?*ChestPainData*) ",")) ", "))
      (if (= ?i (- (length$ ?*ChestPainData*) 1)) then
         (bind ?toPrint (str-cat ?toPrint "or "))
      )
   )

   (bind ?toPrint (str-cat (sub-string 1 (- (str-length ?toPrint) 2) ?toPrint) "."))
   (printline (str-cat ?toPrint " I'm just going to guess a random one."))

   ;the following line of code will just randomly choose an cause from the list of causes
   (eval (str-cat "(assert (" (nth$ 1 (split$ (nth$ (+ (mod (random) (length$ ?*ChestPainData*)) 1) ?*ChestPainData*) ",")) "))"))
); (defrule end "The ending point of the game if there are multiple possible chest pain results."

(defrule win "The ending point of the game if there is only 1 remaining possible cause. Thus, the program is certain of the answer."
   (declare (salience 2))
   (length1)
=>
   (eval (str-cat "(assert (" (nth$ 1 (split$ (nth$ 1 ?*ChestPainData*) ",")) "))"))
)

(reset)
(run)
/**
* Prints out anagrams of any length less than 11. 
* Requires Dr. Nelson's utilities_v4.clp to be batched in before.
*
* @author Dr. Nelson
* @author Max Blennemann
* @version 9/28/22
*/

(deftemplate Letter (slot c) (slot p)) ;the type c means the char and the type p means place 
; p is needed if there are duplicate characters

/**
* Converts any string into a list of characters.
* 
* @param ?arg the string to convert to create 
* @return the list of every character in the given string
*/
(deffunction slice$ (?arg)
   (bind ?toReturn (create$))
   (for (bind ?i 1) (<= ?i (str-length ?arg)) (++ ?i)
      (bind ?toReturn (insert$ ?toReturn (+ 1 (length$ ?toReturn)) (sub-string ?i ?i ?arg)))
   )
   (return ?toReturn)
)

/**
* Prints out all anagrams of a string of characters equal to or less than 10.
*
* @return false if the function fails, true otherwise
*/
(deffunction anagram ()
   (bind ?word (ask "Enter ten or less ascii characters with no quotes: "))
   (if (not (validate ?word)) then
      (printline "Not ten or less unique ascii characters.")
    else
      (addLetterGroup (slice$ ?word))
      (createAnagramRuleOfLength (str-length ?word))
      (run)
   )
) ; (deffunction anagram())

/**
* Adds an additional letter to the facts.
*
* @param ?letter the letter to add 
* @param ?position the position to use 
*/
(deffunction addLetter (?letter ?position)
   (assert (Letter (c ?letter) (p ?position)))
)

/**
* Asserts multiple letters to the fact base.
*
* @param ?letters the letters to add to the asserted facts. 
*/
(deffunction addLetterGroup (?letters)
   (for (bind ?i 1) (<= ?i (length$ ?letters)) (++ ?i)
      (addLetter (nth$ ?i ?letters) ?i)
   )
)

/**
* Returns true if the input is a ten or less letter string, otherwise false.
*
* @param ?letters the string to validate
* @return true if the parameter is a string of length ten or less otherwise false 
*/
(deffunction validate (?letters)
   (try
      (return (<= (str-length ?letters) 10)) ;ten is used here because any 
      ;length greater than 10 overflows the memory stack 

   catch
      (return FALSE)
   )
)

/**
* Defines a new rule that will create many anagrams using letters that were predefined.
*
* @param ?n the length to use for the new rule
*/
(deffunction createAnagramRuleOfLength (?n)
   (bind ?toRule (str-cat "(defrule anagramVariableLength" " \"Enumerate groups of letters with length " ?n ".\"" ))
   (for (bind ?i 1) (<= ?i ?n) (++ ?i) 
      (bind ?toRule (str-cat ?toRule " (Letter (c ?char" ?i ") (p ?position" ?i))
      (for (bind ?j (- ?i 1)) (>= ?j 1) (-- ?j)
         (bind ?toRule (str-cat ?toRule " &~?position" ?j))
      )
      (bind ?toRule (str-cat ?toRule "))"))
   )
   (bind ?toRule (str-cat ?toRule " => (printout t"))
   (for (bind ?i 1) (<= ?i ?n) (++ ?i)
      (bind ?toRule (str-cat ?toRule " ?char" ?i))
   )
   (build (str-cat ?toRule " \" \"\))"))
);createAnagramRuleOfLength (?n)

(defrule main "The first entry point into the anagram code. Only calls (anagram)."
;no left hand side
;calling (run) will call (anagram)
=>
   (anagram)
)

(defrule undefineAnagramLength "Removes anagramVariableLength after the rule has fired once."
   (declare (salience -1)) 
   ;we need to undefine the anagram of variable length rule for future runs 
   =>
   (undefrule "anagramVariableLength")
)
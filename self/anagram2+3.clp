/**
* Prints out groups of 4 letters without duplicates. 
* Length of 4 is used. 
*
* @author Dr. Nelson
* @author Max Blennemann
* @version 9/21/22
*/

(deftemplate Letter (slot c) (slot p)) ;the type c means the char and the type p means place 
; p is needed if there are duplicate characters, but since we only use each once it is redundant in this case

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
* Prints out all anagrams of a word of length 4.
*
* @param ?word the word to create anagrams of 
*/
(deffunction anagram (?word)
   (addLetterGroup (slice$ ?word))
   (run)
)

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

(defrule rule-4 "Enumerate groups of unique letters with length 4"
   (Letter (c ?c1)); each of these are a parameter that will accept a character
   (Letter (c ?c2 &~?c1)); We relist each character to make sure it doesnt reselect the previous characters
   (Letter (c ?c3 &~?c2 &~?c1))
   (Letter (c ?c4 &~?c3 &~?c2 &~?c1))
=>
   (printout t ?c1 ?c2 ?c3 ?c4 " "); printout the entire anagram
)
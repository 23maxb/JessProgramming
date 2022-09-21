/**
* Prints out groups of ten letters without duplicates. 
* Length of 10 is used, since a length of 11 would generate a stack overflow error.
* The alphabet is mostly commented out below for the user to readd letters if needed.
*
* @author Dr. Nelson
* @author Max Blennemann
* @version 9/19/22
*/

(deftemplate Letter (slot c) (slot p)) ;the type c means the char and the type p means place 
; p is needed if there are duplicate characters, but since we only use each once it is redundant in this case

; this is just the alphabet in order
(assert (Letter (c A) (p 1)))
(assert (Letter (c B) (p 2)))
(assert (Letter (c C) (p 3)))
(assert (Letter (c D) (p 4)))
/*
(assert (Letter (c E) (p 5)))
(assert (Letter (c F) (p 6)))
(assert (Letter (c G) (p 7)))
(assert (Letter (c H) (p 8)))
(assert (Letter (c I) (p 9)))
(assert (Letter (c J) (p 10)))*/

;(assert (Letter (c K) (p 11)))
/*
(assert (Letter (c L) (p 12)))
(assert (Letter (c M) (p 13)))
(assert (Letter (c N) (p 14)))
(assert (Letter (c O) (p 15)))
(assert (Letter (c P) (p 16)))
(assert (Letter (c Q) (p 17)))
(assert (Letter (c R) (p 18)))
(assert (Letter (c S) (p 19)))
(assert (Letter (c T) (p 20)))
(assert (Letter (c U) (p 21)))
(assert (Letter (c V) (p 22)))
(assert (Letter (c W) (p 23)))
(assert (Letter (c X) (p 24)))
(assert (Letter (c Y) (p 25)))
(assert (Letter (c Z) (p 26)))
*/

(defrule rule-2 "Enumerate groups of unique letters with length 10"
   (Letter (c ?c1)); each of these are a parameter that will accept a character
   (Letter (c ?c2 &~?c1)); We relist each character to make sure it doesnt reselect the previous characters
   (Letter (c ?c3 &~?c2 &~?c1))
   (Letter (c ?c4 &~?c3 &~?c2 &~?c1))
   /*
   (Letter (c ?c5 &~?c4 &~?c3 &~?c2 &~?c1))
   (Letter (c ?c6 &~?c5 &~?c4 &~?c3 &~?c2 &~?c1))
   (Letter (c ?c7 &~?c6 &~?c5 &~?c4 &~?c3 &~?c2 &~?c1))
   (Letter (c ?c8 &~?c7 &~?c6 &~?c5 &~?c4 &~?c3 &~?c2 &~?c1))
   (Letter (c ?c9 &~?c8 &~?c7 &~?c6 &~?c5 &~?c4 &~?c3 &~?c2 &~?c1))
   (Letter (c ?c10 &~?c9 &~?c8 &~?c7 &~?c6 &~?c5 &~?c4 &~?c3 &~?c2 &~?c1))*/
 ;  (Letter (c ?c11 &~?c10 &~?c9 &~?c8 &~?c7 &~?c6 &~?c5 &~?c4 &~?c3 &~?c2 &~?c1))
=>
   (printout t ?c1 ?c2 ?c3 ?c4 " "); printout the entire anagram
)
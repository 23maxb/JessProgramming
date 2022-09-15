/*
** May 17, 2019
**
** Dr. Eric R. Nelson
**
** Rules and assertions showing how to do different kinds of pattern matching
** using &, |, ~, :() and (test)
**
*/ 

(assert (bob 1))
(assert (bob 2))
(assert (bob 3))
(assert (bob 4))


/*
** This rule shows how to match an OR condition within a pattern without using the (OR) operator
*/
(defrule bobORrule "Prints only if the pattern (bob 1) or (bob 2) exists using |"
   (bob ?x & 1 | 2)
=>
   (printout t "bob 1 OR 2 : " ?x crlf)
)


(defrule bobNOTrule "Prints only if the pattern (bob ?) exists and ? is not a 1 or a 2"
   (bob ?x &~1 &~2)
=>
   (printout t "bob NOT 1 AND NOT 2 : " ?x crlf)
)

(defrule bobNOT3rule "Prints only if the pattern (bob ?) exists and ? is not a 1 or a 2 and not 3 using the colon operator"
   (bob ?x &~1 &~2 & :(<> ?x 3)) 
=>
   (printout t "bob NOT 1 AND NOT 2 and also not 3: " ?x crlf)
)

(defrule bobNOT4rule "Prints only if the pattern (bob ?) exists and ? is not a 1 or a 2 and not 4 using the test operator"
   (bob ?x &~1 &~2)
   (test (<> ?x 4))
=>
   (printout t "bob NOT 1 AND NOT 2 and also not 4: " ?x crlf)
)


/**
*Creates a fibbonacci fucntion that can be called.
*
*@author Max Blennemann
*@version 8/29/22
*@param ?arg the number to be factorialized
*@return the value of the factorial
*/
(deffunction fact (?arg)
   (if (= ?arg 0) then (return 1))
   (return (* (fact (- ?arg 1)) ?arg))
)

/**
*Runs factorial with the error checking. If there is a 0 it will return 0.
*
*@return the factorial value computed
*/
(deffunction factorial ()
   (print crlf)
   (bind ?a (ask "Enter a number for factorial: "))
   
   (if (not (numberp ?a)) then 
      (printline "Not a number.") 
      (return -1)
   )
   (if (not (> ?a 0)) then 
      (printline "Not greater than 0.") 
      (return -1)
   )

   (bind ?b (fact (integer ?a)))
   (print ?b)
   
   (print crlf)
   (return ?b)
)

(factorial) ;calls factorial 
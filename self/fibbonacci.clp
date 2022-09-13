/**
* Creates a fibonacci function that can be executed.
* The Fibonacci sequence is an infinitely long sequence of numbers. 
* Each number in the list besides the first two elements is the sum of the two preceding ones. 
* The sequence used in this program begins with 1 and 1.
* Requires utilities_v3.clp to be batched in before the execution of this program.
*
* @author Max Blennemann
* @version 8/29/22
*/

/**
* Returns true if the input is a positive integer.
* Otherwise returns false.
*
* @param ?arg the number to check
* @return true if the input is a positive integer otherwise false.
*/
(deffunction validate(?arg)
   (if (and (and (numberp ?arg) (> ?arg 0)) (= (round ?arg) ?arg)) then 
      (bind ?arg TRUE)
    else
      (bind ?arg FALSE)
   )
   (return ?arg)
)

/**
* Returns a sublist of the fibonacci sequence starting with 1 and 1. 
* This sublist is of length ?arg.
* This function does no error handling, and will throw an error if an invalid input is provided.
*
* @param ?arg The length of the list to be returned.
* @return A sorted list of the first ?arg elements of the fibbonacci sequence.
*/
(deffunction fibo (?arg)
   (bind ?toReturn (create$))
   (for (bind ?i 1) (<= ?i ?arg) (++ ?i)
      (if (<= ?i 2) then 
         (bind ?toReturn (insert$ ?toReturn (+ 1 (length$ ?toReturn)) 1))
       else
         (bind ?toReturn (insert$ ?toReturn (+ 1 (length$ ?toReturn)) (+ (nth$ (- ?i 1) ?toReturn) (nth$ (- ?i 2) ?toReturn))))
      )
   )
   return ?toReturn
) ; deffunction fiboList (?arg) 

/**
* Returns a sublist of the fibonacci sequence starting with 1 and 1. 
* This sublist is of length ?a.
* This function will tell the user if an invalid input is provided.
* This function will not crash if an invalid input is provided.
* This function does no truncating of demcimals.
* This function will print out what exactly will be calculated.
* If the number given is extremely close to a valid positive integer, it will be rounded.
*
* @param ?n The length of the list to be returned.
* @return A sorted list of the first ?arg elements of the fibbonacci sequence.
*/
(deffunction fibonacci (?n)
   (if (validate ?n) then 
      (print "Calculating fibbonacci of ")
      (print (integer ?n))
      (print ".")
      (print crlf)
      (bind ?toReturn (fibo (integer ?n)))
    else
      (printline "Not a positive integer or positive integer representation.")
      (bind ?toReturn FALSE)
   )
   (return ?toReturn)
)

/**
* Returns a sublist of the fibonacci sequence starting with 1 and 1 of length given by the user. 
* This function will tell the user if an invalid input is provided.
* This function will not crash if an invalid input is provided.
* This function does no truncating of demcimals.
* This function will print out the resulting list or false if an invalid input is provided.
*/
(deffunction fib ()
   (print crlf)
   (bind ?userInput (ask "Enter a number for fibonacci: "))
   (print crlf)
   (print (fibonacci ?userInput))
   (print crlf)
)
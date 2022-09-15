/**
* Contains a function that will split a string into a list of characters 
* and a function that prints out an alphatic histogram.
*
* @author Max Blennemann
* @version 9/7/22
*/

;;;;;Enter Text Below;;;;;

(defglobal ?*toHisto* =
"Hi Dr. Nelson,

Replace the text here and make sure to still have quotation marks.
Happy testing!

-Max"
)

;;;;;Do not edit below or the code may not work;;;;;

(defglobal ?*MAX_ASCII_POSSIBLE* = 255) ; maximum ascii value

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
* Takes input from the user and then prints out the frequency of each letter.
* Despite the output, both uppercase and lower case letters will be listed under the uppercase letter.
* Will accept all ascii characters including semicolons.
*/
(deffunction histo ()
   (bind ?sliced (slice$ ?*toHisto*))
   (bind ?charCount (create$))
   (for (bind ?i 1) (<= ?i (+ ?*MAX_ASCII_POSSIBLE* 1)) (++ ?i)
      (bind ?charCount (insert$ ?charCount ?i 0))
   )
   (for (bind ?i 1) (<= ?i (length$ ?sliced)) (++ ?i)
      (bind ?letterIndex (asc (lowcase (nth$ ?i ?sliced))))
      (bind ?charCount (replace$ ?charCount ?letterIndex ?letterIndex (+ (nth$ ?letterIndex ?charCount) 1)))
   )
   (bind ?charCount (subseq$ ?charCount (asc "a") (asc "z")))
   (for (bind ?i 1) (<= ?i (length$ ?charCount)) (++ ?i)
      (printline (str-cat (toChar (- (+ ?i (asc "A")) 1)) ": " (nth$ ?i ?charCount)))
   )
) ;deffunction histo ()

(histo)
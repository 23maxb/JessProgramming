/*
** January 18, 2019
** Dr. Eric R. Nelson
**
** Small collection of JESSS utilities for ATCS:Expert Systems
**
** print       - prints any number of arguments
** printlist   - prints out the arguments as a literal list followed by a crlf
** printline   - print followed by a newline
** ask         - prompt the user and read back a token
** askline     - prompt the user and read back a line of text (a string)
** askQuestion - adds a question mark to the prompt used in ask
** toChar      - given a unicode long integer value, returns the unicode character as a string
** asciiList$  - Returns a mixed list of symbols/tokens for the ASCII character set that JESS understands.
** asciiToChar - Returns the value from asciiList$ for the number passed to the function 
** boolp       - Test for boolean type
** xor         - Exclusive-OR for two boolean values
** append$     - Appends stuff to a list
** Fixes the casting a long to a long crash
**
** To use these functions add the following line to the top of your primary code file.
** (batch utilities_v4.clp)
*/

/*
** Function that prints out a prompt. It's just a bit shorter than using (printout t "text")
**
** Note that when passing a list it is the arguments, and not the list, that get printed.
** Use printlist to print out a list "as is"
**
*/
(deffunction print ($?args)
   (foreach ?arg ?args (printout t ?arg))
   (return)
)

/*
** Prints out the arguments as a single literal list followed by a crlf
*/
(deffunction printlist ($?args)
   (printout t ?args crlf)
   (return)
)

/*
** print with a new-line at the end
*/
(deffunction printline ($?args)
   (print ?args crlf)
   (return)
)

/*
** ask the user for input and return a token
*/
(deffunction ask ($?args)
   (print ?args)
   (return (read))
)

/*
** Same as ask but returns a string of text
*/
(deffunction askline ($?args)
   (print ?args)
   (return (readline))
)

/*
** Appends a question mark to the prompt of the ask function
*/ 
(deffunction askQuestion ($?args)
   (print ?args)
   (return (ask "? "))
)

/*
** This function returns the unicode character given a number (a long value)
** The format function is just printf() in C, but unlike printout this function returns the output,
** so if you use a nil router you don't get a printout, just the return value. A thanks goes to Henry W. for finding this out.
*/
(deffunction toChar (?unicode)
   (return (format nil "%c" (long ?unicode)))
)

/*
** returns a mixed-type list of the ascii characters understood by JESS
** Control characters are represented as tokes such as NUL CR LF
** Printable characters are represented as strings
*/
(deffunction asciiList$ ()
   (return (create$
            NUL SOH STX ETX EOT ENQ ACK BEL BS TAB LF VT FF CR SO SI DLE DC1 DC2 DC3 DC4 NAK SYN ETB CAN EM SUB ESC FS GS RS US
            " " "!" "\"" "#" "$" "%" "&" "'" "(" ")" "*" "+" "," "-" "." "/"
            "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
            ":" ";" "<" "=" ">" "?" "@"
            "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
            "[" "\\" "]" "^" "_" "`"
            "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
            "{" "|" "}" "~" DEL
            "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" ""                   ; 128 - 160
            "¡" "¢" "£" "¤" "¥" "¦" "§" "¨" "©" "ª" "«" "¬" "­" "®" "¯" "°" "±" "²" "³" "´" "µ" "¶" "·" "¸"
            "¹" "º" "»" "¼" "½" "¾" "¿" "À" "Á" "Â" "Ã" "Ä" "Å" "Æ" "Ç" "È" "É" "Ê" "Ë" "Ì" "Í" "Î" "Ï" "Ð" "Ñ" "Ò" "Ó" "Ô" "Õ" "Ö" "×"
            "Ø" "Ù" "Ú" "Û" "Ü" "Ý" "Þ" "ß" "à" "á" "â" "ã" "ä" "å" "æ" "ç" "è" "é" "ê" "ë" "ì" "í" "î" "ï" "ð" "ñ" "ò" "ó" "ô" "õ" "ö"
            "÷" "ø" "ù" "ú" "û" "ü" "ý" "þ" "ÿ"
           ) ;create$
   ) ; return
) ; deffunction asciiList

/*
** Returns the ASCII character (as JESS understands them) for the given integer value
** Printable characters starting with space are returned as strings
** Control characters (ASCII values less than 32) are returned as tokens of their code such as NUL BRK TAB ESC
** Quotes and \ are escaped, so \" and \\ strings are returned
** If the argument is outside the range 0 to 255 then nil is returned
** 
** Precondition: The argument must be a number. Non-integer values are simply cast as integers
**
*/
(deffunction asciiToChar (?ascii)
   (bind ?ascii (integer ?ascii))            ; don't want to crash if someone passes 64.5 for example, so just truncate

   (if (or (< ?ascii 0) (> ?ascii 255)) then ; return nil if value is out of the range 0 to 255
      (bind ?ret nil)
    else
      (++ ?ascii)                            ; ASCII is zero index but lists are 1 indexed
      (bind ?ret (nth$ ?ascii (asciiList$))) ; get the character from the list of available characters
   )

   (return ?ret)
) ; deffunction asciiToChar

/*
** Tests if the argument is a boolean, which can only take on the value of TRUE and FALSE 
*/
(deffunction boolp (?x)
   (return (or (eq ?x TRUE) (eq ?x FALSE)))
)

/*
** Simple exclusive-or function
** 
** Function assumes values are either TRUE or FALSE
*/
(deffunction xor (?a ?b)
   (return (and (or ?a ?b) (not (and ?a ?b))))
)

/*
** Appends stuff to the first argument, which must be a list. The other arguments can be
** anything and are all passed to this function as a list. The return value is a list that
** is the concatenation of all the arguments in the order given. The logic is simple enough, but
** calling it append$ makes it clear what is going on.
** 
*/
(deffunction append$ (?theList $?stufftoAppend)
   (return (create$ ?theList $?stufftoAppend))
)

/*
** Fix the casting a long to a long crash bug, e.g. (long 1L) causes a crash.
** Jump in front of the (long) function, test for a long and just return the first argument passed if true,
** otherwise just defer to the original function. Note that (long 1 2 3 4) will just return 1.
*/
(defadvice before long (if (longp (nth$ 2 $?argv)) then (return (nth$ 2 $?argv))))

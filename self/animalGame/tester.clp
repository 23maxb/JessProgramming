(do-backward-chaining b)
(do-backward-chaining e)

(defrule a
    (b)
    =>
    (printout t "called a" crlf)
)

(defrule d
    (not (b))
    (e)
)

(defrule c
    (need-b)
    =>
    (assert (b))

)

(run)
(define atom? (lambda (x) (and (not (pair? x)) (not (null? x)))))

(define (build s1 s2) (cons s1 (cons s2 '())))

;; an entry is a pair of lists whose first list is a set.
;; and the twor lists must be of equal length. (k-v)
(define new-entry build)
(define (lookup-in-entry name entry entry-f)
	(define (lookup-in-entry-help name names values entry-f)
		(cond
			((null? names) (entry-f name))
			((eq? name (car names)) (car values))
			(else (lookup-in-entry-help name 
					(cdr names)
					(cdr values)
					entry-f))))
	(lookup-in-entry-help
		name
		(first entry)
		(second entry)
		entry-f))

;; a table(also called environment) is a list of entries.

;; creating a new table by putting the new entry in front of the old table.
(define extend-table cons)

;; mention the trick in this function!!!!
(define (lookup-in-table name table table-f)
	(if (null? table)
		(table-f name)
		(lookup-in-entry 
			name
			(car table)
			(lambda (name)
				(lookup-in-table
					name
					(cdr table)
					table-f)))))

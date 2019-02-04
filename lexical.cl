(setq *print-case* :downcase)

(setq lexeme_list '());lexeme list to operators,keywords,boolean,ınteger
(setq output_temp '());Fot each elements of output
(setq output '()) ; List to output

(defun getChar () ;Take char and then decide which charachter class

    (let ((c (read-char s nil)))
        (setq nextChar c)
        (if (eq c nil)
            (setq charClass 114) ;End of file
                (if (not(null (alpha-char-p c)));Letter
                    (setq charClass 111)
                    (if (not(null (digit-char-p c)));number
                        (setq charClass 112)
                        (setq charClass 113); unknown charachter (like operator etc.)
                    )
                )
        )
    )
 )   

(defun getNonBlank () ;to pass if char is equal tab or space
    (loop while (or (and (not(eq nextChar nil)) (char= nextChar #\space)) (and (not(eq nextChar nil)) (char= nextChar #\tab))) 
        do
        (getChar)
    )
    (if (eq nextChar #\Newline);pass if char is equal newline
        (getChar)
    )
)   


(defun lookup (c) ; to check which operator used (")", "(", "-", "+", "*", "/")

        (cond
            ((char= c  #\() ;  
                (setf nextToken "operator")
            )
            ((char= c #\)) 
                (setf nextToken "operator")
            )
            ((char= c #\-) 
                (setf nextToken "operator")
            )
            ((char= c #\*) 
                (setf nextToken "operator")
            )
            ((char= c #\+) 
                (setf nextToken "operator")
            )
            ((char= c #\/) 
                (setf nextToken "operator")
            )
        )
        nextToken
)

(defun lex() ; to decide token and lexeme
    
    (getNonBlank)
    ;(format t "~a ~a "  nextChar charClass)

    (cond
            ((= charClass 111) ;letter
                ;(format t "~%LETTER~%")
                
                (push nextChar lexeme_list)
                (getChar)
                ;loop for adding character and take new char to nexChar 
                (loop while  (eq charClass 111) 
                    do
                    (push nextChar (cdr(last lexeme_list)))
                    (getChar)
                )

                (setq nextToken "identifier")
                (if (equal nextToken "identifier")
                    (check_Keyword)
                )
                ; pushes and pops for output  
                (push nextToken output_temp)
                (push output_temp output)
                (pop output_temp)
                (pop output_temp)
                ;(writeToLexeme lexeme_list)
                (cond 
                    ((char= nextChar #\)) ;checking right parantheses after "identifier", "keyword" or "boolean" 
                        (push "operator" output_temp)
                        (push  ")" output_temp)
                        (push output_temp output)
                        (pop output_temp)
                        (pop output_temp)
                    )
                    
                )

            )
            ((= charClass 112) 
                ;(format t "~%DIGIT~%")
                (push nextChar lexeme_list)
                (getChar)
                ;loop for adding charachter and take new char to nexChar 
                (loop while (eq charClass 112) 
                    do                
                    (push nextChar (cdr(last lexeme_list)))
                    (getChar)
                )
                (setq nextToken "integer")
                ;(format t "Next Token is: ~a ------------" nextToken)
                ;(format t " Next Lexeme is:")
                 ; pushes and pops for output  
                (push (string(car lexeme_list)) output_temp)
                (push nextToken output_temp)
                (push output_temp output)
                (pop output_temp)
                (pop output_temp)
                (cond 
                    ((char= nextChar #\));checking right parantheses after "identifier", "keyword" or "boolean" 
                        (push  ")" output_temp)
                        (push "operator" output_temp)
                        (push output_temp output)
                        (pop output_temp)
                        (pop output_temp)
                    )
                    
                )
            )
            ((= charClass 113) 
                ;(format t "~%UNKNOWN~%")
                (push nextChar lexeme_list)
                (lookup nextChar) ; look for unknown character is which operator or not
               ; (format t "Next Token is: ~a ------------" nextToken)
                ;(format t " Next Lexeme is:")
                ; pushes and pops for output  
                (push (string(car lexeme_list)) output_temp)
                (push nextToken output_temp)
                (push output_temp output)
                (pop output_temp)
                (pop output_temp)

            )
            ;End of file
            ((= charClass 114) 
                (setq nextToken "EOF")
            )
    )

    ;loop to clear lexeme list to take new lexeme
    (loop for line = lexeme_list 
        do (pop lexeme_list)
        until (eq lexeme_list nil)
    )
    
)


(defun check_Keyword ()

            ;To convert list elements concatenate in one string to control with keywords 
            (setq i (- (list-length lexeme_list) 1))
            (setq string_Lexeme (subseq (string (car lexeme_list)) 0))
            (setq lexeme_list2 lexeme_list)
            (loop while (> i 0)
                do 
                    (setq lexeme_list2 (cdr lexeme_list2))
                    (setq string_Lexeme (concatenate 'string string_Lexeme (string (car lexeme_list2))))
                    ;(format t "~a~%" string_Lexeme)
                    (setq i (- i 1))
                    ;(format t "Count: ~a~%" i)
            )

            ;(format t "~a~%" string_Lexeme)
            ;(format t "~a~%" lexeme_list)
            (push string_Lexeme output_temp)

            (cond
                ((string= string_Lexeme "deffun") 
                    (setq nextToken "Keyword")
                )
                ((string= string_Lexeme "and") ;  
                    (setq nextToken "Keyword")
                )
                ((string= string_Lexeme "or") 
                    (setq nextToken "Keyword")
                )
                ((string= string_Lexeme "not") 
                    (setq nextToken "Keyword")
                )
                ((string= string_Lexeme "equal") 
                    (setq nextToken "Keyword")
                )
                ((string= string_Lexeme "append") 
                    (setq nextToken "Keyword")
                )
                ((string= string_Lexeme "concat") 
                    (setq nextToken "Keyword")
                )
                ((string= string_Lexeme "set") 
                    (setq nextToken "Keyword")
                )
                ((string= string_Lexeme "for") 
                    (setq nextToken "Keyword")
                )
                ((string= string_Lexeme "while") 
                    (setq nextToken "Keyword")
                )
                ((string= string_Lexeme "if") 
                    (setq nextToken "Keyword")
                )
                ((string= string_Lexeme "exit") 
                    (setq nextToken "Keyword")
                )                   
            )
            ;maybe lexeme can be boolean  
            (check_Boolean string_Lexeme)
    nextToken
)

;check lexeme is boolean or not
(defun check_Boolean(string_Lexeme)
    (cond
        ((string= string_Lexeme "true") 
            (setq nextToken "Boolean")
        )
        ((string= string_Lexeme "false") 
            (setq nextToken "Boolean")
        )
    )
)

;main function take filename and get char until end of file
(defun lexer (filename)
    ;(pop lexeme_list)
    (let ((stream (open filename)))
        (setq s stream)
        (loop for line = (getChar) ; stream, no error, :eof value
            do (lex)
            until (eq nextChar nil)
        )
        (close stream)
    )

    (reverse output) ; reverse end then print as list 
)

,           ; read a digit
>,          ; and another

; make sure that their sum is not equal to 10 because it can't handle that

; add the two numbers, putting the result in cell 0
[
    <+      ; add one to the cell 0
    >-      ; subtract one from cell 1
]           ; repeat until cell 1 has value 0 in it

; but we can't display the cell's value since it'd be ASCII for some control
; so we add 48 to the cell

++++++++    ; loop counter: cell 1
[
    <++++++ ; add 6 * 8 = 48 to cell 0
    >-      ; decrease the loop counter in cell 1
]
<.          ; move back to cell 0 and print value

; print a newline, ASCII code 10

>>++        ; loop counter: cell 2
[
    <+++++  ; add 5 * 2 = 10 to cell 1
    >-      ; decrease loop counter in cell 2
]
<.          ; move back to cell 1 and print value

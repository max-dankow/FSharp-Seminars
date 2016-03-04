let rec pascal col row =
    if col=0 || col=row
    then 1
    else pascal col (row - 1) + pascal (col - 1) (row - 1)

let printPascalLn n = 
    for i = 0 to n do printf "%d " (pascal i n)

printPascalLn 4

let printIt n = 
    for i = 0 to n do 
        for i = 0 to n do printf "%d " (pascal i n)
        printfn ""
                 
printIt 10

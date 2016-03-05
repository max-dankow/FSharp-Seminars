let rec pascal col row =
    if col=0 || col=row
    then 1
    else pascal col (row - 1) + pascal (col - 1) (row - 1)

let printIt n = 
    for i = 0 to n do 
        for j = 0 to i do printf "%d " (pascal j i)
        printfn ""
                 
printIt 20


let income = [120000; 45000; 200000; 85000; 300000; 60000; 150000]

let rec pratyushprintList lst =
    match lst with
    | [] -> ()
    | head :: tail ->
        printfn "%A" head
        pratyushprintList tail
pratyushprintList income

// ----------------------------------------------------------------------
let highIncome = List.filter (fun salary -> salary > 100000) income


printfn "Salaries above $100,000 are :"
pratyushprintList highIncome


// ---------------------------------------------------------------------------
let income1 = [120000; 45000; 200000; 85000; 300000; 60000; 150000]

let calculateNetSalary salary =
    let tax =
        match salary with
        | s when s <= 49020 -> float s * 0.15
        | s when s <= 98040 -> (49020.0 * 0.15) + (float (s - 49020) * 0.205)
        | s when s <= 151978 -> (49020.0 * 0.15) + (49020.0 * 0.205) + (float (s - 98040) * 0.26)
        | s when s <= 216511 -> (49020.0 * 0.15) + (49020.0 * 0.205) + (53938.0 * 0.26) + (float (s - 151978) * 0.29)
        | s -> (49020.0 * 0.15) + (49020.0 * 0.205) + (53938.0 * 0.26) + (64533.0 * 0.29) + (float (s - 216511) * 0.33)
    float salary - tax


let netSalaries = List.map calculateNetSalary income
printfn "Net salaries after tax:"
netSalaries |> List.iter (printfn "%.2f")
//--------------------------------------------------------------------------------
let adjustSalaries salary =
    if salary < 49020.0 then salary + 20000.0 else salary

let adjustedSalaries = List.map adjustSalaries netSalaries
printfn "Adjusted Salaries after adding $20,000:"
adjustedSalaries |> List.iter (printfn "%.2f")

// --------------------------------------------------------------------------------------------------
let inRangeSalaries = List.filter (fun salary -> salary >= 50000.0 && salary <= 100000.0) adjustedSalaries
let sumSalaries = List.fold (fun acc salary -> acc + salary) 0.0 inRangeSalaries
printfn "Sum of salaries between $50,000 and $100,000: %.2f" sumSalaries
//---------------------------------------------------------------------------------------------------
let sumOfMultiplesOf3 n =
    let rec sum current total =
        if current > n then
            total
        else
            sum (current + 3) (total + current)
    
    sum 3 0
let result = sumOfMultiplesOf3 27
printfn "The sum of multiples of 3 up to 27 is: %d" result


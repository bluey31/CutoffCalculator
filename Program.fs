open System

let rec exp b p = 
    match p with
    | 0 -> b
    | _ -> 10.0 * (exp b (p - 1)) 

let rec nexp b p = 
    match p with
    | 0 -> b
    | _ -> 0.1 * (nexp b (p - 1))

let calculateCutoff Ω F =
    int (1.0 / (2.0 * Math.PI * Ω * F)) 

let getResistorValue ohms ohmsUnit =
    match ohmsUnit with
    | "Ω"  -> (float ohms)
    | "kΩ" -> exp (float ohms) 3
    | "MΩ" -> exp (float ohms) 6
    | _    -> failwith "Unrecognized unit for resistor value"

let getCapacitorValue farads faradsUnit =
    match faradsUnit with
    | "F"  -> (float farads)
    | "uF" -> nexp (float farads) 6
    | "nF" -> nexp (float farads) 9
    | "pF" -> nexp (float farads) 12
    | _    -> failwith "Unrecognized unit for capacitor value" 

[<EntryPoint>]
let main argv =
    printfn "------ Circuit Cutoff Calculator ------"
   
    match Seq.toList argv with
    | ohms :: ohmsUnit :: farads :: faradsUnit  :: []  ->
        let resistorValue = getResistorValue ohms ohmsUnit 
        let capacitorValue = getCapacitorValue farads faradsUnit
       
        let cutoff = calculateCutoff resistorValue capacitorValue 
        printfn "Cutoff: %dHz" cutoff

    | _ -> failwith "Expected Resistor Value with units  (i.e. 33 kΩ) and Capacitor Value with units (i.e. 5.6 nF)"

    0 

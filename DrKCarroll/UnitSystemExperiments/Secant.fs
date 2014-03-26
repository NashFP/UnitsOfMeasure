module Secant

let private iterationLimit = 100

let secant f x0 x1 convergenceCriterion =

    let rec nextx x1 f1 x0 f0 nIterations =
        if nIterations > iterationLimit then
            (x1, false)
        else
            let deltax = x1 - x0

            if abs deltax < convergenceCriterion then
                (x1, true)
            else
                let denom = f1 - f0

                if abs denom < convergenceCriterion then
                    printfn "Reset at x = %f" x1
                    let tweakedX = x1 + 50.0 * convergenceCriterion
                    nextx tweakedX (f tweakedX) x1 f1 (nIterations + 1)
                else
                    let x1p1 = (x0 * f1 - x1 * f0) / denom
                    nextx x1p1 (f x1p1) x1 f1 (nIterations + 1)

    nextx x1 (f x1) x0 (f x0) 0

let f x = 
    let fx = x * x - 3.0
    printfn "x: %f, fx: %f" x fx
    fx

let (xf, converged) = secant f 0.0 1.0 1e-10

///////////

// Same function, except solves when parameters have Units of Measure
let secant2 (f: float<'a> -> float<'b>) (x0: float<'a>) (x1: float<'a>) (convergenceCriterion: float) =

    let xConvergence = LanguagePrimitives.FloatWithMeasure<'a> convergenceCriterion 
    let fConvergence = LanguagePrimitives.FloatWithMeasure<'b> convergenceCriterion 

    let rec nextx (x1:float<'a>) (f1:float<'b>) (x0:float<'a>) (f0:float<'b>) nIterations =
        if nIterations > iterationLimit then
            (x1, false)
        else
            let deltax = x1 - x0

            if abs deltax < xConvergence then
                (x1, true)
            else
                let denom = f1 - f0

                if abs denom < fConvergence then
                    printfn "Reset at x = %f" (float x1)
                    let tweakedX = x1 + 50.0 * xConvergence
                    nextx tweakedX (f tweakedX) x1 f1 (nIterations + 1)
                else
                    let x1p1 = (x0 * f1 - x1 * f0) / denom
                    nextx x1p1 (f x1p1) x1 f1 (nIterations + 1)

    nextx x1 (f x1) x0 (f x0) 0

[<Measure>] type ft

let f2 (x : float<ft>) =
    let fx = (x * x) - 3.0<ft^2>
    printfn "x (ft): %f, fx (ft^2): %f" (float x) (float fx)
    fx

let (xf2, converged2) = secant2 f2 1.0<ft> 2.0<ft> 5e-5


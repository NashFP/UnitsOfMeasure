module CantileverBeamFS

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames     // kilogram, metre, second, ampere, kelvin, mole, candela
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols   // kg, m, s, A, ...

[<Measure>] type length = m
[<Measure>] type mass = kg
[<Measure>] type time = s
[<Measure>] type force = mass length/time^2
[<Measure>] type area = length^2
[<Measure>] type stress = force/area
[<Measure>] type pressure = stress
[<Measure>] type speed = length/time
[<Measure>] type accel = speed/time

[<Measure>] type foot = static member perMeter = 0.3048<foot/m>
[<Measure>] type feet = foot
[<Measure>] type ft = foot

let getI1 (b:float) (h:float) =
    b * h**3.0 / 12.0

//let getI2 (b:float<length>) (h:float<length>) =
//    b * h**3 / 12.0

let getI (b:float<length>) (h:float<length>) =
    b * h * h * h / 12.0

let getDeflection1 (P:float<N>) (L:float<m>) (E:float<N/m^2>) (I:float<m^4>) = 
    P * L * L * L / (3.0 * E * I)

let getDeflection2 
        (P:float<force>) (L:float<length>) 
        (E:float<stress>) (I:float<length^4>) = // : float<mass> =
    P * L * L * L / (3.0 * E * I)

let E = 209e9<N/m^2>
let length = 3.0<m>
let load = 400.0<N>

let forceRatio = load / 1.0<force>
let forceRatio2 = load / 1.0<mass accel>

let length2 = 2.0<foot>

//let totalLength = length + length2

let ftToLength (x:float<ft>) = x * 0.3048<length/ft>

let totalLength = length + ftToLength length2
let totalLength2 = length + length2 / foot.perMeter

let I = getI  0.010<length> 0.025<length>

let deflection1 = getDeflection1 load length E I
let deflection2 = getDeflection2 load length E I

let deflectionRatio = deflection1 / deflection2

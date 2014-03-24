module CantileverBeam

open System
open UnitSystem
open UnitSystem.Utility
open Units.Fundamental
open Units.Derived
open Units.SI
open Units.US

let getI1 (b:Quantity) (h:Quantity) =
    checkDimensionalConstraints [(b, Length); (h, Length)]
    b * h**3 / 12.0

let getI2 (b:Quantity) (h:Quantity) =
    returnChecked ((b * h**3 / 12.0), AreaMomentOfInertia)

let getDeflection (P:Quantity) (L:Quantity) (E:Quantity) (I:Quantity) =
    checkDimensionalConstraints [
        (P, Force) 
        (L, Length) 
        (E, Stress) 
        (I, AreaMomentOfInertia)]

    P * L**3 / (3 * E * I)

let getDeflection2 (P:Quantity) (L:Quantity) (E:Quantity) (I:Quantity) =
    returnChecked (P * L**3 / (3 * E * I), Length)

let E = 29e6*Psi
let length = 3*Feet
let load = 100.0*Lbf
let I = getI1 (0.5*Inch) (2.0*Inch)

let deflection = getDeflection load length E I
let deflection2 = getDeflection2 load length E I

let length2 = 4*Meter
let bogusLength = 4*Lbf

let totalLength = length + length2
//let bogusTotalLength = length + bogusLength

let totalLengthInInches = totalLength /. Inch

let dInches = deflection /. Inch
let dMillimeters = deflection /. Millimeter


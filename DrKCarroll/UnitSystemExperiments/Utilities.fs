module UnitSystem.Utility

open System
open UnitSystem.Units.Fundamental
open UnitSystem.Units.SI
open UnitSystem.Units.US

let fromCelsius(t:float) = (t + 273.15) * Quantity.Temperature
let fromFahrenheit(t:float) = ((t + 459.67) / 1.8) * Quantity.Temperature

let asCelsius(t:Quantity) =
    t.ConfirmIsConsistentWith(Temperature)
    (t / Kelvin).BaseValue - 273.15

let asFahrenheit(t:Quantity) =
    t.ConfirmIsConsistentWith(Temperature)
    (t / Rankine).BaseValue - 459.67

let radiansToDegrees a = 180.0 * a / Math.PI
let degreesToRadians a = Math.PI * a / 180.0

let dimensionsf (q:Quantity) = q.DimensionsToString()

let checkDimensionalConstraints (constraints:(Quantity * Quantity) list) = Quantity.CheckDimensionalConstraints(constraints)
let returnChecked ((quantity:Quantity), (expected:Quantity)) = Quantity.ReturnChecked(quantity, expected)

let fromBaseUnitString (unitName : string) =
    match unitName.Trim().ToLower() with
    | "m"
    | "meter"
    | "meters"
    | "metre"
    | "metres"      -> Meter

    | "mm"
    | "millimeter"
    | "millimeters"
    | "millimetre"
    | "millimetres" -> Millimeter

    | "cm"
    | "centimeter"
    | "centimeters"
    | "centimetre"
    | "centimetres" -> Centimeter

    | "km"
    | "kilometer"
    | "kilometers"
    | "kilometre"
    | "kilometres" -> Kilometer

    | "kg"
    | "kilogram"
    | "kilograms"   -> Kilogram

    | "g"
    | "gram"
    | "grams"       -> 1e-3 * Kilogram

    | "s"
    | "sec"
    | "second"
    | "seconds"     -> Second

    | "min"
    | "minute"
    | "minutes"     -> Minute

    | "h"
    | "hr"
    | "hour"
    | "hours"       -> Hour

    | "d"
    | "day"
    | "days"        -> 24.0 * Hour

    | "wk"
    | "week"
    | "weeks"       -> 7.0 * 24.0 * Hour

    | "n"
    | "newton"
    | "newtons"     -> Newton

    | "j"           
    | "joule"
    | "joules"      -> Joule

    | "in"
    | "inch"
    | "inches"      -> Inch

    | "ft"
    | "foot"
    | "feet"        -> Foot

    | "yd"
    | "yard"
    | "yards"       -> Yard

    | "mi"
    | "mile"
    | "miles"       -> Mile

    | "lbm"         -> Lbm

    | "lb"
    | "lbf"
    | "pound"
    | "pounds"      -> Lbf

    | "pa"
    | "pascal"
    | "pascals"     -> Pa

    | "kpa"
    | "kilopascal"
    | "kilopascals" -> 1e3 * Pa

    | "mpa"
    | "megapascal"
    | "megapascals" -> 1e6 * Pa

    | "gpa"
    | "gigapascal"
    | "gigapascals" -> 1e9* Pa

    | "psi"         -> Psi
    | "psf"         -> Psf

    | _             -> failwithf "Unsupported unit [%s]" unitName

let fromUnitString  (unitName : string) =
    match unitName.Trim().ToLower() with
    | "foot"
    | "feet"
    | "ft" -> Foot

    | "inch"
    | "in"
    | "inches" -> Inch

    | "yd"
    | "yard" -> Yard

    | "meters"
    | "meter"
    | "m" -> Meter

    | "millimeters"
    | "millimeter"
    | "mm" -> Millimeter

    | "centimeter"
    | "centimeters"
    | "cm" -> Centimeter

    | "kilometer"
    | "kilometers"
    | "km" -> Kilometer

    | "mi"
    | "mile"
    | "miles" -> Mile

    | "lb/ft^2"
    | "lbpersqft"
    | "psf" -> Psf

    | "lb/in^2"
    | "lbpersqin"
    | "psi" -> Psi

    | "pa" -> Pa
    | "kpa" -> kPa
    | "mpa" -> MPa
    | "mph" -> Mph

    | "kph"
    | "kmperhr"
    | "km/hr" -> Kilometer / Hour

    | "feetpersec"
    | "footpersec"
    | "ft/s" -> FeetPerSecond

    | "meter/s"
    | "m/s" -> Meter / Second

    | "lbf"
    | "lb" -> Lbf
    | "kip" -> 1000.0 * Lbf

    | "ft-lb"
    | "ft-lbf" -> FootPound

    | "in-lb"
    | "in-lbf" -> InchPound

    | "joule"
    | "joules"
    | "j" -> Joule

    | "newton"
    | "newtons"
    | "n" -> Newton

    | "newton-meter"
    | "newton-meters"
    | "n-m" -> NewtonMeter

    | "lbm" -> Lbm

    | "psfperft"
    | "psf/ft" -> PsfPerFoot

    | "psiperin"
    | "psi/in" -> Psi / Inch

    | "paperm"
    | "pa/m" -> Pa / Meter

    | "kpaperm"
    | "kpa/m" -> kPa / Meter

    | "radian"
    | "radians"
    | "rad" -> Radian

    | "angulardegree"
    | "degree"
    | "deg" -> AngularDegree

    | "ms"
    | "millisecond"
    | "milliseconds"
        -> 1e-3 * Second

    | "s"
    | "sec"
    | "second"
    | "seconds" -> Second

    | "min"
    | "minute"
    | "minutes" -> Minute

    | "hr"
    | "hour"
    | "hours" -> Hour

    | "d"
    | "day"
    | "days" -> 24.0 * Hour

    | "dimensionless" -> Dimensionless

    | _ -> failwith "Invalid Unit"
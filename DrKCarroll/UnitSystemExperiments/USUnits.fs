module UnitSystem.Units.US

open UnitSystem //.Configurable
open UnitSystem.Units.SI

let Foot = 0.3048 * Meter
let Feet = Foot
let Inch = Foot / 12.0
let Inches = Inch
let Yard = 3 * Feet
let Mile = 5280 * Feet

let Mph = Mile / Hour
let FeetPerSecond = Foot / Second

let Lbm = 0.45359237 * Kilogram
let Lbf = Lbm * g
let Kip = 1000 * Lbf

let FootPound = Foot * Lbf
let InchPound = Inch * Lbf

let Psf = Lbf / Foot**2
let Psi = Lbf / Inch**2

let Ksi = 1000 * Psi

let PsfPerFoot = Psf / Foot
let PsiPerInch = Psi / Inch

let Rankine = Kelvin / 1.8


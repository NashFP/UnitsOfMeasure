module UnitSystem.Units.SI

open System
open UnitSystem //.Configurable, .Bits
open UnitSystem.Units.Fundamental

let Meter = Length

let Meters = Meter
let Centimeter = Meter / 100
let Millimeter = Meter / 1000
let Kilometer = 1000 * Meter

let Second = Time
let Minute = 60 * Second
let Hour = 60 * Minute
let Day = 24 * Hour

let Kph = Kilometer / Hour
let MeterPerSecond = Meter / Second

let g = 9.80665 * Meter / Second**2

let Kilogram = Mass
let Kg = Mass

let Newton = Kilogram * Meter / Second**2
let Joule = Newton * Meter
let NewtonMeter = Joule

let Pa = Newton / Meter**2
let kPa = 1000 * Pa
let MPa = 1e6 * Pa
let GPa = 1e9 * Pa

let PaPerMeter = Pa / Meter
let kPaPerMeter = kPa / Meter

let Radian = Dimensionless
let AngularDegree = Radian * (Math.PI / 180.0)

let Ampere = Current
let Kelvin = Temperature



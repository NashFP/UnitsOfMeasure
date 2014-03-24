module UnitSystem.Units.Derived

open UnitSystem // .Configurable, .Bits

let Area = Fundamental.Length * Fundamental.Length
let Volume = Area * Fundamental.Length
let Speed = Fundamental.Length / Fundamental.Time
let Acceleration = Fundamental.Length / Fundamental.Time**2
let Force = Fundamental.Mass * Acceleration
let Pressure = Force / Area
let Stress = Pressure
let Moment = Force * Fundamental.Length
let Torque = Moment
let AreaMoment = Fundamental.Length * Area
let AreaMomentOfInertia = Fundamental.Length**4



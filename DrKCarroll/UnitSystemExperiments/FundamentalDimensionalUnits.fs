module UnitSystem.Units.Fundamental

#if UseConfigurable

open UnitSystem.Configurable

let Mass = Quantity(1.0, 0)
let Length = Quantity(1.0, 1)
let Time = Quantity(1.0, 2)
let Dimensionless = Quantity.Dimensionless

#else 
#if UseBits

open UnitSystem.Bits

let Mass = Quantity(1.0, mass = 1)
let Length = Quantity(1.0, length = 1)
let Time = Quantity(1.0, time = 1)

#else

open UnitSystem

let Mass = Quantity.Mass
let Length = Quantity.Length
let Time = Quantity.Time
let Current = Quantity.Current
let Temperature = Quantity.Temperature
let Substance = Quantity.Substance
let Intensity = Quantity.Intensity
let Dimensionless = Quantity.Dimensionless

#endif
#endif

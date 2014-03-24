namespace UnitSystem.Bits

open System

type private Dimension =
    | Dimensionless = -1
    | Mass = 0
    | Length = 1
    | Time = 2
    | Current = 3
    | Temperature = 4
    | Substance = 5
    | Intensity = 6

[<Struct>] 
type Quantity(b:float, bits:uint64) =

    member this.BaseValue = b
    member this.ExponentBits = bits

    static member private initialize value shift =
        (uint64 (byte (match value with | Some(v) -> v | None -> 0))) <<< shift
    
    new (b:float, ?mass:int, ?length:int, ?time:int, 
                ?current:int, ?temperature:int, ?substance:int, ?intensity:int) = 
        Quantity(
            b,
            (Quantity.initialize mass 0) ||| 
            (Quantity.initialize length 8) |||
            (Quantity.initialize time 16) |||
            (Quantity.initialize current 24) |||
            (Quantity.initialize temperature 32) |||
            (Quantity.initialize substance 40) |||
            (Quantity.initialize intensity 48))

    static member private dimensionOrZero dim = match dim with | Some(d) -> d | None -> 0

    new (?mass:int, ?length:int, ?time:int, 
                ?current:int, ?temperature:int, ?substance:int, ?intensity:int) = 
        Quantity(1.0, 
            Quantity.dimensionOrZero mass, 
            Quantity.dimensionOrZero length, 
            Quantity.dimensionOrZero time, 
            Quantity.dimensionOrZero current, 
            Quantity.dimensionOrZero temperature, 
            Quantity.dimensionOrZero substance, 
            Quantity.dimensionOrZero intensity)

    new (ts:TimeSpan) = Quantity(ts.TotalSeconds, time = 1)

    static member Dimensionless = Quantity(1.0)
    static member Mass = Quantity(1.0, mass = 1)
    static member Length = Quantity(1.0, length = 1)
    static member Time = Quantity(1.0, time = 1)
    static member Current = Quantity(1.0, current = 1)
    static member Temperature = Quantity(1.0, temperature = 1)
    static member Substance = Quantity(1.0, substance = 1)
    static member Intensity = Quantity(1.0, intensity = 1)

    static member ZeroDimensionless = 0.0 * Quantity.Dimensionless
    static member ZeroMass = 0.0 * Quantity.Mass
    static member ZeroLength = 0.0 * Quantity.Length
    static member ZeroTime = 0.0 * Quantity.Time
    static member ZeroCurrent = 0.0 * Quantity.Current
    static member ZeroTemperature = 0.0 * Quantity.Temperature
    static member ZeroSubstance = 0.0 * Quantity.Substance
    static member ZeroIntensity = 0.0 * Quantity.Intensity

    static member val DefaultEpsilon = 5e-5 with get, set

    member inline this.IsConsistentWith (q:Quantity) = this.ExponentBits = q.ExponentBits
    member inline this.IsNotConsistentWith (q:Quantity) = this.ExponentBits <> q.ExponentBits
    member inline this.IsDimensionless () = this.ExponentBits = 0UL
    member inline this.IsNotDimensionless() = this.ExponentBits <> 0UL

    member this.EqualsWithinEpsilon(other:Quantity, ?epsilon:float) =
        let eps = match epsilon with 
                  | None -> Quantity.DefaultEpsilon 
                  | Some(e) -> e
        this.IsConsistentWith(other) && abs (this.BaseValue - other.BaseValue) < eps

    member inline private this.getExponent (d:Dimension) =
        sbyte (((0xFFUL <<< (8 * (int d))) &&& this.ExponentBits) >>> (8 * (int d)))

    static member private dimensionSymbol dimension =
        match dimension with
        | Dimension.Mass            -> "M"
        | Dimension.Length          -> "L"
        | Dimension.Time            -> "T"
        | Dimension.Current         -> "C"
        | Dimension.Temperature     -> "K"
        | Dimension.Substance       -> "S"
        | Dimension.Intensity       -> "I"
        | Dimension.Dimensionless   -> ""
        | _                         -> "?"

    static member private dimensionByIndex index =
        match index with
        | 0 -> Dimension.Mass        
        | 1 -> Dimension.Length      
        | 2 -> Dimension.Time        
        | 3 -> Dimension.Current     
        | 4 -> Dimension.Temperature 
        | 5 -> Dimension.Substance   
        | 6 -> Dimension.Intensity   
        | 7
        | _ -> Dimension.Dimensionless   

    static member private dimensionString (symbol, exponent) =        
        match exponent with
        | 0y -> ""
        | 1y -> symbol
        | _  -> symbol + "^" + exponent.ToString()

    member private this.dimensionString dimension =
        Quantity.dimensionString (Quantity.dimensionSymbol dimension, this.getExponent dimension)

    /// <summary>
    /// formats the Dimensions using Normal form (e.g., [M^2L^-2T^-1)])
    /// </summary>
    member this.DimensionsToNString() =
        "[" +
            this.dimensionString Dimension.Mass +
            this.dimensionString Dimension.Length +
            this.dimensionString Dimension.Time +
            this.dimensionString Dimension.Current +
            this.dimensionString Dimension.Temperature +
            this.dimensionString Dimension.Substance +
            this.dimensionString Dimension.Intensity +
            "]";

//    member this.DimensionsToNString2 () =
//        "[" + (
//                this.Dimensions
//                |> Array.mapi (fun i e -> (Quantity.dimensionSymbol i, e))
//                |> Array.fold (fun accum (s, e) -> accum + Quantity.dimensionString(s, e)) ""
//               ) + "]"

    /// <summary>
    /// formats the Dimensions using Quotient form (e.g., [M^2/(L^2T)])
    /// </summary>
    member this.DimensionsToQString() =
        let dimensionExponents = BitConverter.GetBytes(this.ExponentBits) |> Array.map (fun b -> sbyte b)

        let (numer, negDenom) = dimensionExponents
                                |> Array.mapi (fun i e -> (Quantity.dimensionByIndex i, e))
                                |> Array.filter (fun (_, e) -> e <> 0y)
                                |> Array.partition (fun (_, e) -> e > 0y)

        let denom = negDenom |> Array.map (fun (d, e) -> (d, -e))

        let formatDimensions (exponents:(Dimension * sbyte) array) =
            exponents 
            |> Array.fold (fun accum (s, e) -> accum + Quantity.dimensionString(Quantity.dimensionSymbol s, e) ) ""

        sprintf "[%s%s]" 
            (formatDimensions numer) 
            (if not <| Array.isEmpty denom then
                "/" + 
                    (match denom.Length = 1 with
                    | true -> formatDimensions denom
                    | false -> "(" + formatDimensions denom + ")")
              else "")

    /// <summary>
    /// Static Flag to determine if Dimensions get printed by default 
    /// in Normal form [MLT^-2] or Quotient form [ML/T^2]
    /// </summary>
    static member val UseQ = true with get, set

    member this.DimensionsToString () =
        (if Quantity.UseQ then this.DimensionsToQString() else this.DimensionsToNString())

    override this.ToString() = this.BaseValue.ToString() + this.DimensionsToString()

    member this.Zero() = Quantity(0.0, this.ExponentBits)

    member this.ConfirmIsConsistentWith (that:Quantity) =
        if this.IsNotConsistentWith(that) then 
            failwithf "Attempted operation on quantities with inconsistent units: %s <> %s" 
                        (this.DimensionsToString()) (that.DimensionsToString())

    member this.ConfirmIsDimensionless() =
        if this.IsNotDimensionless() then 
            failwithf "Attempted operation on quantity with non-dimensionless units: %s" 
                        (this.DimensionsToString())

    static member (+) (left:Quantity, right:Quantity) =
        left.ConfirmIsConsistentWith(right)
        Quantity(left.BaseValue + right.BaseValue, left.ExponentBits)
    
    static member (-) (left:Quantity, right:Quantity) =
        left.ConfirmIsConsistentWith(right)
        Quantity(left.BaseValue - right.BaseValue, left.ExponentBits)
    
    static member (~-) (quan:Quantity) = Quantity(-quan.BaseValue, quan.ExponentBits)
    
    static member private combineExponentBits op (leftExponentBits:uint64) (rightExponentBits:uint64) =
        let leftBytes = BitConverter.GetBytes(leftExponentBits)
        let rightBytes = BitConverter.GetBytes(rightExponentBits)
        BitConverter.ToUInt64((leftBytes, rightBytes) ||> Array.map2 (fun l r -> (op) l r), 0)
 
    member private this.negateExponentBits() =
        let bytes = BitConverter.GetBytes(~~~this.ExponentBits) |> Array.map (fun b -> b + 1uy)
        BitConverter.ToUInt64(bytes, 0)

    static member (*) (left:Quantity, right:Quantity) =
        Quantity(left.BaseValue * right.BaseValue, Quantity.combineExponentBits (+) left.ExponentBits right.ExponentBits)
    
    static member (/) (left:Quantity, right:Quantity) =
        Quantity(left.BaseValue / right.BaseValue, Quantity.combineExponentBits (-) left.ExponentBits right.ExponentBits)

    static member (/.) (quan:Quantity, unit:Quantity) =
        quan.ConfirmIsConsistentWith(unit)
        (quan / unit).BaseValue

    member this.ConvertTo (unit:Quantity) = this /. unit

    static member (*) (left:float, right:Quantity) = Quantity(left * right.BaseValue, right.ExponentBits)
    static member (/) (left:float, right:Quantity) = Quantity(left / right.BaseValue, right.negateExponentBits())
    
    static member (*) (left:Quantity, right:float) = Quantity(left.BaseValue * right, left.ExponentBits)
    static member (/) (left:Quantity, right:float) = Quantity(left.BaseValue / right, left.ExponentBits)

    static member (*) (left:float32, right:Quantity) = Quantity(float left * right.BaseValue, right.ExponentBits)
    static member (/) (left:float32, right:Quantity) = Quantity(float left / right.BaseValue, right.negateExponentBits())
    
    static member (*) (left:Quantity, right:float32) = Quantity(left.BaseValue * double right, left.ExponentBits)
    static member (/) (left:Quantity, right:float32) = Quantity(left.BaseValue / double right, left.ExponentBits)

    static member (*) (left:decimal, right:Quantity) = Quantity(float left * right.BaseValue, right.ExponentBits)
    static member (/) (left:decimal, right:Quantity) = Quantity(float left / right.BaseValue, right.negateExponentBits())
    
    static member (*) (left:Quantity, right:decimal) = Quantity(left.BaseValue * double right, left.ExponentBits)
    static member (/) (left:Quantity, right:decimal) = Quantity(left.BaseValue / double right, left.ExponentBits)

    static member (*) (left:int, right:Quantity) = Quantity(float left * right.BaseValue, right.ExponentBits)
    static member (/) (left:int, right:Quantity) = Quantity(float left / right.BaseValue, right.negateExponentBits())
    
    static member (*) (left:Quantity, right:int) = Quantity(left.BaseValue * float right, left.ExponentBits)
    static member (/) (left:Quantity, right:int) = Quantity(left.BaseValue / float right, left.ExponentBits)

    static member (*) (left:int64, right:Quantity) = Quantity(float left * right.BaseValue, right.ExponentBits)
    static member (/) (left:int64, right:Quantity) = Quantity(float left / right.BaseValue, right.negateExponentBits())
    
    static member (*) (left:Quantity, right:int64) = Quantity(left.BaseValue * float right, left.ExponentBits)
    static member (/) (left:Quantity, right:int64) = Quantity(left.BaseValue / float right, left.ExponentBits)

    static member (*) (left:bigint, right:Quantity) = Quantity(float left * right.BaseValue, right.ExponentBits)
    static member (/) (left:bigint, right:Quantity) = Quantity(float left / right.BaseValue, right.negateExponentBits())
    
    static member (*) (left:Quantity, right:bigint) = Quantity(left.BaseValue * float right, left.ExponentBits)
    static member (/) (left:Quantity, right:bigint) = Quantity(left.BaseValue / float right, left.ExponentBits)

    member private this.multiplyExponentBitsByPower (p:sbyte) =
        let bytes = BitConverter.GetBytes(this.ExponentBits) |> Array.map (fun b -> byte (sbyte b * p)) 
        BitConverter.ToUInt64(bytes, 0)
    
    static member Pow (mantissa:Quantity, exponent:int) =
        Quantity(Math.Pow(mantissa.BaseValue, float exponent), mantissa.multiplyExponentBitsByPower(sbyte exponent))

    member this.Pow (exponent:int) = Quantity.Pow(this, exponent)

    member private this.rootExponentBits nth =
        let (rootMass, remainderMass) = Math.DivRem(int (this.getExponent Dimension.Mass), nth)
        if remainderMass <> 0 then failwithf "Mass Dimension is not an integral power of %d" nth

        let (rootLength, remainderLength) = Math.DivRem(int (this.getExponent Dimension.Length), nth)
        if remainderLength <> 0 then failwithf "Length Dimension is not an integral power of %d" nth

        let (rootTime, remainderTime) = Math.DivRem(int (this.getExponent Dimension.Time), nth)
        if remainderTime <> 0 then failwithf "Time Dimension is not an integral power of %d" nth

        let (rootCurrent, remainderCurrent) = Math.DivRem(int (this.getExponent Dimension.Current), nth)
        if remainderCurrent <> 0 then failwithf "Current Dimension is not an integral power of %d" nth

        let (rootTemperature, remainderTemperature) = Math.DivRem(int (this.getExponent Dimension.Temperature), nth)
        if remainderTemperature <> 0 then failwithf "Temperature Dimension is not an integral power of %d" nth

        let (rootSubstance, remainderSubstance) = Math.DivRem(int (this.getExponent Dimension.Substance), nth)
        if remainderSubstance <> 0 then failwithf "Substance Dimension is not an integral power of %d" nth

        let (rootIntensity, remainderIntensity) = Math.DivRem(int (this.getExponent Dimension.Intensity), nth)
        if remainderIntensity <> 0 then failwithf "Intensity Dimension is not an integral power of %d" nth

        BitConverter.ToUInt64(
            [| 
                byte rootMass
                byte rootLength
                byte rootTime
                byte rootCurrent
                byte rootTemperature
                byte rootSubstance
                byte rootIntensity 
                0uy
            |], 0)

    static member Root((mantissa:Quantity), nth) =
        Quantity (Math.Pow(mantissa.BaseValue, 1.0 / float nth), mantissa.rootExponentBits nth)
   
    member this.Root nth = Quantity.Root(this, nth)

    static member Sqrt q = Quantity.Root(q, 2)
    member this.Sqrt () = Quantity.Sqrt this

    member private this.transcendental trans :float =
        this.ConfirmIsDimensionless()
        trans this.BaseValue

    member this.Sin () = this.transcendental Math.Sin
    member this.Cos () = this.transcendental Math.Cos 
    member this.Tan () = this.transcendental Math.Tan 

    member this.Sinh () = this.transcendental Math.Sinh 
    member this.Cosh () = this.transcendental Math.Cosh 
    member this.Tanh () = this.transcendental Math.Tanh 

    member this.Log () = this.transcendental Math.Log 
    member this.Log10 () = this.transcendental Math.Log10 

    member this.Asin () = Quantity(this.transcendental Math.Asin, 0UL)
    member this.Acos () = Quantity(this.transcendental Math.Acos, 0UL)
    member this.Atan () = Quantity(this.transcendental Math.Atan, 0UL)

    static member Atan2 (y:Quantity, x:Quantity) =
        y.ConfirmIsConsistentWith(x)
        Quantity(Math.Atan2(y.BaseValue, x.BaseValue), 0UL)

    member this.Sign() = Math.Sign this.BaseValue
    member this.Abs() = Quantity(abs this.BaseValue, this.ExponentBits)

    static member op_Implicit (ts:TimeSpan) = Quantity(ts.TotalSeconds, time = 1)

    static member op_Explicit (q:Quantity) : TimeSpan =
        q.ConfirmIsConsistentWith(Quantity.Time)
        TimeSpan.FromSeconds(q.BaseValue)

    static member CheckDimensionalConstraints (constraints:(Quantity * Quantity) list) =
        let constraintsNotMet = 
            constraints
            |> List.mapi (fun i (x, u) -> (i + 1, x, u)) 
            |> List.filter (fun (_, x, u) -> x.IsNotConsistentWith(u))

        match constraintsNotMet with
        | [] -> ()
        | _ ->  let sb = new System.Text.StringBuilder()
                constraintsNotMet 
                |> List.iter (fun (i, actual, required) 
                                -> sb.AppendFormat(
                                        "{0}Dimensional Constraint {1} not met. {2}, {3} are inconsistent.", 
                                        System.Environment.NewLine, 
                                        i, 
                                        actual.DimensionsToString(), 
                                        required.DimensionsToString()) |> ignore)
                failwithf "Dimensional Consistency Error: %s" (sb.ToString())

    static member CheckDimensionalConstraints (constraints: Quantity array) =
        if constraints.Length = 0 || ((constraints.Length &&& 1) = 1) then
            failwith "Invalid constraint array provided. Number of constraints must be divisible by 2."
        Quantity.CheckDimensionalConstraints [ for i in 0..2..(constraints.Length - 1) -> (constraints.[i], constraints.[i + 1])]

    static member CheckDimensionalConstraints (constraints: (Quantity * Quantity) seq) =
        Quantity.CheckDimensionalConstraints (Seq.toList constraints)

    static member ReturnChecked (computed:Quantity, expected:Quantity) =
        computed.ConfirmIsConsistentWith(expected)
        computed

    member this.RoundUp (roundTo:Quantity) = Math.Ceiling(this /. roundTo) * roundTo
    member this.RoundDown (roundTo:Quantity) = Math.Floor(this /. roundTo) * roundTo

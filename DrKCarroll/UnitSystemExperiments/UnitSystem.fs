namespace UnitSystem

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

type private Dimensions = sbyte array

[<Struct; CustomEquality; CustomComparison>]
type Quantity (b:float, dimensions:Dimensions) = 

    member this.BaseValue = b
    member this.Dimensions = dimensions

    override this.Equals(ob:obj) = 
        match ob with
        | :? Quantity as that -> this.IsConsistentWith(that) && this.BaseValue = that.BaseValue
        | _ -> false

    override this.GetHashCode() =
        this.BaseValue.GetHashCode() ^^^ this.Dimensions.GetHashCode()

    interface IComparable<Quantity> with 
        member this.CompareTo(that) =
            let dimensionComparisons =
                (this.Dimensions, that.Dimensions) 
                ||> Array.map2 (fun e1 e2 -> e1.CompareTo(e2)) 
                |> Array.filter (fun c -> c <> 0)
                    
            match dimensionComparisons with
            | [||] -> this.BaseValue.CompareTo(that.BaseValue)
            | _ -> dimensionComparisons.[0]

    static member private makeDimension (d:Dimension): Dimensions =
        [| for i in 0..7 -> if (int d) = i then 1y else 0y |]

    static member val private DimensionlessDimension = Quantity.makeDimension Dimension.Dimensionless
    static member val private MassDimension = Quantity.makeDimension Dimension.Mass
    static member val private LengthDimension = Quantity.makeDimension Dimension.Length
    static member val private TimeDimension = Quantity.makeDimension Dimension.Time
    static member val private CurrentDimension = Quantity.makeDimension Dimension.Current
    static member val private TemperatureDimension = Quantity.makeDimension Dimension.Temperature
    static member val private SubstanceDimension = Quantity.makeDimension Dimension.Substance
    static member val private IntensityDimension = Quantity.makeDimension Dimension.Intensity

    static member val Dimensionless = Quantity(1.0, Quantity.DimensionlessDimension)
    static member val Mass = Quantity(1.0, Quantity.MassDimension)
    static member val Length = Quantity(1.0, Quantity.LengthDimension)
    static member val Time = Quantity(1.0, Quantity.TimeDimension)
    static member val Current = Quantity(1.0, Quantity.CurrentDimension)
    static member val Temperature = Quantity(1.0, Quantity.TemperatureDimension)
    static member val Substance = Quantity(1.0, Quantity.SubstanceDimension)
    static member val Intensity = Quantity(1.0, Quantity.IntensityDimension)

    static member val ZeroDimensionless = Quantity(0.0, Quantity.DimensionlessDimension)
    static member val ZeroMass = Quantity(0.0, Quantity.MassDimension)
    static member val ZeroLength = Quantity(0.0, Quantity.LengthDimension)
    static member val ZeroTime = Quantity(0.0, Quantity.TimeDimension)
    static member val ZeroCurrent = Quantity(0.0, Quantity.CurrentDimension)
    static member val ZeroTemperature = Quantity(0.0, Quantity.TemperatureDimension)
    static member val ZeroSubstance = Quantity(0.0, Quantity.SubstanceDimension)
    static member val ZeroIntensity = Quantity(0.0, Quantity.IntensityDimension)

    new (baseValue:float) = Quantity(baseValue, Quantity.DimensionlessDimension)
    new (dimensions:Dimensions) = Quantity(1.0, dimensions)
    new (ts:TimeSpan) = Quantity(ts.TotalSeconds, Quantity.TimeDimension)

    member this.IsNotConsistentWith (that:Quantity) = (this.Dimensions, that.Dimensions) ||> Array.exists2 (<>)
    member this.IsConsistentWith (that:Quantity) = not <| this.IsNotConsistentWith(that)
    member this.IsNotDimensionless () = this.Dimensions |> Array.exists (fun e -> e <> 0y)
    member this.IsDimensionless () = not <| this.IsNotDimensionless()

    static member val DefaultEpsilon = 5e-5 with get, set
      
    static member EqualsWithinEpsilon (this:Quantity, that:Quantity) =
        this.IsConsistentWith that && abs (this.BaseValue - that.BaseValue) < Quantity.DefaultEpsilon

    static member EqualsWithinEpsilon (this:Quantity, that:Quantity, epsilon:float) =
        this.IsConsistentWith that && abs (this.BaseValue - that.BaseValue) < epsilon

    member this.EqualsWithinEpsilon (that:Quantity) = Quantity.EqualsWithinEpsilon(this, that)
    member this.EqualsWithinEpsilon (that:Quantity, epsilon:float) = Quantity.EqualsWithinEpsilon(this, that, epsilon)

    static member (=~) (this:Quantity, that:Quantity) = Quantity.EqualsWithinEpsilon(this, that)

    /// <summary>
    /// Maps the dimensional index to the SI standard dimensional symbol
    /// ref: http://physics.nist.gov/Pubs/SP330/sp330.pdf, pg 11
    /// </summary>
    static member private dimensionSymbol index =
        [| "M"; "L"; "T"; "I"; "Θ"; "N"; "J"; "?" |].[index]

    static member private dimensionString (symbol, exponent) =        
        match exponent with
        | 0y -> ""
        | 1y -> symbol
        | _  -> symbol + "^" + exponent.ToString()

    member private this.dimensionString index =
        Quantity.dimensionString (Quantity.dimensionSymbol index, this.Dimensions.[index])

    /// <summary>
    /// formats the Dimensions using Normal form (e.g., [M^2L^-2T^-1)])
    /// </summary>
    member this.DimensionsToNString () =
        "[" +
        this.dimensionString (int Dimension.Mass) +
        this.dimensionString (int Dimension.Length) +
        this.dimensionString (int Dimension.Time) +
        this.dimensionString (int Dimension.Current) +
        this.dimensionString (int Dimension.Temperature) +
        this.dimensionString (int Dimension.Substance) +
        this.dimensionString (int Dimension.Intensity) +
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
    member this.DimensionsToQString () =
        let (numer, negativeDenom) = 
            this.Dimensions
            |> Array.mapi (fun i e -> (Quantity.dimensionSymbol i, e))
            |> Array.filter (fun (_, e) -> e <> 0y)
            |> Array.partition (fun (_, e) -> e > 0y)

        let denom = negativeDenom |> Array.map (fun (s, e) -> (s, -e))

        let formatDimensions (exponents:(string * sbyte) array) =
            exponents 
            |> Array.fold (fun accum (s, e) -> accum + Quantity.dimensionString(s, e)) ""

        sprintf "[%s%s]" (formatDimensions numer) 
            (if not <| Array.isEmpty denom then
                let denomString = formatDimensions denom
                let parenthesized s = "(" + s + ")"
                "/" + if denom.Length = 1 then denomString else parenthesized denomString
              else "")

    /// <summary>
    /// Static Flag to determine if Dimensions get printed by default 
    /// in Normal form [MLT^-2] or Quotient form [ML/T^2]
    /// </summary>
    static member val UseQ = true with get, set

    member this.DimensionsToString () =
        (if Quantity.UseQ then this.DimensionsToQString() else this.DimensionsToNString())

    override this.ToString () = this.BaseValue.ToString() + this.DimensionsToString()

    member this.Zero() = Quantity(0.0, this.Dimensions)

    member this.ConfirmIsConsistentWith (that:Quantity) =
        if this.IsNotConsistentWith(that) then 
            failwithf "Attempted operation on quantities with inconsistent units: %s <> %s" 
                        (this.DimensionsToString()) (that.DimensionsToString())

    member this.ConfirmIsDimensionless () =
        if this.IsNotDimensionless() then 
            failwithf "Attempted operation on quantity with non-dimensionless units: %s" 
                        (this.DimensionsToString())

    static member (+) (left:Quantity, right:Quantity) =
        left.ConfirmIsConsistentWith(right)
        Quantity(left.BaseValue + right.BaseValue, left.Dimensions)
    
    static member (-) (left:Quantity, right:Quantity) =
        left.ConfirmIsConsistentWith(right)
        Quantity(left.BaseValue - right.BaseValue, left.Dimensions)

    static member (%) (left:Quantity, right:Quantity) =
        left.ConfirmIsConsistentWith(right)
        Quantity(left.BaseValue % right.BaseValue, left.Dimensions)
    
    static member (~-) (quan:Quantity) = Quantity(-quan.BaseValue, quan.Dimensions)
    static member (~+) (quan:Quantity) = quan
    
    static member (*) (left:Quantity, right:Quantity) =
        Quantity(left.BaseValue * right.BaseValue, (left.Dimensions, right.Dimensions) ||> Array.map2 (+))
    
    static member (/) (left:Quantity, right:Quantity) =
        Quantity(left.BaseValue / right.BaseValue, (left.Dimensions, right.Dimensions) ||> Array.map2 (-))

    static member (/.) (quan:Quantity, unit:Quantity) =
        quan.ConfirmIsConsistentWith(unit)
        (quan / unit).BaseValue

    member this.ConvertTo (unit:Quantity) = this /. unit

    static member (*) (left:float, right:Quantity) = Quantity(left * right.BaseValue, right.Dimensions)
    static member (/) (left:float, right:Quantity) = Quantity(left / right.BaseValue, right.Dimensions |> Array.map (~-))
    
    static member (*) (left:Quantity, right:float) = Quantity(left.BaseValue * right, left.Dimensions)
    static member (/) (left:Quantity, right:float) = Quantity(left.BaseValue / right, left.Dimensions)

    static member (*) (left:float32, right:Quantity) = Quantity(float left * right.BaseValue, right.Dimensions)
    static member (/) (left:float32, right:Quantity) = Quantity(float left / right.BaseValue, right.Dimensions |> Array.map (~-))
    
    static member (*) (left:Quantity, right:float32) = Quantity(left.BaseValue * double right, left.Dimensions)
    static member (/) (left:Quantity, right:float32) = Quantity(left.BaseValue / double right, left.Dimensions)

    static member (*) (left:decimal, right:Quantity) = Quantity(float left * right.BaseValue, right.Dimensions)
    static member (/) (left:decimal, right:Quantity) = Quantity(float left / right.BaseValue, right.Dimensions |> Array.map (~-))
    
    static member (*) (left:Quantity, right:decimal) = Quantity(left.BaseValue * double right, left.Dimensions)
    static member (/) (left:Quantity, right:decimal) = Quantity(left.BaseValue / double right, left.Dimensions)

    static member (*) (left:int, right:Quantity) = Quantity(float left * right.BaseValue, right.Dimensions)
    static member (/) (left:int, right:Quantity) = Quantity(float left / right.BaseValue, right.Dimensions |> Array.map (~-))
    
    static member (*) (left:Quantity, right:int) = Quantity(left.BaseValue * float right, left.Dimensions)
    static member (/) (left:Quantity, right:int) = Quantity(left.BaseValue / float right, left.Dimensions)

    static member (*) (left:int64, right:Quantity) = Quantity(float left * right.BaseValue, right.Dimensions)
    static member (/) (left:int64, right:Quantity) = Quantity(float left / right.BaseValue, right.Dimensions |> Array.map (~-))
    
    static member (*) (left:Quantity, right:int64) = Quantity(left.BaseValue * float right, left.Dimensions)
    static member (/) (left:Quantity, right:int64) = Quantity(left.BaseValue / float right, left.Dimensions)

    static member (*) (left:bigint, right:Quantity) = Quantity(float left * right.BaseValue, right.Dimensions)
    static member (/) (left:bigint, right:Quantity) = Quantity(float left / right.BaseValue, right.Dimensions |> Array.map (~-))
    
    static member (*) (left:Quantity, right:bigint) = Quantity(left.BaseValue * float right, left.Dimensions)
    static member (/) (left:Quantity, right:bigint) = Quantity(left.BaseValue / float right, left.Dimensions)

    static member Pow (mantissa:Quantity, exponent:int) =
        Quantity(Math.Pow(mantissa.BaseValue, float exponent), 
                 mantissa.Dimensions |> Array.map (fun e -> e * sbyte exponent))

    member this.Pow exponent = Quantity.Pow(this, exponent)

    member private this.rootDimensionalExponents nth =
        let rootDimensions = Array.create 8 0y

        (rootDimensions, this.Dimensions)
        ||> Array.iteri2 (fun i _ d -> 
                let (root, remainder) = Math.DivRem(int d, nth)
                if remainder <> 0 then failwithf "[%s] Dimension is not an integral power of %d" (Quantity.dimensionSymbol i) nth
                rootDimensions.[i] <- sbyte root)
 
        rootDimensions

    static member Root (mantissa:Quantity, nth) =
        Quantity (Math.Pow(mantissa.BaseValue, 1.0 / float nth), mantissa.rootDimensionalExponents nth)

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

    member this.Asin () = Quantity(this.transcendental Math.Asin, Quantity.DimensionlessDimension)
    member this.Acos () = Quantity(this.transcendental Math.Acos, Quantity.DimensionlessDimension)
    member this.Atan () = Quantity(this.transcendental Math.Atan, Quantity.DimensionlessDimension)

    static member Atan2 (y:Quantity, x:Quantity) =
        y.ConfirmIsConsistentWith(x)
        Quantity(Math.Atan2(y.BaseValue, x.BaseValue), Quantity.DimensionlessDimension)

    member this.Sign () = Math.Sign this.BaseValue
    member this.Abs () = Quantity(abs this.BaseValue, this.Dimensions)

    static member op_Implicit (ts:TimeSpan) =
        Quantity(ts.TotalSeconds, Quantity.TimeDimension)

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

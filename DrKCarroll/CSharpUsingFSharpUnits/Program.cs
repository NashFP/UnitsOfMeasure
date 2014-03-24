using System;
using System.Collections.Generic;
using Microsoft.FSharp.Collections;
using UnitSystem;
using UnitSystem.Units;

namespace CSharpUsingFSharpUnits
{
    class Program
    {
        static void Main(string[] args)
        {
            var x = 5 * US.Feet;
            var y = 15.5 * SI.Centimeter;

            var length = (x + y).RoundUp(5 * US.Inch);
            var load = 250 * US.Lbf;

            var area = x * y;
            var side = area.Sqrt();

            var P = load;
            var L = length;
            var E = 29e6 * US.Psi;
            var b = 0.5 * US.Inch;
            var h = 2.0 * US.Inch;
            var I = b * h * h * h / 12.0;
            var I2 = b * h.Pow(3) / 12.0;

            var ts = (Quantity) TimeSpan.FromMinutes(5.0);
            var ts2 = new Quantity(TimeSpan.FromMinutes(5.0));
            var r = Quantity.ReturnChecked(P * L.Pow(3) / (3 * E * I), Quantity.Length);


            Console.WriteLine("x: {0}, y: {1}, length: {2}, area: {3}, side: {4}", 
                    x, y, length, area, side);

            var pOverL = Quantity.op_DivideDot(h, b);
            var another_pOverL = h.ConvertTo(b);

            Console.WriteLine("length (in): {0}, length (mm): {1}", length.ConvertTo(US.Inch), length.ConvertTo(SI.Millimeter));

            Quantity.CheckDimensionalConstraints(
                FSharpList<Tuple<Quantity, Quantity>>.Cons(Tuple.Create(length, Fundamental.Length),
                    FSharpList<Tuple<Quantity, Quantity>>.Cons(Tuple.Create(area, Derived.Area),
                        FSharpList<Tuple<Quantity, Quantity>>.Cons(Tuple.Create(load, Derived.Force), 
                            FSharpList<Tuple<Quantity, Quantity> >.Empty))));

            Quantity.CheckDimensionalConstraints(
                SeqModule.ToList(new []
                    {
                        Tuple.Create(length, Fundamental.Length),
                        Tuple.Create(area, Derived.Area),
                        Tuple.Create(load, Derived.Force)
                    }));

            Quantity.CheckDimensionalConstraints(
                new List<Tuple<Quantity, Quantity>>
                    {
                        Tuple.Create(length, Fundamental.Length),
                        Tuple.Create(area, Derived.Area),
                        Tuple.Create(load, Derived.Force)
                    });

            Quantity.CheckDimensionalConstraints(new[] { 
                length, Fundamental.Length, 
                area, Derived.Area, 
                load, Derived.Force });

            Console.ReadKey();
        }
    }
}

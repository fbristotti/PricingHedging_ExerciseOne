using System;
using System.Collections.Generic;

namespace OptionPricing
{
    public class BrownianNumberGenerator : NormalNumberGenerator
    {
        public override double GetNextNumber(int t)
        {
            double r = 0;
            double u = 0;
            do
            {
                u = Rand.Next();
                double v = Rand.Next();
                r = u*u + v*v;
            } while (r == 0 || r > 1);

            return u*Math.Sqrt(-2*Math.Log(r)/r);
        }
    }
}
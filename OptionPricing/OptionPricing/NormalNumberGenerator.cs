using System;

namespace OptionPricing
{
    public class NormalNumberGenerator : UniformNumberGenerator
    {
        private readonly double _mean;
        private readonly double _stdDev;

        public NormalNumberGenerator() : this(0, 1)
        {

        }

        public NormalNumberGenerator(double mean, double stdDev)
        {
            _mean = mean;
            _stdDev = stdDev;
        }

        public override double GetNextNumber(int t)
        {
            var u1 = Rand.NextDouble(); //these are uniform(0,1) random doubles
            var u2 = Rand.NextDouble();
            var randStdNormal = Math.Sqrt(-2.0 * Math.Log(u1)) * Math.Sin(2.0 * Math.PI * u2); //random normal(0,1)
            return _mean + _stdDev * randStdNormal; //random normal(mean,stdDev^2)
        }
    }
}
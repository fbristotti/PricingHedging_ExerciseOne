using System;

namespace OptionPricing
{
    public class UniformNumberGenerator : INumberGenerator
    {
        private Random _rand;

        protected Random Rand => _rand ?? (_rand = new Random());

        public virtual double GetNextNumber(int t)
        {
            return Rand.NextDouble();
        }
    }
}
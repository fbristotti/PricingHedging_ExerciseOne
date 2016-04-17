using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace OptionPricing
{
    class Program
    {
        static void Main(string[] args)
        {
        }
    }

    public class BrownianBridgeNumberGenerator : BrownianNumberGenerator
    {
        public BrownianBridgeNumberGenerator(double mean, double variance) 
            : base(mean, variance)
        {
        }

        public override double GetNextNumber(int t)
        {
            return base.GetNextNumber(t);
        }
    }
}

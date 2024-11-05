using System;

namespace Lab1.LibCSharp
{
    public static class MathUtils
    {
        public static ulong GCD(ulong a, ulong b)
        {
            while (b != 0)
            {
                ulong temp = b;
                b = a % b;
                a = temp;
            }
            return a;
        }

        public static ulong LCM(ulong a, ulong b)
        {
            if (a == 0 || b == 0)
                return 0;

            return (a / GCD(a, b)) * b;
        }
    }
}

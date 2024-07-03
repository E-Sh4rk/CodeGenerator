using System;

namespace FishingStallSearch
{
    class Program
    {

        static uint Next(uint seed) => seed * 0x41C64E6D + 0x00006073;
        static uint Prev(uint seed) => seed * 0xEEB9EB65 + 0x0A3561A1;

        static uint[] APresses = new uint[3] { 1, 3, 6 };
        static uint[] moreAPresses = new uint[3] { 0, 10, 30 };
        static String[] rodStrings = { "Old  ", "Good ", "Super" };


        static uint[] Attempt(bool route119, bool feebas, uint startingSeed, uint rod, uint initialAdvances)
        {
            uint seed = startingSeed;
            for (int i = 0; i < initialAdvances; i++)
            {
                seed = Next(seed);
            }
            uint minRounds = 1 + ((seed >> 16) % APresses[rod]);
            seed = Next(seed); // One call to determine number of dots on first cast
            seed = Next(seed); // call that determines if fish is on line

            uint biteRoll = (seed >> 16) % 100;
            uint biteResult = 2;
            if ((biteRoll & 1) == 0)
            {
                biteResult = 0;  //A fish will be generated regardless of lead
            }
            else if (biteRoll > 14)
            {
                biteResult = 1; //A Suction Cups or Sticky Hold lead is necessary to get a fish
            }
            for (uint doneRounds = 1; doneRounds < minRounds; doneRounds++)
            {
                seed = Next(seed); // More calls to determine dot numbers
            }

            uint advancement = initialAdvances + minRounds;
            if (minRounds == 1)
            {
                seed = Next(seed); // Decide whether or not to make the player reel again
                advancement++;
                if (((seed >> 16) % 100) < moreAPresses[rod])
                {
                    seed = Next(seed);
                }
            }
            seed = Next(seed);
            uint feebasResult = (((seed >> 16) % 100) < 50) ? (uint)1 :(uint)0;
            if (route119 && !feebas)
            {
                advancement++;
            }
            uint[] output = { biteResult, feebasResult, advancement};
            return output;
        }

        static void PrintResult(uint lead, uint advancement, uint seed)
        {
            String leadString = lead == 0 ? "No lead necessary" : "Must have Suction Cup or Sticky Hold lead";
            Console.WriteLine("\t With Lead: {0}", leadString);
            Console.WriteLine("\t Use Seed {0:X} to generate target on advancement {1}", seed,advancement);
        }

        static void Main(string[] args)
        {
            Console.Write("Enter Seed that generates your target H1 frame:");
            uint inputSeed = Convert.ToUInt32(Console.ReadLine(), 16);
            Console.Write("Will you be fishing on route 119? y/n:");
            bool route119 = Console.ReadLine().Equals("y", StringComparison.CurrentCultureIgnoreCase);
            Console.Write("Are you trying to catch a Feebas on a Feebas tile? y/n:");
            bool goingForFeebas = Console.ReadLine().Equals("y", StringComparison.CurrentCultureIgnoreCase);
            //uint inputSeed = 0xb375be25;
            //bool route119 = true;
            //bool goingForFeebas = false;


            uint start = 7;
            uint[] rods = { 0, 1, 2 };
            uint[] maxAdditionalOffsets = { 0, 1, 4 };
            

            uint def = 100;
                                               //Lead, Frame, Seed
            uint[,] bestResults = new uint[,] {{ def, def, def }, { def, def, def },{ def, def, def} };
 

            foreach (uint rod in rods)
            {
                uint offset119 = 0;
                uint startingSeed = Prev(inputSeed);
                for (int i = 1; i < start + 2; i++)
                {
                    startingSeed = Prev(startingSeed);
                }
                if (route119 && !goingForFeebas)
                {
                    startingSeed = Prev(startingSeed);
                    offset119 = 1;
                }
                for (int additionalOffset = 0; additionalOffset <= maxAdditionalOffsets[rod]; additionalOffset++)
                {
                    uint[] result = Attempt(route119, goingForFeebas, startingSeed, rod, start);


                    startingSeed = Prev(startingSeed);
                    if (result[0] == 2)
                    {
                        continue; // No fish can be generated
                    }
                    if (goingForFeebas && result[1] != 1)
                    {
                        continue; // Want a feebas but didn't get one
                    }
                    if (start + 2 + additionalOffset + offset119 != result[2])
                    {
                        continue; // Wrong frame
                    }

                    // Pokemon can be generated with this seed but check if would rather do this one compared to ones we've already found
                    if (bestResults[rod,0] ==0 && result[0] == 1)
                    {
                        continue; // Prefer to not need a specific lead
                    }
                    if (bestResults[rod,1] < result[2])
                    {
                        continue; // Prefer option which involves fewer bite time checks
                    }
                    bestResults[rod, 0] = result[0];
                    bestResults[rod, 1] = result[2];
                    bestResults[rod, 2] = Next(startingSeed);
                }
            }
            

            Console.WriteLine("Best Results for each rod:");
            foreach(uint rod in rods)
            {

                Console.WriteLine("{0} Rod:", rodStrings[rod]);
                if (bestResults[rod,0] == def)
                {
                    Console.WriteLine("\t No matches found.");
                }
                else
                {
                    PrintResult(bestResults[rod, 0], bestResults[rod, 1], bestResults[rod, 2]);
                }
            }
       

        }
    }
}

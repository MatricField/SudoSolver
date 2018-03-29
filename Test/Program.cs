using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using SudoSolver.Core;

namespace Test
{
    class Program
    {
        static void Main(string[] args)
        {
            var sudo = new Sudo<int>(1, 2, 3, 4, 5);
            sudo.InitBoard(5, 5);
            sudo.GroupRows();
            sudo.GroupColumns();

            sudo.Group((0, 0), (1, 0), (2, 0), (0, 1), (1, 1));
            sudo.Group((3, 0), (4, 0), (3, 1), (4, 1), (4, 2));
            sudo.Group((0, 2), (0, 3), (1, 3), (0, 4), (1, 4));
            sudo.Group((2, 4), (3, 3), (3, 4), (4, 3), (3, 4));
            sudo.Group((2, 1), (1, 2), (2, 2), (3, 2), (2, 3));

            sudo[1, 0] = 2;
            sudo[4, 1] = sudo[3, 2] = 2;
            sudo[0, 1] = sudo[1, 4] = 3;
            sudo[1, 2] = sudo[4, 3] = 4;
            sudo.Solve();
            Console.Write(sudo);
        }
    }
}

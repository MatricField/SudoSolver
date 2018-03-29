using System;
using System.Collections.Generic;
using System.Text;

namespace SudoSolver.Core
{
    public class Sudo<Element> where Element : IEquatable<Element>
    {
        private readonly ISet<Element> PossibleElements;

        private Element[,] Board;

        private Dictionary<(int, int), ISet<(int, int)>> Groups;

        private HashSet<(int, int)> InitialValues;

        public Sudo(params Element[] possibleItems)
        {
            PossibleElements = new HashSet<Element>(possibleItems);
            InitialValues = new HashSet<(int, int)>();
            Groups = new Dictionary<(int, int), ISet<(int, int)>>();
        }

        public void ClearGroupping()
        {
            Groups.Clear();
        }

        public void ClearBoard()
        {
            Board = new Element[Board.GetLength(0), Board.GetLength(1)];
            InitialValues.Clear();
        }

        public void InitBoard(int width, int height)
        {
            Board = new Element[width, height];
        }

        public Element this[int i, int j]
        {
            get => Board[i, j];
            set
            {
                Board[i, j] = value;
                if(default(Element)?.Equals(value) ?? false)
                {
                    InitialValues.Remove((i, j));
                }
                else
                {
                    InitialValues.Add((i, j));
                }
            }
        }

        public void Group(params (int, int)[] cells)
        {
            Group((IEnumerable<(int, int)>)cells);
        }

        public void Group(IEnumerable<(int, int)> cells)
        {
            foreach(var idx in cells)
            {
                var othersInGroup = new List<(int, int)>();
                foreach(var other in cells)
                {
                    if(!other.Equals(idx))
                    {
                        othersInGroup.Add(other);
                    }
                }
                if(Groups.ContainsKey(idx))
                {
                    Groups[idx].UnionWith(othersInGroup);
                }
                else
                {
                    Groups[idx] = new HashSet<(int, int)>(cells);
                }
            }
        }

        public void GroupRows()
        {
            for(int x = 0, len0 = Board.GetLength(0); x < len0; ++x)
            {
                var row = new List<(int, int)>();
                for(int y = 0, len1 = Board.GetLength(1); y < len1; ++y)
                {
                    row.Add((x, y));
                }
                Group(row);
            }
        }

        public void GroupColumns()
        {
            for (int y = 0, len1 = Board.GetLength(1); y < len1; ++y)
            {
                var col = new List<(int, int)>();
                for (int x = 0, len0 = Board.GetLength(0); x < len0; ++x)
                {
                    col.Add((x, y));
                }
                Group(col);
            }
        }

        private bool Solve(int x, int y)
        {
            if(x >= Board.GetLength(0))
            {
                return Solve(0, y + 1);
            }

            if(y >= Board.GetLength(1))
            {
                return false;
            }

            if(InitialValues.Contains((x, y)))
            {
                return Solve(x + 1, y);
            }

            foreach(var element in PossibleElements)
            {
                bool duplicate = false;
                foreach (var (x2, y2) in Groups[(x, y)])
                {
                    if(Board[x2, y2].Equals(element))
                    {
                        duplicate = true;
                        break;
                    }
                }
                if(!duplicate)
                {
                    Board[x, y] = element;
                }
                if(Solve(x + 1, y))
                {
                    return true;
                }
            }

            return false;
        }

        public bool Solve()
        {
            return Solve(0, 0);
        }

        public override string ToString()
        {
            var builder = new StringBuilder();
            for (int y = 0, len1 = Board.GetLength(1); y < len1; ++y)
            {
                for (int x = 0, len0 = Board.GetLength(0); x < len0; ++x)
                {
                    builder.AppendFormat("{0, 10}", Board[x, y]);
                }
                builder.AppendLine();
            }
            return builder.ToString();
        }
    }
}

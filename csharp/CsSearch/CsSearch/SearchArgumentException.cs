﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace CsSearch
{
	class SearchArgumentException : Exception
	{
		public SearchArgumentException(string message) : base(message)
		{
		}
	}
}

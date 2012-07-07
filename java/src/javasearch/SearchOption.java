/*******************************************************************************
SearchOption

Class to encapsulate a command line search option

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

public class SearchOption implements ISearchOption {

	private String shortArg;
	private String longArg;
	private String description;

	public SearchOption(String shortArg, String longArg, String description) {
		this.shortArg = shortArg;
		this.longArg = longArg;
		this.description = description;
	}

	public String getShortArg() {
		return this.shortArg;
	}

	public String getLongArg() {
		return this.longArg;
	}

	public String getDescription() {
		return this.description;
	}

	public String getSortArg() {
		if (null != this.shortArg && !this.shortArg.equals(""))
			return this.shortArg;
		else
			return this.longArg;
	}
}

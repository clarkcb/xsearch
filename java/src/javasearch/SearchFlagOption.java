/*******************************************************************************
SearchFlagOption

Class to encapsulate a command line search option that sets a flag

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

public class SearchFlagOption extends SearchOption {

	private SearchFlagSetter searchFlagSetter;

	public SearchFlagOption(String shortArg, String longArg, String description, SearchFlagSetter searchFlagSetter) {
		super(shortArg, longArg, description);
		this.searchFlagSetter = searchFlagSetter;
	}

	public void setFlag(SearchSettings settings) {
		this.searchFlagSetter.setFlag(settings);
	}
}

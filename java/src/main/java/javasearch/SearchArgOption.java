/*******************************************************************************
SearchArgOption

Class to encapsulate a command line search option with an arg

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

public class SearchArgOption extends SearchOption {

	private SearchArgSetter searchArgSetter;

	public SearchArgOption(String shortArg, String longArg, String description, SearchArgSetter searchArgSetter) {
		super(shortArg, longArg, description);
		this.searchArgSetter = searchArgSetter;
	}

	public void setArg(String arg, SearchSettings settings) {
		this.searchArgSetter.setArg(arg, settings);
	}
}

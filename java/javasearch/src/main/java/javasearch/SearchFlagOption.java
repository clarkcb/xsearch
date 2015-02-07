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

    public SearchFlagOption(final String shortArg,
                            final String longArg,
                            final String description,
                            final SearchFlagSetter searchFlagSetter) {
        super(shortArg, longArg, description);
        this.searchFlagSetter = searchFlagSetter;
    }

    public final void setFlag(SearchSettings settings) {
        this.searchFlagSetter.setFlag(settings);
    }
}

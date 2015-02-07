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

    public SearchArgOption(final String shortArg,
                           final String longArg,
                           final String description,
                           final SearchArgSetter searchArgSetter) {
        super(shortArg, longArg, description);
        this.searchArgSetter = searchArgSetter;
    }

    public final void setArg(final String arg, final SearchSettings settings) {
        this.searchArgSetter.setArg(arg, settings);
    }
}

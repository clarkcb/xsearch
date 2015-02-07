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

    public SearchOption(final String shortArg, final String longArg,
                        final String description) {
        this.shortArg = shortArg;
        this.longArg = longArg;
        this.description = description;
    }

    public final String getShortArg() {
        return this.shortArg;
    }

    public final String getLongArg() {
        return this.longArg;
    }

    public final String getDescription() {
        return this.description;
    }

    public final String getSortArg() {
        if (null != this.shortArg && !this.shortArg.equals(""))
            return this.shortArg.toLowerCase() + "a" + this.longArg;
        return this.longArg;
    }
}

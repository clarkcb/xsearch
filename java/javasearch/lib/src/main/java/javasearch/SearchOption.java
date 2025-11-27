/*******************************************************************************
SearchOption

Class to encapsulate a command line search option

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

import javafind.ArgTokenType;
import javafind.Option;

public record SearchOption(String shortArg, String longArg, String description, ArgTokenType argType) implements Option {
    public String getSortArg() {
        if (null != this.shortArg && !this.shortArg.isEmpty()) {
            return this.shortArg.toLowerCase() + "@" + this.longArg;
        }
        return this.longArg;
    }
}

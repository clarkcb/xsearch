/*******************************************************************************
ISearchOption

Search option interface

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

public interface ISearchOption {

    String getShortArg();

    String getLongArg();

    String getDescription();

    String getSortArg();
}
